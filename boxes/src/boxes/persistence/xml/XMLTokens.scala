package boxes.persistence.xml

import java.io.Writer
import scala.collection.mutable.Stack
import boxes.persistence.ValCodecs._
import java.io.StringWriter
import boxes.Var
import boxes.list.ListVar
import scala.annotation.tailrec
import java.io.Reader
import java.io.StringReader
import boxes.persistence._
import scala.xml.pull._
import scala.io.Source
import collection._
import scala.xml.Node

object XMLTokenWriter {
  class Person extends boxes.Node {
    val name = Var("name")
    val age = Var(32)
    val friend:Var[Option[Person]] = Var(None)
    val spouse:Var[Option[Person]] = Var(None)
    val numbers = Var(List[Int]())
    val accounts = Var(Map[String, Double]())
    val nicknames = ListVar[String]()

    override def toString = name() + ", " + age() + ", friend: " + friend() + ", spouse " + spouse() + ", numbers " + numbers() + ", accounts " + accounts() + ", nicknames " + nicknames()
  }
  
  def main(args: Array[String]) {
    val p = new Person()
    p.accounts() = Map("current" -> 10.0, "savings" -> 100.0, "secretswiss" -> 10000000.0)
    p.numbers() = List(10,20,30)
    p.age() = 100
    p.nicknames() = List("Pico", "Pi")

    val q = new Person()
    q.accounts() = Map("current" -> 0.0)
    q.numbers() = List(1, 4, 9)
    q.name() = "q"
    q.nicknames() = List("Queue", "Cue", "QED")

    p.friend() = Some(q)
    p.spouse() = Some(q)

    val codec = new CodecByClass()

    val aliases = new ClassAliases
    aliases.alias(classOf[Person], "Person")

    val writer = new StringWriter()
    val target = new XMLTokenWriter(writer, aliases)

    codec.write(p, target)

    val xml = writer.toString
    println(xml)
    
    var reader = new XMLTokenReader(Source.fromString(xml), aliases)
    val rp = codec.read(reader).asInstanceOf[Person]
    println(p)
    println(rp)
    
//    val reader = new XMLTokenReader(Source.fromString(xml), aliases)
//    
//    @tailrec
//    def printNext(): Unit = {
//      val t = reader.pull
//      println(t)
//      if (t != End) printNext()
//    }
//  
//    printNext()

    
  }
}

//Simplify an XMLEventReader for our use
private class XMLEvents(s: Source) {
  private val events = new XMLEventReader(s)
  private var nextEvent:Option[XMLEvent] = None
  private var expectingText = false;
  
  private def pullEvent() = {
    //Text handling is a little awkward. The format never uses text
    //except for primitives, so we need an expectingText state to
    //track this. We tolerate whitespace anywhere, and ignore it,
    //but any text element that is not expected causes an exception.
    events.find(event => {
      event match {
        case text:EvText => {
          if (expectingText) {
            true
          } else {
            if (!text.text.trim.isEmpty) throw new RuntimeException("Unexpected text '" + text.text + "'")
            false
          }
        }
        case _ => true
      }
    }) match {
      case None => throw new RuntimeException("No more events")
      case Some(start:EvElemStart) if ClassAliases.primitiveAliases.contains(start.label) => {
        expectingText = true
        start
      }
      case Some(event) => {
        expectingText = false
        event
      }
    }
  }

  def peek: XMLEvent = {
    nextEvent.getOrElse{
      val t = pullEvent
      nextEvent = Some(t)
      t
    }
  }
  def pull: XMLEvent = {
    val t = peek
    nextEvent = None
    t
  }

}

class XMLTokenReader(s:Source, aliases:ClassAliases) extends TokenReader {
  
  private val events = new XMLEvents(s)
  
  private var nextToken: Option[Token] = None
  
  private val tokens = Stack[Token]()
  
  private def pullSomeToken(): Token = pullToken().getOrElse(pullSomeToken())
  
  def peek = {
    nextToken.getOrElse{
      val t = pullSomeToken()
      nextToken = Some(t)
      t
    }
  }
    
  def pull() = {
    val t = peek
    if (t != End) nextToken = None
    t
  }  
  
  def intAttr(e:EvElemStart, name:String) = {
    e.attrs(name) match {
      case null => None
      case id:Seq[Node] => Some(id(0).text.toInt)
    }
  }

  private def pullLink(start: EvElemStart) = {
    val idAttr = intAttr(start, "id")
    val refAttr = intAttr(start, "ref")
    idAttr match {
      case Some(id) => refAttr match {
        case Some(_) => throw new RuntimeException("Both id and ref were specified - invalid XML")
        case None => LinkId(id)
      }
      case None => refAttr match {
        case Some(ref) => LinkRef(ref)
        case None => LinkEmpty
      }
    }
  }

  private def pullText() = {
    events.peek match {
      case text:EvText => {
        events.pull
        text.text
      }
      case end:EvElemEnd => ""
      case wrong:Any => throw new RuntimeException("Next event is not text, it is " + wrong)
    }
  }

  private def pullPrimitive[P](c: (String)=>P)(implicit codec:CodecWithClass[P]):P = {
    val t = c(pullText)
    events.pull match {
      case end: EvElemEnd => {}
      case wrong => throw new RuntimeException("Expected end element event, got " + wrong) 
    }
    t
  }

  private var expectingField = false
  
  private def pullToken(): Option[Token] = {
    
    def push(t: Token) = {
      tokens.push(t)
      Some(t)
    }
    
    events.pull match {
      case start:EvElemStart => {
        start.label match {
          
          //When we see a primitive open, we immediately get the content
          //and the close tag - essentially primitives are "atomic"
          case "String" => Some(StringToken(pullPrimitive((s)=>s)))
          case "Int" => Some(IntToken(pullPrimitive(_.trim.toInt)))
          case "Long" => Some(LongToken(pullPrimitive(_.trim.toLong)))
          case "Double" => Some(DoubleToken(pullPrimitive(_.trim.toDouble)))
          case "Boolean" => Some(BooleanToken(pullPrimitive(_.trim.toBoolean)))
          case "Float" => Some(FloatToken(pullPrimitive(_.trim.toFloat)))

          //Always just opens an array
          case "List" => push(OpenArr)
          
          //Might open a class, or a field.
          //Opens a field if we are in an Obj (and hence not an Arr, or in a field already)
          case label => {
            tokens.headOption match {
              case Some(OpenObj(_,_)) => {
                val link = pullLink(start)
                if (link != LinkEmpty) throw new RuntimeException("Unexpected link in field tag " + link)
                push(OpenField(label))       
              }
              case _ => {
                val clazz = aliases.forAlias(label)
                val link = pullLink(start)
                push(OpenObj(clazz, link))                                              
              }
            }
          }
        }
      }
      case end:EvElemEnd => {
        tokens.pop() match {
          case OpenArr => {
            if (end.label != "List") throw new RuntimeException("Expecting to close List, but got '" + end.label + "'")
            Some(CloseArr)
          }
          case OpenField(name) => {
            if (end.label != name) throw new RuntimeException("Expecting to close field '" + name + "', but got '" + end.label + "'")
            //No event in this case...
            None
          }
          case OpenObj(clazz, _) => {
            val expectedAlias = aliases.forClass(clazz)
            if (end.label != expectedAlias) throw new RuntimeException("Expecting to close object with alias '" + expectedAlias + "', but got '" + end.label + "'")
            Some(CloseObj)
          }
          case _ => throw new RuntimeException("Invalid item in tokens") 
        }
      }
    }
    

  }
  
  override def close() {
    super.close()
    s.close()
  }
}

class XMLTokenWriter(writer:Writer, aliases:ClassAliases) extends TokenWriter {

  private val tokens = Stack[Token]()

  private def closeFieldIfNeeded() {
    tokens.headOption.foreach{
    case OpenField(n) => {
      tokens.pop()
      print("</" + n + ">")
    }
    case _ => {}
    }
  }

  //TODO Use a separate implicit that gets us the correct class to look up alias
  //We should enforce that ClassAliases cannot have these basic aliases reassigned
  private def printPrim[P](p:P)(implicit codec:CodecWithClass[P]) {
    print("<" + aliases.forClass(codec.clazz) + ">")
    print("" + p)
    print("</" + aliases.forClass(codec.clazz) + ">")
    closeFieldIfNeeded()
  }

  private def print(s:String) = {
    writer.write(s)
  }

  def write(t: Token) {
    t match {
      case OpenObj(clazz, link) => {        
        val s = aliases.forClass(clazz);
        val linkS = link match {
        case LinkRef(id) => 	" ref='" + id + "'"
        case LinkId(id) => 	" id='" + id + "'"
        case _ => 				""
        }
        print("<" + s + linkS + ">")
        tokens.push(t)
      }
      case CloseObj => {
        tokens.pop() match {
        case OpenObj(clazz, link) => print("</" + aliases.forClass(clazz) + ">")
        case _ => throw new RuntimeException("Mismatched CloseObj token")
        }        
        closeFieldIfNeeded()
      }
  
      case BooleanToken(p) 	=> printPrim(p) 
      case IntToken(p)  	  => printPrim(p)
      case LongToken(p)  	  => printPrim(p)
      case FloatToken(p)  	=> printPrim(p)
      case DoubleToken(p)  	=> printPrim(p)
      case StringToken(p)   => printPrim(scala.xml.Utility.escape(p))
  
      case OpenField(name) => {
        print("<" + name + ">")
        tokens.push(t)
      }
  
      case OpenArr => {
        print("<List>")
        tokens.push(t)
      }
      case CloseArr => {
        tokens.pop() match {
        case OpenArr => print("</List>")
        case _ => throw new RuntimeException("Mismatched CloseArr token")
        }
        closeFieldIfNeeded()
      }
  
      case End => {
        close()
        tokens.push(t)
      }      
    }

  }

  override def close() {
    super.close()
    writer.flush
    writer.close
  }

}
