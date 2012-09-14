package boxes.persistence

import java.io.Writer
import scala.collection.mutable.Stack
import boxes.persistence.ValCodecs._
import java.io.StringWriter
import boxes.Var
import boxes.list.ListVar
import boxes.Node
import boxes.persistence.json.JsonParser._
import boxes.persistence.json.JsonParser
import scala.annotation.tailrec

sealed trait Link
case class LinkRef(id: Int) extends Link	//Link is a reference to another obj
case class LinkId(id: Int) extends Link	//Link is an id, to accept references from other objs
case object LinkEmpty extends Link		//There is no link

sealed trait Token

case class OpenObj(link: Link, clazz: Class[_]) extends Token
case object CloseObj extends Token

sealed trait Prim[P] extends Token {
  def p: P
}

case class BooleanToken(p: Boolean) extends Prim[Boolean] 
case class ByteToken(p: Byte) extends Prim[Byte] 
case class ShortToken(p: Short) extends Prim[Short] 
case class CharToken(p: Char) extends Prim[Char] 
case class IntToken(p: Int) extends Prim[Int] 
case class LongToken(p: Long) extends Prim[Long] 
case class FloatToken(p: Float) extends Prim[Float] 
case class DoubleToken(p: Double) extends Prim[Double] 
case class StringToken(p: String) extends Prim[String] 

case class OpenField(name: String) extends Token
case object CloseField extends Token

case object OpenArr extends Token
case object CloseArr extends Token

case object End extends Token

//TODO add validation of token sequence - there are rules for which
//tokens can follow each other token, which could be enforced. 
//This could take form of a wrapper a TokenWriter or TokenReader, 
//performing validation as tokens are passed through to/from delegate. 

trait TokenWriter {
  def write(t: Token)
}

trait TokenReader {
  def peek: Token
  def pull(): Token
  def assert(t:Token)
}

class XMLTokenWriter(writer:Writer, aliases:ClassAliases) extends TokenWriter {
  
  private val tokens = Stack[Token]()

  //TODO Use a separate implicit that gets us the correct class to look up alias
  //We should enforce that ClassAliases cannot have these basic aliases reassigned
  private def printPrim[P](p:P)(implicit codec:CodecWithClass[P]) {
	print("<" + aliases.forClass(codec.clazz) + ">")
	print("" + p)
    print("</" + aliases.forClass(codec.clazz) + ">")
  }
  
  private def print(s:String) = {
    writer.write(s)
  }
  
  def write(t: Token) {
    t match {
      case OpenObj(link, clazz) => {        
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
          case OpenObj(link, clazz) => print("</" + aliases.forClass(clazz) + ">")
          case _ => throw new RuntimeException("Mismatched CloseObj token")
        }        
      }
      
      case BooleanToken(p) 	=> printPrim(p) 
      case ByteToken(p)  	=> printPrim(p)
      case ShortToken(p)  	=> printPrim(p)
      case CharToken(p)  	=> printPrim(p)
      case IntToken(p)  	=> printPrim(p)
      case LongToken(p)  	=> printPrim(p)
      case FloatToken(p)  	=> printPrim(p)
      case DoubleToken(p)  	=> printPrim(p)
      case StringToken(p)   => printPrim(p)
      
      case OpenField(name) => {
    	  print("<" + name + ">")
    	  tokens.push(t)
      }
      case CloseField => {
        tokens.pop() match {
          case OpenField(name) => print("</" + name + ">")
          case _ => throw new RuntimeException("Mismatched CloseField token")
        }
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
      }
      
      case End => {
        writer.flush
        writer.close
        tokens.push(t)
      }      
    }
    
  }
}

class JSONTokenWriter(writer:Writer, aliases:ClassAliases, pretty: Boolean = false) extends TokenWriter {
  
  private val tokens = Stack[Token]()
  private var previousToken:Option[Token] = None

  //We need to output a comma before a Prim, OpenObj or OpenArr, IFF we
  //have a list open, and our previous token is not the actual OpenArr (which
  //means we are the first entry in the Arr).
  private def commaNeeded = {
    tokens.headOption match {
      case Some(OpenArr) => previousToken != Some(OpenArr) 
      case _ => false
    }
  }
  
  private def printPrim[P](p:P) {
    if (commaNeeded) {
      print(",")
      println()
    }
	print("" + p)
  }

  //TODO perform escaping
  private def quoted(s: String) = "\"" + s + "\"";

  private def print(s:String) = {
    writer.write(s)
  }
  private def println() {
    if (pretty) {
	  writer.write("\n")
	  Range(0, tokens.count(t => t match {
	    case OpenObj(_,_) => true
	    case OpenArr => true
	    case _ => false
	  })).foreach{i=>writer.write("  ")}
    }
  }
  
  def write(t: Token) {
    t match {
      case OpenObj(link, clazz) => {        
        if (commaNeeded) {
          print(",")
          println()
        }
        print("{")
        tokens.push(t)
        val s = "\"_type_\":" + quoted(aliases.forClass(clazz))
        val linkS = link match {
          case LinkRef(id) => 	", \"_ref_\":" + id 
          case LinkId(id) => 	", \"_id_\":" + id
          case _ => 			""
        }
        print(s + linkS)
      }
      case CloseObj => {
        tokens.pop() match {
          case OpenObj(link, clazz) => {
        	  println()
        	  print("}")
          }
          case _ => throw new RuntimeException("Mismatched CloseObj token")
        }        
      }
      
      case BooleanToken(p) 	=> printPrim(p) 
      case ByteToken(p)  	=> printPrim(p)
      case ShortToken(p)  	=> printPrim(p)
      case CharToken(p)  	=> printPrim(p)
      case IntToken(p)  	=> printPrim(p)
      case LongToken(p)  	=> printPrim(p)
      case FloatToken(p)  	=> printPrim(p)
      case DoubleToken(p)  	=> printPrim(p)
      case StringToken(p)   => printPrim(quoted(p))
      
      case OpenField(name) => {
    	  print(",")
    	  println()
    	  print(quoted(name) + ":")
    	  tokens.push(t)
      }
      case CloseField => {
        tokens.pop() match {
          case OpenField(name) => {}
          case _ => throw new RuntimeException("Mismatched CloseField token")
        }
      }
      
      case OpenArr => {
        if (commaNeeded) {
          print(",")
          println()
        }
    	print("[")
    	tokens.push(t)
    	println()
      }
      case CloseArr => {
        tokens.pop() match {
          case OpenArr => {
        	println()
            print("]")
          }
          case _ => throw new RuntimeException("Mismatched CloseArr token")
        }
      }
      
      case End => {
        writer.flush
        writer.close
        tokens.push(t)
      }      
    }
    previousToken = Some(t)
  }
}

class Thingy {}

object XMLTokenWriter {
  def main(args: Array[String]) {
    
    val stream = List(
      OpenObj(LinkId(0), classOf[Thingy]),
      OpenField("fieldName"),
      IntToken(42),
      CloseField,
      OpenField("listFieldName"),
      OpenArr,
      IntToken(42),
      IntToken(43),
      IntToken(44),
      OpenObj(LinkId(1), classOf[Thingy]),
      OpenField("fieldName"),
      IntToken(24),
      CloseField,
      CloseObj,
      CloseArr,
      CloseField,
      CloseObj,
      End
    )
    
    val aliases = new ClassAliases
    aliases.alias(classOf[Thingy], "Thingy")
 
//    val xml = new StringWriter
//    val xmlWriter = new XMLTokenWriter(xml, aliases)
//    stream.foreach(xmlWriter.write(_))
//    println(xml.toString)
    
    val json = new StringWriter
    val jsonWriter = new JSONTokenWriter(json, aliases, true)
    stream.foreach(jsonWriter.write(_))
    println(json.toString)
    
//    new Parser(new Buffer(new StringReader(json.toString)))
    
    val parser = JsonParser(json.toString)
    
    @tailrec
	def printNext(): Unit = {
	  val t = parser.nextToken
	  println(t)
	  if (t != JsonParser.End) printNext()
	}
	
    printNext()

  }
}
