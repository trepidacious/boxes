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
import java.io.Reader
import java.io.StringReader

sealed trait Link
case class LinkRef(id: Int) extends Link	//Link is a reference to another obj
case class LinkId(id: Int) extends Link	//Link is an id, to accept references from other objs
case object LinkEmpty extends Link		//There is no link

sealed trait Token

case class OpenObj(clazz: Class[_], link: Link = LinkEmpty) extends Token
case object CloseObj extends Token

sealed trait Prim[P] extends Token {
  def p: P
}

case class BooleanToken(p: Boolean) extends Prim[Boolean] 
case class IntToken(p: Int) extends Prim[Int] 
case class LongToken(p: Long) extends Prim[Long] 
case class FloatToken(p: Float) extends Prim[Float] 
case class DoubleToken(p: Double) extends Prim[Double] 
case class StringToken(p: String) extends Prim[String] 

case class OpenField(name: String) extends Token

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
      }
      
      case BooleanToken(p) 	=> printPrim(p) 
      case IntToken(p)  	=> printPrim(p)
      case LongToken(p)  	=> printPrim(p)
      case FloatToken(p)  	=> printPrim(p)
      case DoubleToken(p)  	=> printPrim(p)
      case StringToken(p)   => printPrim(p)
      
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
  
  private def commaIfNeeded() = {
    if (commaNeeded) {
      print(",")
      println()
    }
  }

  private def printObjPrim[P](p:P)(implicit codec:CodecWithClass[P]) {
    commaIfNeeded()
    //object with _type_ as class alias, _val_ as primitive.
	print("{\"_val_type_\":" + quoted(aliases.forClass(codec.clazz)) + ", \"_val_\":" + p + "}")
  }
  
  private def printPrim[P](p:P) {
    commaIfNeeded()
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
      case OpenObj(clazz, link) => {        
        commaIfNeeded()
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
      
      //First-class primitives - represented directly in JSON
      case BooleanToken(p) 	=> printPrim(p) 
      case IntToken(p)  	=> printPrim(p)
      case DoubleToken(p)  	=> printPrim(p)
      case StringToken(p)   => printPrim(quoted(p))
      
      //Second-class primitives - represented as objects in JSON
      case LongToken(p)  	=> printObjPrim(p)
      case FloatToken(p)  	=> printObjPrim(p)
      
      case OpenField(name) => {
    	  print(",")
    	  println()
    	  print(quoted(name) + ":")
      }
      
      case OpenArr => {
    	commaIfNeeded()
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

class JSONTokenReader(reader: Reader, aliases: ClassAliases) extends TokenReader {
  private val parser = JsonParser(reader)
  
  private var nextToken: Option[Token] = None
  
  def peek = {
    nextToken.getOrElse{
      val t = pullToken()
      nextToken = Some(t)
      t
    }
  }
  def pull() = {
    val t = peek
    if (t != End) nextToken = None
    t
  }
  
  def assert(t:Token) {
    val p = pull()
    if (p != t) throw new RuntimeException("Expected " + t + ", got " + pull)
  }
  
  private def pullClassField() = {
    val classToken = parser.pull
	classToken match {
	  case JsonParser.StringVal(alias) => aliases.forAlias(alias)
	  case _ => throw new RuntimeException("Unexpected type field value, not a string: " + classToken)
	} 
  }

  private def pullInt() = {
    val token = parser.pull
	token match {
	  case JsonParser.IntVal(i) => i
	  case _ => throw new RuntimeException("Unexpected int field value, not an int: " + token)
	} 
  }

  private def pullDouble() = {
    val token = parser.pull
	token match {
	  case JsonParser.DoubleVal(i) => i
	  case _ => throw new RuntimeException("Unexpected double field value, not a double: " + token)
	} 
  }

  private def pullLink() = {
    val l = parser.peek
    l match {
      case JsonParser.FieldStart("_id_") => {
        parser.pull
        LinkId(pullInt().intValue)
      }
      case JsonParser.FieldStart("_ref_") => {
        parser.pull
        LinkRef(pullInt().intValue)
      }
      case _ => LinkEmpty
    } 
  }
  
  private def pullSecondClassPrim[P](clazz: Class[P]) = {
    val valToken = parser.pull
    if (valToken != JsonParser.FieldStart("_val_")) throw new RuntimeException("Unexpected val token for second class primitive: " + valToken)
    
    val t = if (clazz == LongCodec.clazz) LongToken(pullInt().longValue)
    else if (clazz == FloatCodec.clazz) FloatToken(pullDouble().floatValue)
    else throw new RuntimeException("Unexpected second class primitive clazz: " + clazz)
    
    val closeToken = parser.pull
    if (closeToken != JsonParser.CloseObj) throw new RuntimeException("Unexpected close token for second class primitive: " + closeToken)

    t
  }

  private def pullToken(): Token = {
    parser.pull match {
        case JsonParser.OpenObj => {
          val t = parser.pull
          t match {
            case JsonParser.FieldStart("_type_") => {
              val clazz = pullClassField()
              val link = pullLink()
              OpenObj(clazz, link)
            }
            case JsonParser.FieldStart("_val_type_") => {
              val clazz = pullClassField()
              pullSecondClassPrim(clazz)
            }
            case _ => throw new RuntimeException("Unexpected first token in Obj, not a type or valType: " + t)
          }
        }
        case JsonParser.CloseObj => CloseObj
        case JsonParser.FieldStart(name) => OpenField(name)
        case JsonParser.End => End
        case JsonParser.StringVal(value) => StringToken(value)
        case JsonParser.IntVal(value) => IntToken(value.intValue)
        case JsonParser.DoubleVal(value) => DoubleToken(value)
        case JsonParser.BoolVal(value) => BooleanToken(value)
        case JsonParser.NullVal => throw new RuntimeException("Unexpected null token")
        case JsonParser.OpenArr => OpenArr
        case JsonParser.CloseArr => CloseArr
    }
  }
  
}


class Thingy {}

object Tokens {
  def main(args: Array[String]) {
    
    val stream = List(
      OpenObj(classOf[Thingy], LinkId(0)),
      OpenField("fieldName"),
      IntToken(42),
      OpenField("longField"),
      LongToken(42),
      OpenField("listFieldName"),
      OpenArr,
      IntToken(42),
      BooleanToken(false),
      FloatToken(42),
      DoubleToken(42),
      IntToken(43),
      IntToken(44),
      OpenObj(classOf[Thingy], LinkId(1)),
      OpenField("fieldName"),
      IntToken(24),
      CloseObj,
      CloseArr,
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
        
//    val parser = JsonParser(json.toString)
//    
//    @tailrec
//	def printNext(): Unit = {
//	  val t = parser.nextToken
//	  println(t)
//	  if (t != JsonParser.End) printNext()
//	}
//	
//    printNext()
    
    val reader = new JSONTokenReader(new StringReader(json.toString), aliases)
    
    @tailrec
	def printNext(): Unit = {
	  val t = reader.pull
	  println(t)
	  if (t != End) printNext()
	}
	
    printNext()

  }
}
