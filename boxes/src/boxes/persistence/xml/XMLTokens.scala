package boxes.persistence.xml

import java.io.Writer
import scala.collection.mutable.Stack
import boxes.persistence.ValCodecs._
import java.io.StringWriter
import boxes.Var
import boxes.list.ListVar
import boxes.Node
import scala.annotation.tailrec
import java.io.Reader
import java.io.StringReader
import boxes.persistence._

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
        close()
        tokens.push(t)
      }      
    }
    
  }
  
  def close() {
    writer.flush
    writer.close
  }
  
}
