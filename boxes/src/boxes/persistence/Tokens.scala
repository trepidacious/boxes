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
import boxes.persistence.json.JSONTokenReader
import boxes.persistence.json.JSONTokenWriter

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


//Result of checking for an object in the cache
sealed abstract class CacheResult
//Object is already cached, use a ref as given
case class Cached(ref:Int) extends CacheResult
//Object was not cached, use an id as given
case class New(id:Int) extends CacheResult

//TODO add validation of token sequence - there are rules for which
//tokens can follow each other token, which could be enforced. 
//This could take form of a wrapper a TokenWriter or TokenReader, 
//performing validation as tokens are passed through to/from delegate. 

trait TokenWriter {
  def write(t: Token)
  
  private val c = collection.mutable.Map[Any, Int]()
  private var nextId = 0
  
  /**
   * Try to cache a thing. The result will tell us whether the thing
   * is already cached:
   * 
   *   If already cached, the CacheResult is Cached(ref), where the
   *   supplied ref can be written out in place of the object. This
   *   refers back to the previous instance with the matching id.
   *  
   *   If NOT already cached, the CacheResult is New(id), where the
   *   id should be written out with the object, so that it can be
   *   referenced by future refs.
   */
  def cache(thing:Any):CacheResult = {
    c.get(thing) match {
      case None => {
        val id = nextId
        nextId = nextId + 1
        c.put(thing, id)
        New(id)
      }
      case Some(ref) => Cached(ref)
    }
  }
  
  def close() {
    c.clear()
  }
}

trait TokenReader {
  def peek: Token
  def pull(): Token
  
  def pullAndAssert(t:Token) {
    val p = pull()
    if (p != t) throw new RuntimeException("Expected " + t + ", got " + pull)
  }
  
  private val c = collection.mutable.Map[Int, Any]()

  /**
   * Store something in the cache.
   * Must be called by any Codec that has
   * just decoded something that had an
   * id. This makes the decoded thing
   * available for following codecs to
   * retrieve if they encounter a ref.
   */
  def cache(id:Int, thing:Any) = {
    c.put(id, thing).foreach(existing => {
      throw new RuntimeException("Tried to cache " + thing + " with id " + id + " but this was already used for " + existing)
    })
  }

  /**
   * Retrieve a thing cached by a previous
   * codec with a given id. The ref parameter
   * is the id of the object, and is presumably
   * present in the decoded data from the source.
   */
  def retrieveCached(ref:Int):Any = {
    c.get(ref) match {
      case None => throw new RuntimeException("Nothing found in cache for id (ref) " + ref)
      case Some(thing) => thing
    }
  }
  
  def close() {
    c.clear()
  }

}


class Thingy {}

class NodeWithBlah extends Node {
  val name = Var("Default Name")
  val optionalBlah: Var[Option[Blah]] = Var(None)
  override def toString() = "name()=\"" + name() + "\"; optionalBlah()=" + optionalBlah()
}
case class Blah(s:String = "sVal", i:Int = 42, l: List[Double] = List(1.0, 2.2, 3.9))

object Tokens {
  def main(args: Array[String]) {
    
    class Person extends Node {
      val name = Var("name")
      val age = Var(32)
      val friend:Var[Option[Person]] = Var(None)
      val spouse:Var[Option[Person]] = Var(None)
      val numbers = Var(List[Int]())
      val accounts = Var(Map[String, Double]())
      val nicknames = ListVar[String]()

      override def toString = name() + ", " + age() + ", friend: " + friend() + ", spouse " + spouse() + ", numbers " + numbers() + ", accounts " + accounts() + ", nicknames " + nicknames()
    }
    
//    val p = new Person()
//    p.accounts() = Map("current" -> 10.0, "savings" -> 100.0, "secretswiss" -> 10000000.0)
//    p.numbers() = List(10,20,30)
//    p.age() = 100
//    p.nicknames() = List("Pico", "Pi")
//
//    val q = new Person()
//    q.accounts() = Map("current" -> 0.0)
//    q.numbers() = List(1, 4, 9)
//    q.name() = "q"
//    q.nicknames() = List("Queue", "Cue", "QED")
//
//    p.friend() = Some(q)
//    p.spouse() = Some(q)
//    
    val aliases = new ClassAliases
    aliases.alias(classOf[Person], "Person")
    aliases.alias(classOf[NodeWithBlah], "NodeWithBlah")
    aliases.alias(classOf[Blah], "Blah")
     
    val json = new StringWriter
    val jsonWriter = new JSONTokenWriter(json, aliases, true)

    val codec = new CodecByClass()
    
    val nb = new NodeWithBlah()
    nb.optionalBlah() = Some(Blah())
    
    codec.write(nb, jsonWriter)
    println(json.toString)    

    val jsonReader = new JSONTokenReader(new StringReader(json.toString), aliases)
    println(codec.read(jsonReader))

//    codec.write(p, jsonWriter)
//    println(json.toString)
//
//    val jsonReader = new JSONTokenReader(new StringReader(json.toString), aliases)
//    println(codec.read(jsonReader))
    
//
//
//    stream.foreach(jsonWriter.write(_))
//    println(json.toString)
//
//    val stream = List(
//      OpenObj(classOf[Thingy], LinkId(0)),
//      OpenField("fieldName"),
//      IntToken(42),
//      OpenField("longField"),
//      LongToken(42),
//      OpenField("listFieldName"),
//      OpenArr,
//      IntToken(42),
//      BooleanToken(false),
//      FloatToken(42),
//      DoubleToken(42),
//      IntToken(43),
//      IntToken(44),
//      OpenObj(classOf[Thingy], LinkId(1)),
//      OpenField("fieldName"),
//      IntToken(24),
//      CloseObj,
//      CloseArr,
//      CloseObj,
//      End
//    )
//    
//    val aliases = new ClassAliases
//    aliases.alias(classOf[Thingy], "Thingy")
// 
////    val xml = new StringWriter
////    val xmlWriter = new XMLTokenWriter(xml, aliases)
////    stream.foreach(xmlWriter.write(_))
////    println(xml.toString)
//    
//    val json = new StringWriter
//    val jsonWriter = new JSONTokenWriter(json, aliases, true)
//    stream.foreach(jsonWriter.write(_))
//    println(json.toString)
//        
////    val parser = JsonParser(json.toString)
////    
////    @tailrec
////	def printNext(): Unit = {
////	  val t = parser.nextToken
////	  println(t)
////	  if (t != JsonParser.End) printNext()
////	}
////	
////    printNext()
    
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
