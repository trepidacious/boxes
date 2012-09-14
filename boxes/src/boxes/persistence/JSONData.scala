package boxes.persistence

import boxes.persistence.ValCodecs._
import java.io.Writer
import scala.collection._
import java.io.OutputStreamWriter
import boxes.Var
import boxes.list.ListVar
import boxes.Node
import java.io.StringWriter
import boxes.persistence.json.JsonParser._
import boxes.persistence.json.JsonParser
import scala.annotation.tailrec

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

object JSONData {
  def main(args: Array[String]) {
    
    val p = new Person
    p.nicknames.insert(0, "Mr. Person")
    
    val encode = new CodecByClass()

    val encodeAliases = new ClassAliases
    encodeAliases.alias(classOf[Person], "Person")

    val json = new StringWriter
    val jsonTarget = new JSONDataTarget(json, encodeAliases)
    encode.code(p, jsonTarget)
    println(json.toString)
    
    val xml = new StringWriter
    val xmlTarget = new XMLDataTarget(xml, new ClassAliases)
    encode.code(p, xmlTarget)
    println(xml.toString)

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

object JSONDataTarget {
  val dq = "\""
}

class JSONDataTarget(writer:Writer, aliases:ClassAliases) extends DataTarget {

  sealed trait TagType
  case object ClassTagType extends TagType
  case object PlainEmptyTagType extends TagType
  case object PlainFullTagType extends TagType
  
  private val tagStack = mutable.ListBuffer[TagType]()

  private def pushTag(t: TagType) = tagStack.append(t)
  private def popTag() = tagStack.remove(tagStack.size-1)
  private def peekTag() = if (tagStack.isEmpty) None else Some(tagStack(tagStack.size-1))
  private def fillTag(checkState: Boolean) = {
//    if (tagStack.isEmpty) {
//      if (checkState) {
//        throw new RuntimeException("Tried to fill when no tags open")
//      } 
//    } else {
//      val t = tagStack(tagStack.size - 1)
//      if (t == ClassTagType) throw new RuntimeException("Tried to fill a class tag")
//      tagStack(tagStack.size - 1) = PlainFullTagType
//    }
  }
  
  private val cache = mutable.Map[Any, Int]()
  private var nextId = 0

  override def cache(thing:Any) = {
    cache.get(thing) match {
      case None => {
        val id = nextId
        nextId = nextId + 1
        cache.put(thing, id)
        New(id)
      }
      case Some(ref) => Cached(ref)
    }
  }

  private def print(s:String) = {
    writer.write(s)
  }
  private def println(s:String) = {
    writer.write(s)
    writer.write("\n")
    Range(0, tagStack.count(_==ClassTagType)).foreach{i=>writer.write("  ")}
  }

  private def quoted(s: String) = JSONDataTarget.dq + s + JSONDataTarget.dq;
  
  private def putPrimitive[P](p:P)(implicit codec:CodecWithClass[P]) {
    print("" + p)
    fillTag(true)
  }
  
  def putBoolean(b:Boolean) =   putPrimitive(b)
  def putByte(i:Byte) =         putPrimitive(i)
  def putShort(i:Short) =       putPrimitive(i)
  def putChar(i:Char) =         putPrimitive(i)
  def putInt(i:Int) =           putPrimitive(i)
  def putLong(l:Long) =         putPrimitive(l)
  def putFloat(f:Float) =       putPrimitive(f)
  def putDouble(d:Double) =     putPrimitive(d)
  //TODO need to escape string properly
  def putUTF(s:String) =        putPrimitive(quoted(s))

  def openTag(s:String) = {
    println(",")
    print(quoted(s) + ":")
    pushTag(PlainEmptyTagType)
  }
  def openClassTag(c:Class[_], id:Option[Int]=None, ref:Option[Int]=None) {
    val label = aliases.forClass(c)
    fillTag(false)
    pushTag(ClassTagType)
    println("{")
    print("\"_type_\":" + quoted(label))
    id.foreach(i => {
    	println(",") 
    	print("\"_id_\":" + i) 
    })
    ref.foreach(i => {
    	println(",") 
    	print("\"_ref_\":" + i) 
    })
  }
  
  def closeTag() = {
    val t = popTag
    t match {
      case ClassTagType => {
        println("")
        print("}") 
      }
      case PlainEmptyTagType => print("1") //Tags in JSON always need a value, so just use 1
      case PlainFullTagType => 
    }
  }
  
  def flush() = {
    writer.flush
  }

  def close() = {
    writer.close
    cache.clear
//    if (!tagStack.isEmpty) throw new RuntimeException("Closed XMLDataTarget with tags still open")
  }
    
}