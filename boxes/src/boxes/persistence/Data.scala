package boxes.persistence

import scala.xml._
import scala.xml.pull._
import scala.io.Source
import collection._


trait DataSource {
//  def get():Int
//  def get(bytes:Array[Byte]):Int
//  def get(bytes:Array[Byte], offset:Int, length:Int):Int

  def getBoolean():Boolean
  def getByte():Byte
  def getShort():Short
  def getChar():Char
  def getInt():Int
  def getLong():Long
  def getFloat():Float
  def getDouble():Double
  def getUTF():String
  def close()

  /**
   * Consume the next open tag, or exception if the
   * next element is not an open tag. Returns the string
   * in the tag
   */
  def getOpenTag():String

  /**
   * Consume the next open class tag, or exception if the
   * next element is not an open class tag. Returns the class
   * in the tag
   */
  def getOpenClassTag():Class[_]

  /**
   * Consume the next open class tag, or exception if the
   * next element is not an open class tag with the specified
   * class
   */
  def getOpenClassTag(c:Class[_])

  /**
   * Get the string from next open tag, or exception if
   * next element is not an open tag
   * Does not consume the tag - leaves it in the source
   */
  def peekOpenTag():String

  /**
   * Get the class from next open class tag, or exception if
   * next element is not an open class tag
   * Does not consume the tag - leaves it in the source
   */
  def peekOpenClassTag():Class[_]

  /**
   * True if next tag is a close tag, false otherwise
   * Does not consume the tag - leaves it in the source
   */
  def peekCloseTag():Boolean

  /**
   * Consume the next close tag, or exception if the
   * next element is not a close tag
   */
  def getCloseTag()
}

/**
 * Output of the most basic data types, whose representation
 * is different between different basic file types. For example,
 * XML will use a standard string representation for Ints, whereas
 * a binary format will
 */
trait DataTarget {
//  def put(i:Int)
//  def put(bytes:Array[Byte])
//  def put(bytes:Array[Byte], offset:Int, length:Int)

  def putBoolean(b:Boolean)
  def putByte(i:Byte)
  def putShort(i:Short)
  def putChar(i:Char)
  def putInt(i:Int)
  def putLong(l:Long)
  def putFloat(f:Float)
  def putDouble(d:Double)
  def putUTF(s:String)
  def openTag(s:String)
  def openClassTag(c:Class[_])
  def closeTag()
  def flush()
  def close()
}

class XMLDataSource(s:Source, aliases:XMLAliases) extends DataSource {

  val events = new XMLEventReader(s)
  var nextEvent:Option[XMLEvent] = None

  private def getNextEvent = {
    val e = nextEvent.getOrElse(events.next)
    nextEvent = None
    e
  }

  private def peekNextEvent = {
    nextEvent match {
      case None => {
        val e = events.next
        nextEvent = Some(e)
        e
      }
      case Some(e) => e
    }
  }

  private def getText() = {
    getNextEvent match {
      case text:EvText => text.text
      case wrong:Any => throw new RuntimeException("Next event is not text, it is " + wrong)
    }
  }

  override def getBoolean() = getText.trim.toBoolean
  override def getByte() = getText.trim.toByte
  override def getShort() = getText.trim.toShort
  override def getChar() = getText.trim.toCharArray.apply(0)
  override def getInt() = getText.trim.toInt
  override def getLong() = getText.trim.toLong
  override def getFloat() = getText.trim.toFloat
  override def getDouble() = getText.trim.toDouble
  override def getUTF():String = getText

  override def close() = s.close

  override def getOpenTag() = {
    getNextEvent match {
      case start:EvElemStart => start.label
      case wrong:Any => throw new RuntimeException("Next tag is not an open tag, it is " + wrong)
    }
  }

  override def getOpenClassTag():Class[_] = {
    aliases.forAlias(getOpenTag)
  }

  override def getOpenClassTag(c:Class[_]):Unit = {
    val tagClass = getOpenClassTag
    if (tagClass != c) throw new RuntimeException("Expected tag for class " + c + " but found " + tagClass)
  }

  override def peekOpenTag():String = {
    peekNextEvent match {
      case start:EvElemStart => start.label
      case wrong:Any => throw new RuntimeException("Next tag is not an open tag, it is " + wrong)
    }
  }

  override def peekOpenClassTag():Class[_] = {
    aliases.forAlias(peekOpenTag)
  }
  override def peekCloseTag():Boolean = {
    peekNextEvent match {
      case end:EvElemEnd => true
      case _ => false
    }
  }

  override def getCloseTag() = {
    getNextEvent match {
      case end:EvElemEnd => {}
      case wrong:Any => throw new RuntimeException("Next tag is not a close tag, it is " + wrong)
    }
  }
}

class XMLAliases {
  private val aliases = mutable.Map[Class[_], String]()
  private val aliasesReverse = mutable.Map[String, Class[_]]()

  def alias(c:Class[_], s:String) = {
    //Note that we enforce that each string is only used for at most one class,
    //BUT we allow each class to map to multiple strings. The last-set alias
    //is the one used for encoding, but any string is valid for decoding. This
    //allows us to support legacy aliases for classes.
    aliasesReverse.get(s) match {
      case None => {}
      case Some(previousClass) => throw new RuntimeException(s + " is already an alias for " + previousClass.getCanonicalName)
    }
    aliases.put(c, s)
    aliasesReverse.put(s, c)
  }

  def forClass(c:Class[_]) = aliases.getOrElse(c, c.getCanonicalName)

  def forAlias(s:String) = {
    aliasesReverse.get(s) match {
      case None => Class.forName(s)
      case Some(c) => c
    }
  }
}

class XMLDataTarget(aliases:XMLAliases) extends DataTarget {

  private val tagStack = mutable.ListBuffer[String]()

  private def printWithTabs(s:String) = {
//    tagStack.foreach(_ => print("  "))
//    println(s)
    print(s)
  }

  def putBoolean(b:Boolean) = printWithTabs(""+b)
  def putByte(i:Byte) = printWithTabs(""+i)
  def putShort(i:Short) = printWithTabs(""+i)
  def putChar(i:Char) = printWithTabs(""+i)
  def putInt(i:Int) = printWithTabs(""+i)
  def putLong(l:Long) = printWithTabs(""+l)
  def putFloat(f:Float) = printWithTabs(""+f)
  def putDouble(d:Double) = printWithTabs(""+d)
  def putUTF(s:String) = printWithTabs(s)

  def openTag(s:String) = {
    printWithTabs("<" + s + ">")
    tagStack.append(s)
  }

  def openClassTag(c:Class[_]) = {
    val s = aliases.forClass(c)
    printWithTabs("<" + s + ">")
    tagStack.append(s)
  }

  def closeTag() = {
    printWithTabs("</" + tagStack.remove(tagStack.size-1) +">")
  }

  def flush() = {}

  def close() = {
    if (!tagStack.isEmpty) throw new RuntimeException("Closed XMLDataTarget with tags still open")
  }
}