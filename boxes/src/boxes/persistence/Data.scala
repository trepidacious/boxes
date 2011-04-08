package boxes.persistence

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

class XMLDataTarget extends DataTarget {

  private val tagStack = mutable.ListBuffer[String]()
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

  private def printWithTabs(s:String) = {
    tagStack.foreach(_ => print("  "))
    println(s)
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
    val s = aliases.getOrElse(c, c.getCanonicalName)
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