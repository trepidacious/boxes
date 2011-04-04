package boxes.codec

import collection._

trait DataSource {

//  def get():Int
//
//  def get(bytes:Array[Byte]):Int
//
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

  //FIXME add support for reading an entire tag as an object

}

/**
 * Output of the most basic data types, whose representation
 * is different between different basic file types. For example,
 * XML will use a standard string representation for Ints, whereas
 * a binary format will
 */
trait DataTarget {

//  def put(i:Int)
//
//  def put(bytes:Array[Byte])
//
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

  def closeTag()

  def flush()

  def close()  
  
}

class DataPrintTarget extends DataTarget {

  val tagStack = mutable.ListBuffer[String]()

  private def printWithTabs(s:String) = {
    tagStack.foreach(_ => print("  "))
    println(s)
  }

  def putBoolean(b:Boolean) = printWithTabs(b + " : Boolean")

  def putByte(i:Byte) = printWithTabs(i + " : Byte")

  def putShort(i:Short) = printWithTabs(i + " : Short")

  def putChar(i:Char) = printWithTabs(i + " : Char")

  def putInt(i:Int) = printWithTabs(i + " : Int")

  def putLong(l:Long) = printWithTabs(l + " : Long")

  def putFloat(f:Float) = printWithTabs(f + " : Float")

  def putDouble(d:Double) = printWithTabs(d + " : Double")

  def putUTF(s:String) = printWithTabs(s + " : String")

  def openTag(s:String) = {
    printWithTabs(s + " {")
    tagStack.append(s)
  }

  def closeTag() = {
    printWithTabs("} " + tagStack.remove(tagStack.size-1))
  }

  def flush() = {}

  def close() = {
    if (!tagStack.isEmpty) throw new RuntimeException("Closed DataPrintTarget with tags still open")
  }
}