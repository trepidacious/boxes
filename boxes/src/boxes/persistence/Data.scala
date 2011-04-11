package boxes.persistence

trait AnyTag
case class Tag(text:String, id:Option[Int]=None, ref:Option[Int]=None) extends AnyTag
case class ClassTag(clazz:Class[_], id:Option[Int]=None, ref:Option[Int]=None) extends AnyTag

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

  /**
   * Return the details of the next open tag.
   * If the next data is NOT an open tag, then
   * a RunTimeException is thrown.
   * If consume is true, the tag is removed from the source,
   * and will not be returned again. Otherwise the tag
   * will still be present in the source, as the next
   * data to be retrieved.
   */
  def getOpenTag(consume:Boolean):Tag

  //TODO DataSource should provide caching for users - when they
  //decode something with an id they must put it in the cache
  //for subsequent codecs to retrieve when they see a ref.
  //DataTarget will do the other part - handing out new ids, and
  //storing a map to look up the ref for use if an object is seen
  //again.

  /**
   * Return the details of the next open class tag.
   * If the next data is NOT an open class tag, then
   * a RunTimeException is thrown.
   * If consume is true, the tag is removed from the source,
   * and will not be returned again. Otherwise the tag
   * will still be present in the source, as the next
   * data to be retrieved.
   */
  def getOpenClassTag(consume:Boolean):ClassTag

  /**
   * Equivalent to calling getOpenClassTag(true) and
   * then throwing an exception if the tag is not
   * equal to the expectedTag
   */
  def assertOpenClassTag(expectedTag:ClassTag)

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

  def close()
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
  def openClassTag(c:Class[_], id:Option[Int]=None, ref:Option[Int]=None)
  def closeTag()
  def flush()
  def close()
}

trait ValHandler[T] {
  def clazz():Class[_]
  def put(target:DataTarget, t:T)
  def get(source:DataSource):T
}

object ValHandlers {
  implicit object BooleanHandler extends ValHandler[Boolean] {
    override def clazz = classOf[java.lang.Boolean]
    override def put(target:DataTarget, t:Boolean) = target.putBoolean(t)
    override def get(source:DataSource) = source.getBoolean
  }
  implicit object IntHandler extends ValHandler[Int] {
    override def clazz = classOf[java.lang.Integer]
    override def put(target:DataTarget, t:Int) = target.putInt(t)
    override def get(source:DataSource) = source.getInt
  }
  implicit object ShortHandler extends ValHandler[Short] {
    override def clazz = classOf[java.lang.Short]
    override def put(target:DataTarget, t:Short) = target.putShort(t)
    override def get(source:DataSource) = source.getShort
  }
  implicit object ByteHandler extends ValHandler[Byte] {
    override def clazz = classOf[java.lang.Byte]
    override def put(target:DataTarget, t:Byte) = target.putByte(t)
    override def get(source:DataSource) = source.getByte
  }
  implicit object LongHandler extends ValHandler[Long] {
    override def clazz = classOf[java.lang.Long]
    override def put(target:DataTarget, t:Long) = target.putLong(t)
    override def get(source:DataSource) = source.getLong
  }
  implicit object FloatHandler extends ValHandler[Float] {
    override def clazz = classOf[java.lang.Float]
    override def put(target:DataTarget, t:Float) = target.putFloat(t)
    override def get(source:DataSource) = source.getFloat
  }
  implicit object DoubleHandler extends ValHandler[Double] {
    override def clazz = classOf[java.lang.Double]
    override def put(target:DataTarget, t:Double) = target.putDouble(t)
    override def get(source:DataSource) = source.getDouble
  }
  implicit object CharHandler extends ValHandler[Char] {
    override def clazz = classOf[java.lang.Character]
    override def put(target:DataTarget, t:Char) = target.putChar(t)
    override def get(source:DataSource) = source.getChar
  }
  implicit object StringHandler extends ValHandler[String] {
    override def clazz = classOf[java.lang.String]
    override def put(target:DataTarget, t:String) = target.putUTF(t)
    override def get(source:DataSource) = source.getUTF
  }

}