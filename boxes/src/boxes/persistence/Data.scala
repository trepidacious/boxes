package boxes.persistence

trait AnyTag
case class Tag(text:String, id:Option[Int]=None, ref:Option[Int]=None) extends AnyTag
case class ClassTag(clazz:Class[_], id:Option[Int]=None, ref:Option[Int]=None) extends AnyTag

//Result of checking for an object in the cache
trait CacheResult
//Object is already cached, use a ref as given
case class Cached(ref:Int) extends CacheResult
//Object was not cached, use an id as given
case class New(id:Int) extends CacheResult

trait DataSource {

  /**
   * Store something in the cache.
   * Must be called by any Codec that has
   * just decoded something that had an
   * id. This makes the decoded thing
   * available for following codecs to
   * retrieve if they encounter a ref.
   */
  def cache(id:Int, thing:Any)

  /**
   * Retrieve a thing cached by a previous
   * codec with a given id. The ref parameter
   * is the id of the object, and is presumably
   * present in the decoded data from the source.
   */
  def retrieveCached(ref:Int):Any

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
 * a binary format will use a particular binary representation
 */
trait DataTarget {

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
  def cache(thing:Any):CacheResult

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
