package boxes.util

import java.text.{DecimalFormat, NumberFormat}

trait NumericClass[N] {
  def numericClass:Class[N]
  def javaWrapperClass:Class[_]
  def toN(n:Number):N
  def parse(s:String):N
  def format(n:N):String
  def formatInstance:NumberFormat
  def isWhole:Boolean
  def defaultSequence:Sequence[N]
}

object NumericClass {
  implicit object IntClass extends NumericClass[Int] {
    override def formatInstance = NumberFormat.getIntegerInstance
    private val format = formatInstance
    override def numericClass = classOf[Int]
    override def javaWrapperClass = classOf[java.lang.Integer]
    override def toN(n:Number) = n.intValue
    def parse(s:String) = format.synchronized(toN(format.parse(s)))
    def format(n:Int) = format.synchronized(format.format(n))
    val isWhole = true
    val defaultSequence = Step(1)
  }
  implicit object ShortClass extends NumericClass[Short] {
    override def formatInstance = NumberFormat.getIntegerInstance
    private val format = formatInstance
    override def numericClass = classOf[Short]
    override def javaWrapperClass = classOf[java.lang.Short]
    override def toN(n:Number) = n.shortValue
    def parse(s:String) = format.synchronized(toN(format.parse(s)))
    def format(n:Short) = format.synchronized(format.format(n))
    val isWhole = true
    val defaultSequence = Step(1.asInstanceOf[Short])
  }
  implicit object ByteClass extends NumericClass[Byte] {
    override def formatInstance = NumberFormat.getIntegerInstance
    private val format = formatInstance
    override def numericClass = classOf[Byte]
    override def javaWrapperClass = classOf[java.lang.Byte]
    override def toN(n:Number) = n.byteValue
    def parse(s:String) = format.synchronized(toN(format.parse(s)))
    def format(n:Byte) = format.synchronized(format.format(n))
    val isWhole = true
    val defaultSequence = Step(1.asInstanceOf[Byte])
  }
  implicit object LongClass extends NumericClass[Long] {
    override def formatInstance = NumberFormat.getIntegerInstance
    private val format = formatInstance
    override def numericClass = classOf[Long]
    override def javaWrapperClass = classOf[java.lang.Long]
    override def toN(n:Number) = n.longValue
    def parse(s:String) = format.synchronized(toN(format.parse(s)))
    def format(n:Long) = format.synchronized(format.format(n))
    val isWhole = true
    val defaultSequence = Step(1L)
  }
  implicit object FloatClass extends NumericClass[Float] {
    override def formatInstance = NumberFormat.getNumberInstance
    private val format = formatInstance
    override def numericClass = classOf[Float]
    override def javaWrapperClass = classOf[java.lang.Float]
    override def toN(n:Number) = n.floatValue
    def parse(s:String) = format.synchronized(toN(format.parse(s)))
    def format(n:Float) = format.synchronized(format.format(n))
    val isWhole = false
    val defaultSequence = Step(1f)
  }
  implicit object DoubleClass extends NumericClass[Double] {
    override def formatInstance = NumberFormat.getNumberInstance
    private val format = formatInstance
    override def numericClass = classOf[Double]
    override def javaWrapperClass = classOf[java.lang.Double]
    override def toN(n:Number) = n.doubleValue
    def parse(s:String) = format.synchronized(toN(format.parse(s)))
    def format(n:Double) = format.synchronized(format.format(n))
    val isWhole = false
    val defaultSequence = LogStep(10)
  }
  implicit object BigIntClass extends NumericClass[BigInt] {
    override def formatInstance = {
      val format = new DecimalFormat
      format.setParseBigDecimal(true)
      format.setParseIntegerOnly(true)
      format
    }
    private val format = formatInstance
    override def numericClass = classOf[BigInt]
    override def javaWrapperClass = classOf[java.math.BigInteger]
    override def toN(n:Number) = n match {
      case b:java.math.BigInteger => new BigInt(b)
      case b:BigInt => b
      case n:Number => BigInt(n.longValue)
    }
    def parse(s:String) = format.synchronized(toN(format.parse(s)))
    def format(n:BigInt) = format.synchronized(format.format(n))
    val isWhole = true
    val defaultSequence = Step(BigInt(1))
  }

  implicit object BigDecimalClass extends NumericClass[BigDecimal] {
    override def formatInstance = {
      val format = new DecimalFormat
      format.setParseBigDecimal(true)
      format
    }
    private val format = formatInstance
    override def numericClass = classOf[BigDecimal]
    override def javaWrapperClass = classOf[java.math.BigDecimal]
    override def toN(n:Number) = n match {
      case b:java.math.BigDecimal => new BigDecimal(b)
      case b:BigDecimal => b
      case n:Number => BigDecimal(n.doubleValue)
    }
    def parse(s:String) = format.synchronized(toN(format.parse(s)))
    def format(n:BigDecimal) = format.synchronized(format.format(n))
    val isWhole = false
    val defaultSequence = Step(BigDecimal(1))
  }

  val classes = List(
    classOf[Byte],
    classOf[Double],
    classOf[Long],
    classOf[Float],
    classOf[Int],
    classOf[Short],
    classOf[BigInt],
    classOf[BigDecimal]
  )

}
