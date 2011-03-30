package boxes.util

trait NumericClass[N] {
  def numericClass:Class[N]
  def javaWrapperClass:Class[_]
}

object NumericClass {
  implicit object IntClass extends NumericClass[Int] {
    override def numericClass = classOf[Int]
    override def javaWrapperClass = classOf[java.lang.Integer]
  }
  implicit object ShortClass extends NumericClass[Short] {
    override def numericClass = classOf[Short]
    override def javaWrapperClass = classOf[java.lang.Short]
  }
  implicit object ByteClass extends NumericClass[Byte] {
    override def numericClass = classOf[Byte]
    override def javaWrapperClass = classOf[java.lang.Byte]
  }
  implicit object LongClass extends NumericClass[Long] {
    override def numericClass = classOf[Long]
    override def javaWrapperClass = classOf[java.lang.Long]
  }
  implicit object FloatClass extends NumericClass[Float] {
    override def numericClass = classOf[Float]
    override def javaWrapperClass = classOf[java.lang.Float]
  }
  implicit object DoubleClass extends NumericClass[Double] {
    override def numericClass = classOf[Double]
    override def javaWrapperClass = classOf[java.lang.Double]
  }
  implicit object BigIntClass extends NumericClass[BigInt] {
    override def numericClass = classOf[BigInt]
    override def javaWrapperClass = classOf[java.math.BigInteger]
  }
  implicit object CharClass extends NumericClass[Char] {
    override def numericClass = classOf[Char]
    override def javaWrapperClass = classOf[java.lang.Character]
  }
  implicit object BigDecimalClass extends NumericClass[BigDecimal] {
    override def numericClass = classOf[BigDecimal]
    override def javaWrapperClass = classOf[java.math.BigDecimal]
  }
}
