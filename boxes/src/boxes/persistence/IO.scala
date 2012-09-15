package boxes.persistence

import collection._
import java.io.{InputStream, OutputStream}
import boxes.Box

class ClassAliases {
  private val aliases = mutable.Map[Class[_], String]()
  private val aliasesReverse = mutable.Map[String, Class[_]]()

  //Common default aliases

  {
    alias(classOf[java.lang.Long],    "Long")
    alias(classOf[java.lang.Integer], "Int")
    alias(classOf[java.lang.Short],   "Short")
    alias(classOf[java.lang.Byte],    "Byte")
    alias(classOf[java.lang.Boolean], "Boolean")
    alias(classOf[java.lang.Double],  "Double")
    alias(classOf[java.lang.Float],   "Float")
    alias(classOf[java.lang.String],  "String")

    alias(classOf[List[_]],           "List")
    alias(classOf[Map[_,_]],          "Map")
    alias(classOf[Set[_]],            "Set")
    alias(classOf[Option[_]],         "Option")

  }

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

  def forAlias(s:String) = aliasesReverse.getOrElse(s, Class.forName(s))

}

trait DataFactory {
  def reader(input:InputStream, aliases:ClassAliases):TokenReader
  def writer(output:OutputStream, aliases:ClassAliases):TokenWriter
}

class IO(val dataFactory:DataFactory, val aliases:ClassAliases = new ClassAliases) {

  val codecByClass = new CodecByClass()

  def code(t:Any, output:OutputStream) = {
    //Code as a transaction, to prevent concurrent modification
    Box.transact {
      val target = dataFactory.writer(output, aliases)
      codecByClass.write(t, target)
      target.close
    }
  }

  def decode(input:InputStream) = {
    //Decode, so we run as a transaction, AND reactions are handled properly
    Box.decode {
      val source = dataFactory.reader(input, aliases)
      val t = codecByClass.read(source)
      source.close
      t
    }
  }

  def alias(c:Class[_], s:String) = aliases.alias(c, s)

  def add(codec:CodecWithClass[_]) {
    codecByClass.add(codec)
  }

  def add(codec:Codec[_], clazz:Class[_]) {
    codecByClass.add(codec, clazz)
  }

}
