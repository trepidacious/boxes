package boxes.demo

import scala.xml._
import scala.xml.pull._
import scala.io.Source

object XMLProto {

  val src = Source.fromString("<Person id='1' ref='2'><name><java.lang.String>name</java.lang.String></name><friend><scala.Option><Some><Person><name><java.lang.String>q</java.lang.String></name><friend><scala.Option><None></None></scala.Option></friend><numbers><scala.collection.immutable.List><java.lang.Integer>1</java.lang.Integer><java.lang.Integer>4</java.lang.Integer><java.lang.Integer>9</java.lang.Integer></scala.collection.immutable.List></numbers><age><java.lang.Integer>32</java.lang.Integer></age></Person></Some></scala.Option></friend><numbers><scala.collection.immutable.List></scala.collection.immutable.List></numbers><age><java.lang.Integer>32</java.lang.Integer></age></Person>")
  val er = new XMLEventReader(src)

  def main(args: Array[String]) {
    val e = er.next.asInstanceOf[EvElemStart]
    println(e.attrs("id").apply(0).text.toInt)
    println(e.attrs("ref").apply(0).text.toInt)
  }

}