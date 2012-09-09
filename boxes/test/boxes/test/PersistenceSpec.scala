package boxes.test

import boxes.list.ListVar
import boxes.persistence.ClassAliases
import boxes.persistence.ClassTag
import boxes.persistence.CodecByClass
import boxes.persistence.XMLDataSource
import boxes.persistence.XMLDataTarget
import boxes.persistence.XMLIO
import boxes.Var
import boxes.Node
import boxes.Reaction
import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.io.StringWriter
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.WordSpec
import scala.collection.Map
import scala.io.Source

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

class SpecificStateReaction(aI:Boolean, bI:Boolean) extends Node {

  def this() = this(false, false)

  val a = Var(aI)
  val b = Var(bI)

  //a and b can only become true simultaneously
  Reaction(a, a() & b())
  Reaction(b, a() & b())

  override def toString = "a() = " + a() + ", b() = " + b()
}

@RunWith(classOf[JUnitRunner])
class PersistenceSpec extends WordSpec {

  def assertPersonsEqualButNotSame(p:Person, dp:Person) {
    assert(p ne dp)

    assert(p.name() === dp.name())
    assert(p.age() === dp.age())

    assert(p.numbers() === dp.numbers())
    assert(p.numbers() ne dp.numbers())

    assert(p.accounts() === dp.accounts())
    assert(p.accounts() ne dp.accounts())

    assert(p.nicknames() === dp.nicknames())
    assert(p.nicknames() ne dp.nicknames())
  }

  "CodecByClass and XML" should {

    "code and decode some people " in {

      //encode
      val encode = new CodecByClass()

      val encodeAliases = new ClassAliases
      encodeAliases.alias(classOf[Person], "Person")

      val s = new StringWriter()
      val target = new XMLDataTarget(s, encodeAliases)

      val p = new Person()
      p.accounts() = Map("current" -> 10.0, "savings" -> 100.0, "secretswiss" -> 10000000.0)
      p.numbers() = List(10,20,30)
      p.age() = 100
      p.nicknames() = List("Pico", "Pi")

      val q = new Person()
      q.accounts() = Map("current" -> 0.0)
      q.numbers() = List(1, 4, 9)
      q.name() = "q"
      q.nicknames() = List("Queue", "Cue", "QED")

      p.friend() = Some(q)
      p.spouse() = Some(q)

      encode.code(p, target)

      val xml = s.toString
      println(xml)

      //Decode
      val src = Source.fromString(xml)

      val decode = new CodecByClass()
      val decodeAliases = new ClassAliases
      decodeAliases.alias(classOf[Person], "Person")
      val source = new XMLDataSource(src, decodeAliases)

      val dp = decode.decode(source).asInstanceOf[Person]

      //Check decoded person is same as original, but not identical
      assertPersonsEqualButNotSame(p, dp)

      val odq = dp.friend()

      //FIXME there must be a neater way to do this
      odq match {
        case None => throw new RuntimeException("dp has no friend")
        case Some(dq) => {
          assertPersonsEqualButNotSame(q, dq)

          //Now we check that dp's spouse is the same exact instance as his friend,
          //to check we are encoding instances using ref and id correctly
          dp.spouse() match {
            case None => throw new RuntimeException("dp has no spouse")
            case Some(dps) => {
              assert(dq eq dps)
            }
          }
        }
      }

      //Quick check that changing friend's name changes spouse's name
      dp.friend().foreach(_.name() = "qe2")
      dp.spouse().foreach(spouse => assert(spouse.name() === "qe2"))
    }

    "code and decode a class with reactions requiring a specific state" in {
      //If we make a default instance, we can't ever get either var to be true
      val s = new SpecificStateReaction

      s.a() = true
      assert(s.a() === false)
      s.b() = true
      assert(s.b() === false)

      s.a() = true
      s.b() = true
      assert(s.a() === false)
      assert(s.b() === false)

      //But if we make one where both vars are true before reactions are applied, it works
      val s2 = new SpecificStateReaction(true, true)
      assert(s2.a() === true)
      assert(s2.b() === true)

      //Now encode and decode s2 without using Box.decode, to show that by using the default
      //values in the constructor, the wrong result occurs, and the vars become false in the
      //duplicate
      val codec = new CodecByClass()

      val aliases = new ClassAliases

      val writer = new StringWriter()
      val target = new XMLDataTarget(writer, aliases)

      codec.code(s2, target)

      val xml = writer.toString
      println(xml)

      //Decode
      val src = Source.fromString(xml)

      val source = new XMLDataSource(src, aliases)

      val s3 = codec.decode(source).asInstanceOf[SpecificStateReaction]

      assert(s3.a() === false)
      assert(s3.b() === false)

      //Now we encode and decode s2 with XMLIO, which uses Box.decode, and so decodes correctly
      val io = XMLIO()
      val baos = new ByteArrayOutputStream()
      io.code(s2, baos)
      val s4 = io.decode(new ByteArrayInputStream(baos.toByteArray)).asInstanceOf[SpecificStateReaction]

      assert(s4.a() === true)
      assert(s4.b() === true)

      //And the reactions still work
      s4.a() = false
      assert(s3.a() === false)
      assert(s3.b() === false)
    }
  }

  "XMLDataSource and XMLDataTarget" should {
    "encode and decode an empty String" in {
      val s = new StringWriter()
      val target = new XMLDataTarget(s, new ClassAliases)
      target.openClassTag(classOf[String])
      target.putUTF("")
      target.closeTag

      val xml = s.toString
      val source = new XMLDataSource(Source.fromString(xml), new ClassAliases)

      source.assertOpenClassTag(ClassTag(classOf[String]))
      val emptyString = source.getUTF
      source.getCloseTag

      assert(emptyString === "")
    }
  }



}