package boxes.persistence.test

import org.scalatest.WordSpec
import scala.collection._
import boxes._
import java.io.StringWriter
import io.Source
import persistence._

class Person extends Node {
    val name = Var("name")
    val age = Var(32)
    val friend:Var[Option[Person]] = Var(None)
    val spouse:Var[Option[Person]] = Var(None)
    val numbers = Var(List[Int]())
    val accounts = Var(Map[String, Double]())

    override def toString = name() + ", " + age() + ", friend: " + friend() + ", spouse " + spouse() + ", numbers " + numbers() + ", accounts " + accounts()
  }

class PersistenceSpec extends WordSpec {

  def assertPersonsEqualButNotSame(p:Person, dp:Person) {
    assert(p ne dp)

    assert(p.name() === dp.name())
    assert(p.age() === dp.age())

    assert(p.numbers() === dp.numbers())
    assert(p.numbers() ne dp.numbers())

    assert(p.accounts() === dp.accounts())
    assert(p.accounts() ne dp.accounts())
  }

  "CodecByClass and XML" should {

    "code and decode some people " in {

      //encode
      val encode = new CodecByClass()

      val encodeAliases = new XMLAliases
      encodeAliases.alias(classOf[Person], "Person")

      val s = new StringWriter()
      val target = new XMLDataTarget(s, encodeAliases)

      val p = new Person()
      p.accounts() = Map("current" -> 10.0, "savings" -> 100.0, "secretswiss" -> 10000000.0)
      p.numbers() = List(10,20,30)
      p.age() = 100

      val q = new Person()
      q.accounts() = Map("current" -> 0.0)
      q.numbers() = List(1, 4, 9)
      q.name() = "q"

      p.friend() = Some(q)
      p.spouse() = Some(q)

      encode.code(p, target)

      val xml = s.toString

      //Decode
      val src = Source.fromString(xml)

      val decode = new CodecByClass()
      val decodeAliases = new XMLAliases
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
  }

  "XMLDataSource and XMLDataTarget" should {
    "encode and decode an empty String" in {
      val s = new StringWriter()
      val target = new XMLDataTarget(s, new XMLAliases)
      target.openClassTag(classOf[String])
      target.putUTF("")
      target.closeTag

      val xml = s.toString
      val source = new XMLDataSource(Source.fromString(xml), new XMLAliases)

      source.assertOpenClassTag(ClassTag(classOf[String]))
      val emptyString = source.getUTF
      source.getCloseTag

      assert(emptyString === "")
    }
  }


}