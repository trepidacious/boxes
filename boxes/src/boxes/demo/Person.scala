package boxes.demo

import boxes.Var
import boxes.list.ListVar
import boxes.Node

object Person {
  def testPerson = {
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
    p
  }
}

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
