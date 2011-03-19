package boxes.test

import org.scalatest.WordSpec
import scala.collection._
import boxes._

class BoxSpec extends WordSpec {

  //FIXME we should try to test effect of GC - make sure that reactions
  //are not GCed as long as they have a target

  class Person {
    val name = Var("name")
    val age = Var(32)
    val friend:Var[Person] = Var(null)
  }

  "Cal" should {

    "work in simple case" in {
      val a = Var(2)
      val b = Cal(a() + 3)

      assert(b() === 5)

      val c = Cal(a() + b())

      assert(c() === 7)

      a() = 4

      assert(b() === 7)
      assert(c() === 11)
    }

    "permit simple unidirectional paths" in {

      val cate = new Person()
      cate.name() = "Cate"

      val alice = new Person()
      alice.name() = "Alice"

      val bob = new Person()
      bob.name() = "Bob"
      bob.friend() = cate

      val bobsFriendsName = Cal(bob.friend().name())

      var alterations = 0
      val aView = View {
        //Read this so that we are called when it changes
        bobsFriendsName()
        alterations += 1
      }

      //Starts from 1, because view is called once
      //when registered
      assert(alterations === 1)

      assert(bobsFriendsName() === "Cate")

      bob.friend() = alice

      assert(alterations === 2)

      assert(bobsFriendsName() === "Alice")

      //Should see no changes to bobsFriendsName when something not
      //in the path changes, even if it is deeply referenced, or used to be part of path, etc.
      cate.name() = "Katey"

      assert(alterations === 2)

      assert(bobsFriendsName() === "Alice")

      alice.name() = "Alicia"
      assert(bobsFriendsName() === "Alicia")

      assert(alterations === 3)
    }

  }

  "Reaction" should {
    "support a cycle of two reactions, enforcing a relationship between two Vars" in {

      val x = Var(2d)
      val doubleX = Var(0d)

      Reaction(doubleX, x()*2d, "doubleX = 2 * x")
      assert(x() === 2d)
      assert(doubleX() === 4d)

      Reaction(x, doubleX()/2d, "x = doubleX / 2")
      assert(x() === 2d)
      assert(doubleX() === 4d)

      x() = 4
      assert(x() === 4d)
      assert(doubleX() === 8d)

      doubleX() = 10
      assert(x() === 5d)
      assert(doubleX() === 10d)
    }

  }

  "Box" should {

    "support multiple reactions targetting the same Box, where they do not conflict" in {
      val x = Var(2d)
      val y = Var(0d)

      Reaction(y, x() * 2, "double")
      assert(x() === 2d)
      assert(y() === 4d)

      Reaction(y, x() * 2, "also double")
      assert(x() === 2d)
      assert(y() === 4d)
    }

    "throw FailedReactionsException if reactions conflict" in {
      val x = Var(2d)
      val y = Var(0d)

      Reaction(y, x() * 2, "double")

      intercept[FailedReactionsException] {
        Reaction(y, x() * 4, "quadruple")
      }

    }

  }

  "Path" should {
    "work for Person" in {

      val cate = new Person()
      cate.name() = "Cate"
      val alice = new Person()
      alice.name() = "Alice"
      val bob = new Person()
      bob.name() = "Bob"
      bob.friend() = cate

      val bobsFriendsName = Path(bob.friend().name)

      assert(bobsFriendsName() === "Cate")

      bob.friend() = alice

      assert(bobsFriendsName() === "Alice")

      //Should see no changes to bobsFriendsName when something not
      //in the path changes, even if it is deeply referenced, or used to be part of path, etc.
      var changed = false
      val v = View{bobsFriendsName(); changed = true}

      //Setting up the view leads to it being called
      assert(changed === true)

      //Now we reset, so we can see if we get a new change
      changed = false

      cate.name() = "Katey"

      //We shouldn't have a change to bobsFriendsName, from changing cate's name
      assert(changed === false)

      alice.name() = "Alicia"

      //NOW we should have a change
      assert(changed === true)

      assert(bobsFriendsName() === "Alicia")

      bobsFriendsName() = "Alucard"

      assert(bobsFriendsName() === "Alucard")
      assert(alice.name() === "Alucard")

    }
  }

  "View" should {
    "report only changes from current cycle" in {

      val alice = new Person()
      alice.name() = "Alice"

      var lastChanges:Option[immutable.Queue[String]] = Some(immutable.Queue("MustChange"))

      val v = View{
        lastChanges = alice.name.changes
      }

      //View is called with no changes when it is first set up
      assert(lastChanges === None)

      alice.name() = "Alicia"

      assert(lastChanges === Some(immutable.Queue("Alicia")))

      alice.name() = "Alucard"

      assert(lastChanges === Some(immutable.Queue("Alucard")))
    }
  }


}