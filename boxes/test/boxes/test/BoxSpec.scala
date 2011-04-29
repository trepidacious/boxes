package boxes.test

import org.scalatest.WordSpec
import scala.collection._
import boxes._
import immutable.Queue

class BoxSpec extends WordSpec {

  //FIXME we should try to test effect of GC - make sure that reactions
  //are not GCed as long as they have a source

  class Person {
    val name = Var("name")
    val age = Var(32)
    val friend:VarSingle[Person] = Var(null)
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

      var lastChanges:Option[immutable.Queue[ChangeSingle[String]]] = Some(immutable.Queue(ChangeSingle("MustChange")))

      val v = View{
        lastChanges = alice.name.changes
      }

      //View is called with no changes when it is first set up
      assert(lastChanges === None)

      alice.name() = "Alicia"

      assert(lastChanges === Some(immutable.Queue(ChangeSingle("Alicia"))))

      alice.name() = "Alucard"

      assert(lastChanges === Some(immutable.Queue(ChangeSingle("Alucard"))))
    }
  }

  "ListUtils" should {

    "insert a single element" in {
      val l = List(1, 2, 3, 4)
      assert(ListUtils.insert(l, 0, 42).sameElements(List(42, 1, 2, 3, 4)))
      assert(ListUtils.insert(l, 1, 42).sameElements(List(1, 42, 2, 3, 4)))
      assert(ListUtils.insert(l, 2, 42).sameElements(List(1, 2, 42, 3, 4)))
      assert(ListUtils.insert(l, 3, 42).sameElements(List(1, 2, 3, 42, 4)))
      assert(ListUtils.insert(l, 4, 42).sameElements(List(1, 2, 3, 4, 42)))
    }

    "insert multiple elements" in {
      val l = List(1, 2, 3, 4)
      assert(ListUtils.insert(l, 0, 42, 43).sameElements(List(42, 43, 1, 2, 3, 4)))
      assert(ListUtils.insert(l, 1, 42, 43).sameElements(List(1, 42, 43, 2, 3, 4)))
      assert(ListUtils.insert(l, 2, 42, 43).sameElements(List(1, 2, 42, 43, 3, 4)))
      assert(ListUtils.insert(l, 3, 42, 43).sameElements(List(1, 2, 3, 42, 43, 4)))
      assert(ListUtils.insert(l, 4, 42, 43).sameElements(List(1, 2, 3, 4, 42, 43)))
    }

    "remove a single element" in {
      val l = List(1, 2, 3, 4)
      assert(ListUtils.remove(l, 0, 1).sameElements(List(2, 3, 4)))
      assert(ListUtils.remove(l, 1, 1).sameElements(List(1, 3, 4)))
      assert(ListUtils.remove(l, 2, 1).sameElements(List(1, 2, 4)))
      assert(ListUtils.remove(l, 3, 1).sameElements(List(1, 2, 3)))
    }

    "remove multiple elements" in {
      val l = List(1, 2, 3, 4)
      assert(ListUtils.remove(l, 0, 2).sameElements(List(3, 4)))
      assert(ListUtils.remove(l, 1, 2).sameElements(List(1, 4)))
      assert(ListUtils.remove(l, 2, 2).sameElements(List(1, 2)))
    }

  }

  "ListCal" should {
    "calculate list of indices" in {
      val i = Var(0)
      val l = ListCal(Range(0, i()).toList)

      assert(l().sameElements(Range(0, 0).toList))

      i() = 10
      assert(l().sameElements(Range(0, 10).toList))
    }
  }

  "ListCal" should {
    "work with Path" in {
      val i = Var(0)
      val l = ListCal(Range(0, i()).toList)

      assert(l().sameElements(Range(0, 0).toList))

      i() = 10
      assert(l().sameElements(Range(0, 10).toList))

      val v = Var(l)
      val p = Path(v)

      assert(v()().sameElements(Range(0, 10).toList))
      assert(p()().sameElements(Range(0, 10).toList))

    }
  }

  "ListVar" should {
    "allow replacement, insertion and removal" in {
      val l = ListVar(0, 1, 2, 3)
      l(1) = 42
      assert(l().sameElements(List(0, 42, 2, 3)))
      l.remove(1, 1)
      assert(l().sameElements(List(0, 2, 3)))
      l.insert(1, 24, 25)
      assert(l().sameElements(List(0, 24, 25, 2, 3)))
    }

    "notify replacement, insertion and removal" in {
      val l = ListVar(0, 1, 2, 3)
      var changes:Option[Queue[ListChange]] = None
      val v = View(changes = l.changes)
      assert(changes == None)
      l(1) = 42
      assert(changes == Some(Queue(ReplacementListChange(1, 1))))
      l.remove(2, 1)
      assert(changes == Some(Queue(RemovalListChange(2, 1))))
      l.insert(3, 24, 25)
      assert(changes == Some(Queue(InsertionListChange(3, 2))))
    }

  }
//
//  "ListIndices" should {
//    "track single index" in {
//      val l = ListVar(0, 1, 2, 3, 4, 5, 6, 7)
//      val r = Var(l)
//      val i = ListIndex[Int, ListVar[Int]](r)
//
//      assert(i() === 0)
//
//      //Can't select -1 when list is not empty
//      i() = -1
//      assert(i() === 0)
//
//      //Can't select past end of list - just selects last index
//      i() = 10
//      assert(i() === 7)
//
//      i() = 4
//      assert(i() === 4)
//
//      l(0) = 42
//      assert(i() === 4)
//
//      l(0) = 0
//      assert(i() === 4)
//
//      println("About to remove")
//      l.remove(0, 2)
//      println("Removed")
//      println(l())
//      println(i())
//      assert(i() === 2)
//      assert(l(i()) === 4)
//
//      l.insert(0, 0, 1)
//      assert(i() === 4)
//      assert(l(i()) === 4)
//
//      //Completely replace the ListVar with a new one, should reset selection
//      r() = ListVar(0, 1, 2, 3)
//      assert(i() === 0)
//
//    }
//  }

}