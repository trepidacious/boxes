package boxes.test

import org.scalatest.WordSpec
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import boxes._
import list._
import scala.collection.immutable.Queue

@RunWith(classOf[JUnitRunner])
class BoxSpec extends WordSpec {

  //FIXME we should try to test effect of GC - make sure that reactions
  //are not GCed as long as they have a source

  class Person {
    val name = Var("name")
    val age = Var(32)
    val friend:Var[Person] = Var(null)
  }

  class OptionPerson extends Node {
    val name = Var("name")
    val age = Var(32)
    val friend:Var[Option[OptionPerson]] = Var(None)
    val spouse:Var[Option[OptionPerson]] = Var(None)
    val numbers = Var(List[Int]())
    val accounts = Var(Map[String, Int]())

    override def toString = name() + ", " + age() + ", friend: " + friend() + ", spouse " + spouse() + ", numbers " + numbers() + ", accounts " + accounts()
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

      Reaction(doubleX, x()*2d)
      assert(x() === 2d)
      assert(doubleX() === 4d)

      Reaction(x, doubleX()/2d)
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

      Reaction(y, x() * 2)
      assert(x() === 2d)
      assert(y() === 4d)

      Reaction(y, x() * 2)
      assert(x() === 2d)
      assert(y() === 4d)
    }

    "throw FailedReactionsException if reactions conflict" in {
      val x = Var(2d)
      val y = Var(0d)

      Reaction(y, x() * 2)

      intercept[FailedReactionsException] {
        Reaction(y, x() * 4)
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

    "support paths via Options" in {

      val cate = new OptionPerson()
      cate.name() = "Cate"
      val alice = new OptionPerson()
      alice.name() = "Alice"
      val bob = new OptionPerson()
      bob.name() = "Bob"

      //Bob may not have a friend - but we can create a path
      //through this to make a Var[Option[String]] that will
      //contain bob's friend's name when bob has a friend,
      //or None otherwise.
      val bobsFriendsName = PathViaOption(
        for {
          friend <- bob.friend()
        } yield friend.name
      )

      //No friend yet
      assert(bobsFriendsName() === None)

      //Now we have a friend, and so a name
      bob.friend() = Some(alice)
      assert(bobsFriendsName() === Some("Alice"))

      //We can edit the name of Alice using either her own
      //name Var, or the path Var
      alice.name() = "Alicia"
      assert(alice.name() == "Alicia")
      assert(bobsFriendsName() === Some("Alicia"))

      bobsFriendsName() = Some("Aliss")
      assert(alice.name() == "Aliss")
      assert(bobsFriendsName() === Some("Aliss"))

      //We can reassign bob's friends, and do it all again
      bob.friend() = Some(cate)
      assert(bobsFriendsName() === Some("Cate"))

      cate.name() = "Kate"
      assert(cate.name() == "Kate")
      assert(bobsFriendsName() === Some("Kate"))
      assert(alice.name() == "Aliss")

      bobsFriendsName() = Some("Katherine")
      assert(cate.name() == "Katherine")
      assert(bobsFriendsName() === Some("Katherine"))
      assert(alice.name() == "Aliss")

      //Setting bobs friends name to None is meaningless, since
      //this is only the case when the path is broken (which it is
      //not) or cate's name is None (which it cannot be). Therefore
      //this is ignored.
      bobsFriendsName() = None
      assert(cate.name() == "Katherine")
      assert(bobsFriendsName() === Some("Katherine"))

    }

    "support paths to (and via) Options" in {

      val cate = new OptionPerson()
      cate.name() = "Cate"
      val alice = new OptionPerson()
      alice.name() = "Alice"
      val bob = new OptionPerson()
      bob.name() = "Bob"

      bob.friend() = Some(alice)

      //We can also have a path that goes TO
      //a Ref[Option[Something]]. In this case
      //it makes no difference whether or not the
      //path also goes via an optional element -
      //the resulting Path Var will contain None
      //whenever the Path fails OR the path succeeds
      //but leads to an end Ref that contains None.
      val bobsFriendsFriend = PathToOption(
        for {
          friend <- bob.friend()
        } yield friend.friend
      )

      //Bob has a friend, alice, but she has no friend, so
      //there is no "Bob's friend's friend"
      assert(bobsFriendsFriend() === None)

      //Now we give alice a friend, and complete the path
      alice.friend() = Some(cate)
      assert(bobsFriendsFriend() === Some(cate))

      //But if we break the path early, then we go back to None
      bob.friend() = None
      assert(bobsFriendsFriend() === None)

    }
  }

  "View" should {
    "report only changes from current cycle" in {

      val alice = new Person()
      alice.name() = "Alice"

      var lastChanges:Option[Queue[(Long, SingleChange[String])]] = Some(Queue((1, SingleChange("MustChange", "MustChange"))))

      val v = View{
        lastChanges = alice.name.changes
      }

      //View is called with no changes when it is first set up
      assert(lastChanges === None)

      alice.name() = "Alicia"

      assert(lastChanges != None)
      lastChanges.foreach(q => {
        assert(q.size === 1)
        assert(q.head._2 === SingleChange("Alice", "Alicia"))
      })

      alice.name() = "Alucard"

      assert(lastChanges != None)
      lastChanges.foreach(q => {
        assert(q.size === 1)
        assert(q.head._2 === SingleChange("Alicia", "Alucard"))
      })
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
      var changes:Option[Queue[(Long,ListChange[Int])]] = None
      val v = View(changes = l.changes)
      assert(changes == None)
      l(1) = 42
      assert(changes != None)
      changes.foreach(q => {
        assert(q.size === 1)
        assert(q.head._2 === ReplacementListChange(List(0, 1, 2, 3), List(0, 42, 2, 3), 1, 1))
      })

      l.remove(2, 1)
      assert(changes != None)
      changes.foreach(q => {
        assert(q.size === 1)
        assert(q.head._2 === RemovalListChange(List(0, 42, 2, 3), List(0, 42, 3), 2, 1))
      })

      l.insert(3, 24, 25)
      assert(changes != None)
      changes.foreach(q => {
        assert(q.size === 1)
        assert(q.head._2 === InsertionListChange(List(0, 42, 3), List(0, 42, 3, 24, 25), 3, 2))
      })
    }

  }

  "ListIndices" should {
    "track correctly" in {
      val l = ListVar(0, 1, 2, 3, 4, 5, 6, 7)
      val i = ListIndices(l)

      assert(i() === Set(0))

      //Can't select past end of list - just selects last element
      i() = Set(10)
      assert(i() === Set(7))

      i() = Set(4)
      assert(i() === Set(4))

      l(0) = 42
      assert(i() === Set(4))

      l(0) = 0
      assert(i() === Set(4))

      l.remove(0, 2)
      assert(i() === Set(2))

      l.insert(0, 0, 1)
      assert(i() === Set(4))

      //Completely replace the List with a new one, should reset selection
      l() = List(0, 1, 2, 3)
      assert(i() === Set(0))
    }

    "track correctly with multiple selections" in {
      val l = ListVar(0, 1, 2, 3, 4, 5, 6, 7)
      val i = ListIndices(l)

      i() = Set(0, 1, 5, 6)
      assert(i() === Set(0, 1, 5, 6))

      l(0) = 42
      assert(i() === Set(0, 1, 5, 6))

      l(0) = 0
      assert(i() === Set(0, 1, 5, 6))

      l.remove(2, 2)
      assert(i() === Set(0, 1, 3, 4))

      l.insert(2, 2, 3)
      assert(i() === Set(0, 1, 5, 6))

      //Completely replace the List with a new one, should reset selection
      l() = List(0, 1, 2, 3)
      assert(i() === Set(0))
    }

  }

  "ListIndex" should {
    "track correctly" in {
      val l = ListVar(0, 1, 2, 3, 4, 5, 6, 7)
      val i = ListIndex(l)

      assert(i() === Some(0))

      //Can't select past end of list - just selects last index
      i() = Some(10)
      assert(i() === Some(7))

      i() = Some(4)
      assert(i() === Some(4))

      l(0) = 42
      assert(i() === Some(4))

      l(0) = 0
      assert(i() === Some(4))

      l.remove(0, 2)
      assert(i() === Some(2))
      assert(l(i().getOrElse(-1)) === 4)

      l.insert(0, 0, 1)
      assert(i() === Some(4))
      assert(l(i().getOrElse(-1)) === 4)

      //Completely replace the List with a new one, should reset selection
      l() = List(0, 1, 2, 3)
      assert(i() === Some(0))
    }

    "work with ListSelection" in {
      val l = ListVar(0, 1, 2, 3, 4, 5, 6, 7)
      val i = ListIndex(l)
      val s = ListSelection(l, i)

      assert(i() === Some(0))
      assert(s() === Some(0))

      i() = Some(10)
      assert(i() === Some(7))
      assert(s() === Some(7))

      i() = Some(4)
      assert(i() === Some(4))
      assert(s() === Some(4))

      l(4) = 42
      assert(i() === Some(4))
      assert(s() === Some(42))


      l(4) = 4
      assert(i() === Some(4))
      assert(s() === Some(4))

      l.remove(0, 2)
      assert(i() === Some(2))
      assert(l(i().getOrElse(-1)) === 4)
      assert(s() === Some(4))

      l.insert(0, 0, 1)
      assert(i() === Some(4))
      assert(l(i().getOrElse(-1)) === 4)
      assert(s() === Some(4))

      //Completely replace the List with a new one, should reset selection
      l() = List(0, 1, 2, 3)
      assert(i() === Some(0))
      assert(s() === Some(0))

      l() = List()
      assert(s() === None)

    }

    "work with ListPath when modifying ListPath" in {

      class ListNode {
        val list = ListVar(0, 1, 2, 3, 4, 5, 6, 7)
      }
      val ln = new ListNode()

      val l = ListPath(ln.list)
      val i = ListIndex(l)

      assert(i() === Some(0))

      //Can't select past end of list - just selects last index
      i() = Some(10)
      assert(i() === Some(7))

      i() = Some(4)
      assert(i() === Some(4))

      l(0) = 42
      assert(i() === Some(4))

      l(0) = 0
      assert(i() === Some(4))

      l.remove(0, 2)
      assert(i() === Some(2))
      assert(l(i().getOrElse(-1)) === 4)

      l.insert(0, 0, 1)
      assert(i() === Some(4))
      assert(l(i().getOrElse(-1)) === 4)

      //Completely replace the List with a new one, should reset selection
      l() = List(0, 1, 2, 3)
      assert(i() === Some(0))
    }

    "work with ListPath when modifying ListPath endpoint" in {

      class ListNode {
        val list = ListVar(0, 1, 2, 3, 4, 5, 6, 7)
      }
      val ln = new ListNode()

      val l = ListPath(ln.list)
      val i = ListIndex(l)

      assert(i() === Some(0))

      //Can't select past end of list - just selects last index
      i() = Some(10)
      assert(i() === Some(7))

      i() = Some(4)
      assert(i() === Some(4))

      ln.list(0) = 42
      assert(i() === Some(4))

      ln.list(0) = 0
      assert(i() === Some(4))

      ln.list.remove(0, 2)
      assert(i() === Some(2))
      assert(l(i().getOrElse(-1)) === 4)

      ln.list.insert(0, 0, 1)
      assert(i() === Some(4))
      assert(l(i().getOrElse(-1)) === 4)

      //Completely replace the List with a new one, should reset selection
      ln.list() = List(0, 1, 2, 3)
      assert(i() === Some(0))
    }

    "work with ListPath when modifying path" in {
      class ListNode(l:Int*) {
        val list = ListVar(l:_*)
      }

      val ln = new ListNode(0, 1, 2, 3, 4, 5, 6, 7)
      val ln2 = new ListNode(0, 1, 2, 3, 4, 5)
      val s = Var(true)


      val l = ListPath(if (s()) ln.list else ln2.list)
      val i = ListIndex(l)

      assert(i() === Some(0))
      assert(l().sameElements(List(0, 1, 2, 3, 4, 5, 6, 7)))


      //Can't select past end of list - just selects last index
      i() = Some(10)
      assert(i() === Some(7))

      i() = Some(4)
      assert(i() === Some(4))

      ln.list(0) = 42
      assert(i() === Some(4))

      ln.list(0) = 0
      assert(i() === Some(4))

      ln.list.remove(0, 2)
      assert(i() === Some(2))
      assert(l(i().getOrElse(-1)) === 4)

      ln.list.insert(0, 0, 1)
      assert(i() === Some(4))
      assert(l(i().getOrElse(-1)) === 4)

      //Completely replace the List with a new one, should reset selection
      ln.list() = List(0, 1, 2, 3)
      assert(i() === Some(0))

      i() = Some(2)
      assert(i() === Some(2))

      //Change the path, should reset selection
      s() = false
      assert(i() === Some(0))
      assert(l().sameElements(List(0, 1, 2, 3, 4, 5)))

      //Check selection still works on new path
      i() = Some(4)
      assert(i() === Some(4))

      ln2.list.remove(0, 2)
      assert(i() === Some(2))
      assert(l(i().getOrElse(-1)) === 4)


    }
  }



  "ListIndex with loseIndexOnDeletion false" should {
    "leave selection alone on deletion after selection" in {
      val l = ListVar(0, 1, 2, 3, 4, 5, 6, 7, 8)
      val i = ListIndex(l, loseIndexOnDeletion=false)

      i() = Some(4)
      l.remove(5, 4)
      assert(i() === Some(4))
    }

    "move selection on deletion before selection" in {
      val l = ListVar(0, 1, 2, 3, 4, 5, 6, 7, 8)
      val i = ListIndex(l, loseIndexOnDeletion=false)

      i() = Some(4)
      l.remove(0, 3)
      assert(i() === Some(1))
    }

    "select last index after deletion from selection to end" in {
      val l = ListVar(0, 1, 2, 3, 4, 5, 6, 7, 8)
      val i = ListIndex(l, loseIndexOnDeletion=false)

      i() = Some(4)
      l.remove(4, 5)
      assert(i() === Some(3))
    }

    "select next undeleted index after deletion from selection to before end" in {
      val l = ListVar(0, 1, 2, 3, 4, 5, 6, 7, 8)
      val i = ListIndex(l, loseIndexOnDeletion=false)

      i() = Some(4)
      l.remove(4, 2)
      assert(i() === Some(4))
    }

    "change invalid initial selection to valid one" in {
      val l = ListVar(0, 1, 2, 3, 4, 5, 6, 7, 8)
      val i = ListIndex(l, Some(100), loseIndexOnDeletion=false)

      assert(i() === Some(8))
    }

    "clear selection on list change, if selectFirstRatherThanNone is false" in {
      val l = ListVar(0, 1, 2, 3, 4, 5, 6, 7, 8)
      val i = ListIndex(l, loseIndexOnDeletion=false, selectFirstRatherThanNone = false)

      i() = Some(4)
      assert(i() === Some(4))

      l() = List(42)
      assert(i() === None)
    }

    "clear selection then prefer 0 on list change for non-empty list, if selectFirstRatherThanNone is true" in {
      val l = ListVar(0, 1, 2, 3, 4, 5, 6, 7, 8)
      val i = ListIndex(l, loseIndexOnDeletion=false, selectFirstRatherThanNone = true)

      i() = Some(4)
      assert(i() === Some(4))

      l() = List(42)
      assert(i() === Some(0))

      l() = List()
      assert(i() === None)
    }
  }

  "ListIndex with loseIndexOnDeletion true" should {
    "leave selection alone on deletion after selection" in {
      val l = ListVar(0, 1, 2, 3, 4, 5, 6, 7, 8)
      val i = ListIndex(l, loseIndexOnDeletion=true)

      i() = Some(4)
      l.remove(5, 4)
      assert(i() === Some(4))
    }

    "move selection on deletion before selection" in {
      val l = ListVar(0, 1, 2, 3, 4, 5, 6, 7, 8)
      val i = ListIndex(l, loseIndexOnDeletion=true)

      i() = Some(4)
      l.remove(0, 3)
      assert(i() === Some(1))
    }

    "clear selection on delete, if selectFirstRatherThanNone is false" in {
      val l = ListVar(0, 1, 2, 3, 4, 5, 6, 7, 8)
      val i = ListIndex(l, loseIndexOnDeletion=true, selectFirstRatherThanNone=false)

      i() = Some(4)
      l.remove(4, 2)
      assert(i() === None)
    }

    "clear selection then prefer 0, if selectFirstRatherThanNone is true" in {
      val l = ListVar(0, 1, 2, 3, 4, 5, 6, 7, 8)
      val i = ListIndex(l, loseIndexOnDeletion=true, selectFirstRatherThanNone=true)

      i() = Some(4)
      l.remove(4, 2)
      assert(i() === Some(0))
    }

    "change invalid initial selection to valid one" in {
      val l = ListVar(0, 1, 2, 3, 4, 5, 6, 7, 8)
      val i = ListIndex(l, Some(100), loseIndexOnDeletion=true)

      assert(i() === Some(8))
    }

    "clear selection on list change, if selectFirstRatherThanNone is false" in {
      val l = ListVar(0, 1, 2, 3, 4, 5, 6, 7, 8)
      val i = ListIndex(l, loseIndexOnDeletion=true, selectFirstRatherThanNone = false)

      i() = Some(4)
      assert(i() === Some(4))

      l() = List(42)
      assert(i() === None)
    }

    "clear selection then prefer 0 on list change for non-empty list, still None for empty list, if selectFirstRatherThanNone is true" in {
      val l = ListVar(0, 1, 2, 3, 4, 5, 6, 7, 8)
      val i = ListIndex(l, loseIndexOnDeletion=true, selectFirstRatherThanNone = true)

      i() = Some(4)
      assert(i() === Some(4))

      l() = List(42)
      assert(i() === Some(0))

      l() = List()
      assert(i() === None)
    }
  }

}