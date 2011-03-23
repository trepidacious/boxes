/*
 * Created by IntelliJ IDEA.
 * User: trepidacious
 * Date: 22/02/2011
 * Time: 20:37
 */
package boxes.demo

import boxes._
import util.CoalescingResponder
import javax.swing.{JPanel, JFrame, JTextField, SwingUtilities}

object BoxesDemo {

  class Person {
    val name = Var("name")
    val age = Var(32)
    val friend:Var[Person] = Var(null)

    //override def toString = name() + ", " + age() + ", friend: " + friend()
  }

  class OptionPerson {
    val name = Var("name")
    val age = Var(32)
    val friend:Var[Option[OptionPerson]] = Var(None)

    override def toString = name() + ", " + age() + ", friend: " + friend()
  }

  def optionPath() = {

    println()
    println("optionPath")

    val cate = new OptionPerson()
    cate.name() = "Cate"
    val alice = new OptionPerson()
    alice.name() = "Alice"
    val bob = new OptionPerson()
    bob.name() = "Bob"

//    println (for {
//      friend <- bob.friend()
//    } yield friend.name)
//
//    bob.friend() = Some(alice)
//
//    println("bob.friend() = " + bob.friend())
//
//    println (for {
//      friend <- bob.friend()
//    } yield friend.name)

//    val bobsFriendsName = PathWithDefault(
//      for {
//        friend <- bob.friend()
//      } yield friend.name,
//      "NONAME"
//    )

    val bobsFriendsName = PathWithOption(
      for {
        friend <- bob.friend()
      } yield friend.name
    )

    println("Before change: " + bobsFriendsName())

    bob.friend() = Some(alice)

    println("After change: " + bobsFriendsName())

    val bobsFriendsFriend = PathWithDefault(
      for {
        friend <- bob.friend()
      } yield friend.friend,
      None
    )

    println("Before alice has a friend: " + bobsFriendsFriend)

    alice.friend() = Some(cate)

    println("After alice has a friend: " + bobsFriendsFriend)


//    println("About to change cate's name")
//    cate.name() = "Katey"
//
//    println("After changing cate's name")
//
//    println("About to change alice's name")
//    alice.name() = "Alicia"
//
//    println("After changing alice's name: " + bobsFriendsName())
//
//    println("About to change bobsFriendsName")
//    bobsFriendsName() = "Alucard"
//
//    println("After changing bobsFriendsName: " + bobsFriendsName() + ", and alice's name is " + alice.name())


  }


  def simpleCalc() = {

    println()
    println("simpleCalc")

    val a = Var(2)
    val b = Cal(a() + 3)
    val c = Cal(a() + b())

    println("b = " + b() + ", c = " + c())

    a() = 4

    println("b = " + b() + ", c = " + c())

  }

  def simplePath() = {

    println()
    println("simplePath")

    val cate = new Person()
    cate.name() = "Cate"
    val alice = new Person()
    alice.name() = "Alice"
    val bob = new Person()
    bob.name() = "Bob"
    bob.friend() = cate

    val bobsFriendsName = Cal(bob.friend().name())

    //FIXME implement test - should see no changes to bobsFriendsName when something not
    //in the path changes, even if it is deeply referenced, or used to be part of path, etc.
    //Reaction(bobsFriendsName, {bob.friend().name()})

    println("Before change: " + bobsFriendsName())

    bob.friend() = alice

    println("After change: " + bobsFriendsName())

    println("About to change cate's name")
    cate.name() = "Katey"

    println("After changing cate's name")

    println("About to change alice's name")
    alice.name() = "Alicia"

    println("After changing alice's name: " + bobsFriendsName())
  }

  def separateBIDIReactions = {

    println()
    println("separateBIDIReactions")

    val x = Var(2d)
    val doubleX = Var(0d)

    println("x = " + x() + ", doubleX = " + doubleX())

    println("About to create reaction doubleX = 2 * x")
    Reaction(doubleX, x()*2d, "doubleX = 2 * x")
    println("Created reaction doubleX = 2 * x")
    println("x = " + x() + ", doubleX = " + doubleX())
    println("About to create reaction x = doubleX / 2")
    Reaction(x, doubleX()/2d, "x = doubleX / 2")
    println("Created both reactions")

    println("x = " + x() + ", doubleX = " + doubleX())

    println("About to set x() = 4")
    x() = 4
    println("Just set x() = 4")

    println("x = " + x() + ", doubleX = " + doubleX())

    println("About to set doubleX() = 10")
    doubleX() = 10
    println("Just set doubleX() = 10")

    println("x = " + x() + ", doubleX = " + doubleX())
  }

  def conflictingReactions() = {

    println()
    println("conflictingReactions")

    val x = Var(2d)
    val y = Var(0d)

    Reaction(y, x() * 2, "double")

    println("Applied first constraint")

    Reaction(y, x() * 4, "quadruple")

    println("Applied second constraint - should not get here...")
  }

  def nonConflictingReactions() = {

    println()
    println("nonConflictingReactions")

    val x = Var(2d)
    val y = Var(0d)

    Reaction(y, x() * 2, "double")

    println("Applied first constraint")

    Reaction(y, x() * 2, "also double")

    println("Applied second constraint - should get here fine...")
  }

  def bidiPath() = {

    println()
    println("bidiPath")

    val cate = new Person()
    cate.name() = "Cate"
    val alice = new Person()
    alice.name() = "Alice"
    val bob = new Person()
    bob.name() = "Bob"
    bob.friend() = cate

    val bobsFriendsName = Path(bob.friend().name)

    //FIXME implement test - should see no changes to bobsFriendsName when something not
    //in the path changes, even if it is deeply referenced, or used to be part of path, etc.
    //Reaction(bobsFriendsName, {bob.friend().name()})

    println("Before change: " + bobsFriendsName())

    bob.friend() = alice

    println("After change: " + bobsFriendsName())

    println("About to change cate's name")
    cate.name() = "Katey"

    println("After changing cate's name")

    println("About to change alice's name")
    alice.name() = "Alicia"

    println("After changing alice's name: " + bobsFriendsName())

    println("About to change bobsFriendsName")
    bobsFriendsName() = "Alucard"

    println("After changing bobsFriendsName: " + bobsFriendsName() + ", and alice's name is " + alice.name())


  }

  def views() = {

    val alice = new Person()
    alice.name() = "Alice"

    val v = View{
      println(alice.name())
      for {
        changes <- alice.name.changes
        change <- changes
      } print(" " + change)
      println()
    }
//    val v2 = View{println(alice.name())}

    alice.name() = "Alicia"

    println()

    alice.name() = "Alucard"

  }

  def responder() = {
    val responder = new CoalescingResponder(println("Hi!"))
    responder.request
    Thread.sleep(5000)
    responder.request
    responder.request
    responder.request
    responder.request
    responder.request
    responder.request
    responder.request
    Thread.sleep(5000)
  }

  def swingViews() = {

    val alice = new Person()
    alice.name() = "Alice"

    val sv = new SwingView() {
      val c = new JTextField()
      def component = c
      val v = View{
        val name = alice.name()
        println("Got change: " + name)
        replaceUpdate{
          println("SwingView Update Run: " + name + " Swing thread? " + SwingUtilities.isEventDispatchThread )
        }
      }
    }

    println("About to set Alicia")

    alice.name() = "Alicia"

    println("About to set Alucard")

    alice.name() = "Alucard"
    for (x <- 1 to 100) {
      alice.name() = "Alucard " + x
      Thread.sleep(10)
    }

    Thread.sleep(5000)

  }

  def textViews() = {
    SwingUtilities.invokeLater(new Runnable(){
      override def run = {
        val s = Var("S")
        val t = Var{""}
        Reaction(t, s()+"_T")
        val sView = new StringView(s)
        val tView = new StringView(t)

        val frame = new JFrame()
        val panel = new JPanel()
        panel.add(sView.component)
        panel.add(tView.component)
        frame.add(panel)
        frame.pack
        frame.setVisible(true)
      }
    })
  }

  def main(args: Array[String]) {
//    simpleCalc
//    simplePath
//    separateBIDIReactions
//    nonConflictingReactions
//
//    bidiPath

//    conflictingReactions

//    swingViews
//    responder
//    textViews
    optionPath
  }


}