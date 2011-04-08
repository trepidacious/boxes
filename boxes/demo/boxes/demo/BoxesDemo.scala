/*
 * Created by IntelliJ IDEA.
 * User: trepidacious
 * Date: 22/02/2011
 * Time: 20:37
 */
package boxes.demo

import java.awt.Dimension
import javax.swing._
import java.awt.event.ActionEvent
import boxes.util.{LogStep, Step, CoalescingResponder, NumericClass}
import boxes._
import persistence.{CodecByClass, XMLDataTarget, NodeAccessors, Node}

object BoxesDemo {

  class Person extends Node {
    val name = Var("name")
    val age = Var(32)
    val friend:Var[Person] = Var(null)

    //override def toString = name() + ", " + age() + ", friend: " + friend()
  }

  class OptionPerson extends Node {
    val name = Var("name")
    val age = Var(32)
    val friend:Var[Option[OptionPerson]] = Var(None)
    val numbers = Var(List[Int]())

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

    val bobsFriendsName = PathViaOption(
      for {
        friend <- bob.friend()
      } yield friend.name
    )

    println("Before change: " + bobsFriendsName())

    bob.friend() = Some(alice)

    println("After change: " + bobsFriendsName())

    val bobsFriendsFriend = PathToOption(
      for {
        friend <- bob.friend()
      } yield friend.friend
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
        val sView = StringView(s)
        val tView = StringView(t)

        val x = Var(true)
        val y:Var[Option[Boolean]] = Var(Some(false))
        Reaction(x, {
          y() match {
            case None => {
              println("y is None, should set x true")
              true
            }
            case Some(b) => {
              println("y is Some(" + b + "), should set x " + !b)
              !b
            }
          }
        })
        Reaction(y, {
          println("x is " + x() + " should set y to " + !x())
          Some(!x())
        })
        val xView = BooleanView(x, Val("X"))
        val yView = BooleanOptionView(y, Val("Y"))

        val button = new JButton(new AbstractAction() {
          override def actionPerformed(e:ActionEvent) = {
            println("Reactions targeting x:")
            x.targetingReactions.foreach{r => println(r)}
            println("Reactions targeting y:")
            y.targetingReactions.foreach{r => println(r)}
          }
        })

        val p = Var(10)
        val q = Var(0)
        Reaction(p, 10-q())
        Reaction(q, 10-p())
        val pView = RangeView(p, 0, 10)
        val qView = RangeView(q, 0, 10)

        val pView2 = NumberView(p, Step(1))
        val qView2 = NumberView(q, Step(1))

        val m = Var(1.0)

        val mView = NumberView(m)
        val nView = NumberView(m, LogStep(100))


        val frame = new JFrame()
        val panel = new JPanel()
        panel.add(sView.component)
        panel.add(tView.component)
        panel.add(xView.component)
        panel.add(yView.component)
        panel.add(pView.component)
        panel.add(qView.component)
        panel.add(pView2.component)
        panel.add(qView2.component)
        panel.add(mView.component)
        panel.add(nView.component)
        panel.add(button)
        frame.add(panel)
        frame.pack
        frame.setMinimumSize(new Dimension(300, 50))
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
        frame.setVisible(true)
      }
    })
  }

  def sequences() {
    val tensInt = Step(10)
    val halvesDouble = Step(0.5)

    println(tensInt.next(1))
    println(halvesDouble.next(0.1))

    val logTenths = LogStep(10)
    println(logTenths.next(0.1))
    println(logTenths.next(0.2))
    println(logTenths.next(0.99))
    println(logTenths.next(1))
    println(logTenths.next(1.01))
    println(logTenths.next(2))
    println(logTenths.next(10))
    println(logTenths.next(10.1))
    println(logTenths.next(200))
  }

  def printNumericClass[N](number:N)(implicit nc:NumericClass[N]) {
    println(nc.numericClass)
  }

  def numericClass() {
    printNumericClass(0.1)
    printNumericClass(1)
  }

  def codecAccessors() {
    println("From class of person: " + NodeAccessors.accessorsOfClass(classOf[Person]))
    println("From Person: " + NodeAccessors.accessors(new Person()))
  }

  def data() {
    val d = new XMLDataTarget
    d.openTag("Person")
    d.openTag("Name")
    d.putUTF("Bob")
    d.closeTag
    d.openTag("Address")
    d.openTag("Street")
    d.putUTF("One Way Street")
    d.closeTag
    d.closeTag
    d.closeTag
  }

  def codec() {
//    val d = new XMLDataTarget
//    val p = new OptionPerson
//    val codec = DefaultCodecs.NodeCodec
//    codec.code(p, d)
//    val p = new Person()
//    System.out.println(p.age().asInstanceOf[AnyRef].getClass)
    val codec = new CodecByClass()
    val target = new XMLDataTarget
    target.alias(classOf[OptionPerson], "Person")
    val p = new OptionPerson()
    val q = new OptionPerson()
    q.numbers()=List(1, 4, 9)
    p.friend() = Some(q)
    q.name() = "q"
    codec.code(p, target)
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
//    optionPath
//    textViews
//    sequences
//    numericClass
//    codecAccessors
    codec
//    data
  }

}