package boxes.demo

import java.awt.event.ActionEvent
import boxes.util.{LogStep, Step, CoalescingResponder, NumericClass}
import boxes._
import general.{RadioReaction, SetOp}
import graph._
import list._
import persistence._
import java.io.{ByteArrayInputStream, ByteArrayOutputStream, StringWriter}
import java.util.concurrent.atomic.AtomicBoolean
import java.awt.{Dimension, BorderLayout, GridLayout, Color}
import javax.swing._
import border.EmptyBorder
import boxes.BoxImplicits._
import boxes.swing.{EmbossedLabel, TabBuilder, TabSpacer, SheetBuilder, BoxesDropdownView, SwingButton, GraphSwingBGView, GraphSwingView, SwingButtonBar, SwingOp, SwingBarButton}
import boxes.swing.icons.IconFactory

import com.mongodb.casbah.Imports._

object BoxesDemo {

  class Person extends Node {
    val name = Var("name")
    val age = Var(32)
    val friend:Var[Person] = Var(null)

    //override def toString = name() + ", " + age() + ", friend: " + friend()
  }

  class Sine {
    val name = Var("Sine")
    val phase = Var(0d)
    val amplitude = Var(1d)
    val enabled = Var(true)
    val points = Var(false)
    val description = Var("Default Description\nCan have multiple lines")
  }

  class OptionPerson extends Node {
    val name = Var("name")
    val age = Var(32)
    val friend:Var[Option[OptionPerson]] = Var(None)
    val spouse:Var[Option[OptionPerson]] = Var(None)
    val numbers = Var(List[Int]())
    val accounts = Var(Map[String, Int]())
    val zombie = Var(false)

    override def toString = name() + ", " + age() + ", friend: " + friend() + ", spouse " + spouse() + ", numbers " + numbers() + ", accounts " + accounts() + ", zombie? " + zombie()
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

    //Reaction(doubleX, x() *2d, "doubleX = 2 * x")

    doubleX << x() *2d

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
    val xView = BooleanView(x, Val("Some Text"), BooleanControlType.RADIO)
    val yView = BooleanOptionView(y, Val("Tab"), BooleanControlType.TAB)

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
    val qView = RangeView(q, 0, 10, true)

    val pView2 = NumberView(p, Step(1))
    val qView2 = NumberView(q, Step(1))

    val m = Var(1.0)

    val mView = NumberView(m)
    val nView = NumberView(m, LogStep(100))


    val frame = new JFrame()
    val panel = new JPanel(new GridLayout(1, -1))
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
    println("From class of person: " + Node.accessorsOfClass(classOf[Person]))
    println("From Person: " + Node.accessors(new Person()))
  }

  def data() {
    val s = new StringWriter()
    val a = new ClassAliases
    val d = new XMLDataTarget(s, a)
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

    println(s.toString)
  }

  def code() = {

    val io = XMLIO()
    io.alias(classOf[OptionPerson], "Person")

    val p = new OptionPerson()
    p.accounts() = Map("current" -> 10, "savings" -> 100, "secretswiss" -> 10000000)
    p.numbers() = List(10,20,30)
    p.age() = 100
    val q = new OptionPerson()
    q.numbers() = List(1, 4, 9)
    p.friend() = Some(q)
    p.spouse() = Some(q)
    q.name() = "q"


    val output = new ByteArrayOutputStream()

    io.code(p, output)

    val s = output.toString("UTF-8")

    println("Start of output, " + output.size + " bytes")
    println(s)
    println("End of output")

    s
  }

  def decode(xml:String) = {

    val input = new ByteArrayInputStream(xml.getBytes("UTF-8"))

    val io = XMLIO()
    io.alias(classOf[OptionPerson], "Person")

    val p = io.decode(input).asInstanceOf[OptionPerson]

    println(p)

    p.friend().foreach(_.name() = "qe2")
    println(p)

  }

  def listPath() {
    class ListNode {
      val list = ListVar(0, 1, 2, 3, 4, 5, 6, 7)
    }
    val ln = new ListNode()

    val l = ListPath(ln.list)
//    val i = ListIndex(l)

    println("l() = " + l())
//    l(0) = 42
//    println("l(0) = 42")
    ln.list(0) = 42
    println("ln.list(0) = 42")
    println("l() = " + l())
    println("ln.list() = " + ln.list())
  }

  def ledger() {

    val p = new OptionPerson()
    p.name() = "p"
    val q = new OptionPerson()
    q.name() = "q"

    val list = ListVar(p, q, q, p)

    val index = ListIndex(list)

    val view = LensRecordView[OptionPerson](
      VarLens("Name", _.name),
      VarLens("Age", _.age),
      VarLens("Zombie?", _.zombie)
    )

    val ledger = new ListLedger(list, view)

    ledger.update(0, 1, 42)

    for (f <- 0 until ledger.fieldCount) {
      print(ledger.fieldName(f) + "\t")
    }
    println()
    for (f <- 0 until ledger.fieldCount) {
      print(ledger.fieldClass(f) + "\t")
    }
    println()
    for (r <- 0 until ledger.recordCount) {
      for (f <- 0 until ledger.fieldCount) {
        print(ledger(r, f) + "\t")
      }
      println()
    }

    val ledgerRef = Var(ledger)

    val ledgerView = LedgerView.singleSelection(ledgerRef, index)

    val indexView = NumberOptionView(index, Step(1))

    val add = new JButton(new AbstractAction("Add") {
      override def actionPerformed(e:ActionEvent) = {
        val person = new OptionPerson()
        person.name() = "New item at " + list().size
        list.insert(list().size, person)
      }
    })

    val delete = new JButton(new AbstractAction("Delete") {
      override def actionPerformed(e:ActionEvent) = {
        if (!list().isEmpty) list.remove(0, 1)
      }
    })

    val frame = new JFrame()
    val panel = new JPanel()
    panel.add(add)
    panel.add(delete)
    panel.add(indexView.component)
    frame.add(new JScrollPane(ledgerView.component), BorderLayout.CENTER)
    frame.add(panel, BorderLayout.SOUTH)
    frame.pack
    frame.setMinimumSize(new Dimension(300, 50))
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    frame.setVisible(true)

  }





  def fieldCompositeLedger() {

    val p = new OptionPerson()
    p.name() = "p"
    val q = new OptionPerson()
    q.name() = "q"

    val list = ListVar(p, q, q, p)

    val index = ListIndex(list)

    val viewNA = LensRecordView[OptionPerson](
      VarLens("Name", _.name),
      VarLens("Age", _.age)
    )
    val ledgerNA = new ListLedger(list, viewNA)

    val viewZ = LensRecordView[OptionPerson](
      VarLens("Zombie?", _.zombie)
    )
    val ledgerZ = new ListLedger(list, viewZ)

    val ledger = new FieldCompositeLedger(ledgerNA, ledgerZ)

    ledger.update(0, 1, 42)

    for (f <- 0 until ledger.fieldCount) {
      print(ledger.fieldName(f) + "\t")
    }
    println()
    for (f <- 0 until ledger.fieldCount) {
      print(ledger.fieldClass(f) + "\t")
    }
    println()
    for (r <- 0 until ledger.recordCount) {
      for (f <- 0 until ledger.fieldCount) {
        print(ledger(r, f) + "\t")
      }
      println()
    }

    val ledgerRef = Var(ledger)

    val ledgerView = LedgerView.singleSelection(ledgerRef, index)

    val indexView = NumberOptionView(index, Step(1))

    val add = new JButton(new AbstractAction("Add") {
      override def actionPerformed(e:ActionEvent) = {
        val person = new OptionPerson()
        person.name() = "New item at " + list().size
        list.insert(list().size, person)
      }
    })

    val delete = new JButton(new AbstractAction("Delete") {
      override def actionPerformed(e:ActionEvent) = {
        if (!list().isEmpty) list.remove(0, 1)
      }
    })

    val frame = new JFrame()
    val panel = new JPanel()
    panel.add(add)
    panel.add(delete)
    panel.add(indexView.component)
    frame.add(new JScrollPane(ledgerView.component), BorderLayout.CENTER)
    frame.add(panel, BorderLayout.SOUTH)
    frame.pack
    frame.setMinimumSize(new Dimension(300, 50))
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    frame.setVisible(true)

  }

  def ledgerMulti() {

    val frame = new JFrame()

    val panel = new JPanel(new GridLayout(2, 2, 1, 1))
    panel.setBackground(SwingView.dividingColor)


    val stuff = buildLedgerMulti()
    panel.add(stuff._1)
    panel.add(buildGraphPanel(stuff._2, stuff._3))


    val stuff2 = buildLedgerMulti()
    panel.add(buildGraphPanel(stuff2._2, stuff2._3))

    panel.add(stuff2._1)

    frame.add(panel)

    frame.pack
    frame.setMinimumSize(new Dimension(50, 50))
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    frame.setVisible(true)

  }



  def buildGraphPanel(sines: ListVar[Sine], indices:Var[Set[Int]]) = {

    val selectEnabled = Var(false)
    val zoomEnabled = Var(true)
    val grabEnabled = Var(false)
    val manualBounds = Var(None:Option[Area])
    RadioReaction(selectEnabled, zoomEnabled, grabEnabled)

    val series = Cal{
      sines().zipWithIndex.map(v => {
        val s = v._1
        val i = v._2
        Series(i,
          if (s.enabled()) Range(0, 100).map(x => x/100d).map(x => Vec2(x, math.sin((x + s.phase()) * 2 * 3.1415) * s.amplitude())).toList else List[Vec2](),
          Color.getHSBColor((9-i)/14f, 1f, 1f),
          2
        )
      }).toList
    }

    import boxes.graph.Axis._


    val x = Var(0.5d)
    val xThreshold = GraphThreshold(X, x, Color.blue, "X Threshold", true)

    val y = Var(0.5d)
    val yThreshold = GraphThreshold(Y, y, Color.red, "Y Threshold", true)

    val graph = Var (
      GraphBasic.withSeries (
        ColorSeriesBySelection(series, indices),
        xName = "X (Time)",
        yName = "Y (Intensity)",
        zoomEnabled = zoomEnabled,
        manualBounds = manualBounds,
        selectEnabled = selectEnabled,
        selection = indices,
        grabEnabled = grabEnabled,
        extraOverLayers = List(xThreshold, yThreshold)
      )
    )

    val v = GraphSwingBGView(graph)

    //Zoom out by clearing manual bounds to None
    val zoomOutButton = SwingBarButton(SwingOp("", Some(GraphSwingView.zoomOut), SetOp(manualBounds, None:Option[Area])))

    val zoomEnabledView = BooleanView(zoomEnabled, "", BooleanControlType.TOOLBARBUTTON, Some(GraphSwingView.zoomSelect), false)
    val selectEnabledView = BooleanView(selectEnabled, "", BooleanControlType.TOOLBARBUTTON, Some(GraphSwingView.boxSelect), false)

    val grabEnabledView = BooleanView(grabEnabled, "", BooleanControlType.TOOLBARBUTTON, Some(GraphSwingView.move), false)

    val buttons = SwingButtonBar()
                    .add(selectEnabledView)
                    .add(grabEnabledView)
                    .add(zoomEnabledView)
                    .add(zoomOutButton)
                  .buildWithListStyleComponent(EmbossedLabel("Demo Graph"))

    val panel = new JPanel(new BorderLayout())
    panel.add(v.component, BorderLayout.CENTER)

    panel.add(buttons, BorderLayout.SOUTH)

    panel
  }

  def swingRun(r : => Unit) {
    SwingUtilities.invokeLater(new Runnable(){
      override def run = r
    })
  }


  def ticks() {
    println(Ticks((0.05, 1.05), 100, 10))
  }


  def backgroundReaction() = {
    val s = Var("S")
    val t = Var{""}

    val p = Var{0d}

    //Note that we keep a reference so the reaction isn't GCed
    val br = BackgroundReaction{

      val sval = s()  //We store the actual value (String) here, so that we DON'T read the Var s in our background function

      //This is the background function - it uses the stored String value sval, and so doesn't read any Box state
      (cancel:AtomicBoolean) => {

        //Create an artificial delay
        Range(0, 100, 1).foreach(pc => {
          val n = pc/100d
          p() = n     //Update our progress var, to give visual feedback via PieView
          if (!cancel.get()) Thread.sleep(10) //Use cancel to terminate early if requested
        })
        p() = 1

        //TODO implement test to confirm that doing this (reading s()) causes an exception
        //println(s())

        if (!cancel.get()) t() = sval + "_T"  //If cancelled, don't make the update. Note - in some cases you might
                                              // want to make a partial update, it depends on the purpose of the reaction.
                                              // For example, a fractal rendering reaction might display a lower res partial result.
      }
    }

    val sView = StringView(s)
    val tView = StringView(t)
    val pView = PieView(p, Cal{math.min(1, 7*(0.5 - math.abs(p() - 0.5)))})


    val frame = new JFrame()
    val panel = new JPanel(new GridLayout(2, 1))
    panel.add(sView.component)
    panel.add(tView.component)
    frame.add(panel, BorderLayout.CENTER)
    frame.add(pView.component, BorderLayout.EAST)
    frame.pack
    frame.setMinimumSize(new Dimension(300, 50))
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    frame.setVisible(true)
  }

  def ledgerAndSelected() {

    val people = Range(0, 10).map(i => {
      val p = new OptionPerson()
      p.name() = "Mr " + i
      p.age() = i * 10 + 5
      p
    })

    val list = ListVar(people:_*)

    val index = ListIndex(list)

    val view = LensRecordView[OptionPerson](
      VarLens("Name", _.name),
      VarLens("Age", _.age),
      VarLens("Zombie?", _.zombie)
    )

    val ledger = Var(new ListLedger(list, view))

    val ledgerView = LedgerView.singleSelectionScroll(ledger, index)

    val indexView = NumberOptionView(index, Step(1))

    val add = SwingButton("Add", None, Op{
      val person = new OptionPerson()
      person.name() = "New item at " + list().size
      list.insert(list().size, person)
    })

    val delete = SwingButton("Delete", None,
      Op(if (!list().isEmpty) list.remove(0, 1), Cal(!list().isEmpty))
    )

    val selected = ListSelection(list, index)

    val nameView = StringOptionView(PathViaOption{
      for {
        p <- selected()
      } yield p.name
    })

    val age = PathViaOption{
      for {
        p <- selected()
      } yield p.age
    }
    val ageView = NumberOptionView[Int](age)

    val zombie = PathViaOption{
      for {
        p <- selected()
      } yield p.zombie
    }
    val zombieView = BooleanOptionView(zombie)

    val personDropdown = new BoxesDropdownView(ledger, index)

    val frame = new JFrame()
    val panel = new JPanel()
    panel.add(add)
    panel.add(delete)

    panel.add(Label("Index:"))
    panel.add(indexView.component)

    panel.add(Label("Name:"))
    panel.add(nameView.component)

    panel.add(Label("Age:"))
    panel.add(ageView.component)

    panel.add(Label("Zombie:"))
    panel.add(zombieView.component)

    panel.add(Label("Henchman:"))
    panel.add(personDropdown.component)


    panel.setBorder(new EmptyBorder(5, 5, 5, 5))
    frame.add(ledgerView.component, BorderLayout.CENTER)
    frame.add(panel, BorderLayout.SOUTH)
    frame.pack
    frame.setMinimumSize(new Dimension(300, 50))
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    frame.setVisible(true)

  }

  def sheetBuilder() {

    val p = new OptionPerson()
    p.name() = "Bob"
    p.age() = 55

    val nameView = StringView(p.name)

    val ageView = NumberView[Int](p.age)

    val zombieView = BooleanView(p.zombie)

    val frame = new JFrame()

    val sheet = SheetBuilder()
    val panel = sheet separator("Edit Bob") view("Name", nameView) view("Age", ageView) view("Zombie", zombieView) panel

    frame.add(panel)
    frame.pack
    frame.setMinimumSize(new Dimension(300, 50))
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    frame.setVisible(true)

  }


  def buildLedgerMulti() = {

    val list = ListVar(Range(0, 10).map(i=>{
      val s = new Sine
      s.name() = "Sine " + i
      s.phase() = i/40d
      s.amplitude() = 1
      s
    }):_*)

    val view = LensRecordView[Sine](
      VarLens("Name", _.name),
      VarLens("Phase", _.phase),
      VarLens("Amplitude", _.amplitude),
      VarLens("Enabled", _.enabled)
    )

    val indices = ListIndices(list, defaultSelection = DefaultSelection.FirstIndex)

    val ledger = Var(ListLedger(list, view))

    val ledgerView = LedgerView.multiSelectionScroll(ledger, indices, sorting=true)

    val add = new ListMultiAddOp(list, indices, Some(new Sine()))

    val delete = new ListMultiDeleteOp[Sine](list, indices, t=>Unit)

    val up = new ListMultiMoveOp[Sine](list, indices, true)

    val down = new ListMultiMoveOp[Sine](list, indices, false)

    val buttons = SwingButtonBar().add(add).add(delete).add(up).add(down).buildWithListStyleComponent(EmbossedLabel("Sine Table"))

    val firstSelected = Cal{
      val is = indices()
      if (is.isEmpty) {
        None
      } else {
        val min = is.min
        if (min >= 0 && min < list().size) {
          Some(list(min))
        } else {
          None
        }
      }
    }

    val mainPanel = new JPanel(new BorderLayout())
    mainPanel.add(ledgerView.component, BorderLayout.CENTER)
    mainPanel.add(buttons, BorderLayout.SOUTH)

    (mainPanel, list, indices, firstSelected)
  }

  def tabs() {

    val graphIcon = IconFactory.icon("GraphTab")
    val tableIcon = IconFactory.icon("TableTab")
    val propertiesIcon = IconFactory.icon("PropertiesTab")

    val frame = new JFrame()

    val stuff = buildLedgerMulti()
    val table = stuff._1
    val graph = buildGraphPanel(stuff._2, stuff._3)

    val sine = stuff._4

    val nameView = StringOptionView(for (s <- sine()) yield s.name)
    val amplitudeView = NumberOptionView(for (s <- sine()) yield s.amplitude)
    val phaseView = NumberOptionView(for (s <- sine()) yield s.phase)
    val enabledView = BooleanOptionView(for (s <- sine()) yield s.enabled)
    val descriptionView = StringOptionView(for (s <- sine()) yield s.description, true)

    val sheet = SheetBuilder()
    val properties = sheet
                      .separator("Edit Sine")
                      .view("Name", nameView)
                      .view("Amplitude", amplitudeView)
                      .view("Phase", phaseView)
                      .view("Enabled", enabledView)
                      .view("Description", descriptionView, true)
                     .panel()

    val tabs =
      TabBuilder()
        .add(graph,       "Graph",  Some(graphIcon))
        .add(table,       "Table",  Some(tableIcon))
        .add(properties,  "Edit",   Some(propertiesIcon))
      .panel()

//    val tabs = TabBuilder()
//      .add(new JLabel("Plus"), Val("Plus"))
//      .add(new JLabel("Minus"), Val("Minus"))
//      .add(new JLabel("Up"), Val("Up"))
//      .add(new JLabel("Down"), Val("Down"))
//      .panel(64, 32)

    frame.add(tabs)

    frame.pack
    frame.setMinimumSize(new Dimension(50, 50))
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    frame.setVisible(true)

  }


  def textArea() {

    val frame = new JFrame()

    val name = Var("Name")

    val nameView = StringView(name, true)

    frame.add(nameView.component)

    frame.pack
    frame.setMinimumSize(new Dimension(50, 50))
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    frame.setVisible(true)

  }

  def main(args: Array[String]) {
//    simpleCalc
//    simplePath
    separateBIDIReactions
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
//    data

//    val xml = code
//    decode(xml)

//    listPath
//    ledger
    //ledgerMulti
//    ledger
//    fieldCompositeLedger

//    val l = List(1, 2, 3)
//    val l2 = l ::: List(4)
//    println(l2)

    swingRun{
//      SwingView.nimbus()
//      backgroundReaction
//      textViews
      ledgerMulti

//      ledgerAndSelected

//      sheetBuilder
//      tabs
//      textArea()
    }
//    axis
  }

}