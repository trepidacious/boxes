package boxes.javafx

import boxes.BooleanControlType._
import scala.collection._
import java.text.DecimalFormat
import boxes.util.CoalescingResponder
import javafx.application.Platform
import javax.swing.JComponent
import boxes.util.GConverter
import boxes.util.TConverter
import boxes.util.OptionTConverter
import boxes.swing.SwingView
import boxes.Box
import boxes.View
import scalafx.scene.Node
import scalafx.scene.control.Label
import boxes.swing.BoxesScrollBarUI
import boxes.VarBox
import boxes.swing.BoxesTextFieldUI
import boxes.swing.BoxesTextAreaUI
import scalafx.scene.control.TextField
import scalafx.scene.control.TextArea
import scalafx.scene.control.ScrollPane
import javafx.scene.input.KeyEvent
import javafx.beans.value.ChangeListener
import javafx.beans.value.ObservableValue
import javafx.event.EventHandler
import javafx.scene.input.KeyCode
import scalafx.event.EventHandler._
import boxes.javafx.JFXImplicits._
import boxes.BooleanControlType._
import boxes.Val
import scalafx.scene.control.CheckBox
import boxes.util.NumericClass
import javafx.event.ActionEvent
import boxes.util.Sequence


//object SwingViewImplicits {
//  implicit def varToBooleanView(v : Var[Boolean]) = BooleanView(v)
//  implicit def varToNumberView[N](v : Var[N])(implicit n:Numeric[N], nc:NumericClass[N]) = NumberView[N](v)
//  implicit def varToStringView(v : Var[String]) = StringView(v)
//
//  implicit def optionVarToBooleanView(v : Var[Option[Boolean]]) = BooleanOptionView(v)
//  implicit def optionVarToNumberView[N](v : Var[Option[N]])(implicit n:Numeric[N], nc:NumericClass[N]) = NumberOptionView[N](v)
//  implicit def optionVarToStringView(v : Var[Option[String]]) = StringOptionView(v)
//}

object JFXView {

  val defaultDecimalFormat = new DecimalFormat("0.#")
  val viewToUpdates = new mutable.WeakHashMap[Any, mutable.ListBuffer[() => Unit]]()
  val responder = new CoalescingResponder(respond)
  val lock = new Object()
  
  def format(d:Double) = defaultDecimalFormat.format(d)

  def addUpdate(v:Any, update: => Unit) = {
    lock.synchronized{
      viewToUpdates.get(v) match {
        case None => viewToUpdates.put(v, mutable.ListBuffer(() => update))
        case Some(list) => list.append(() => update)
      }
      responder.request
    }
  }

  def replaceUpdate(v:Any, update: => Unit) = {
    lock.synchronized{
      viewToUpdates.put(v, mutable.ListBuffer(() => update))
      responder.request
    }
  }

  private def respond = jfxRun {
    while(
      popUpdates match {
        case Some(updates) => {
          for {
            update <- updates
          } update.apply
          true
        }
        case None => false
      }
    ) {}
  }


  /**
   * If there are any updates stored in map, get a list of updates
   * for some key, remove them from the map, and return them.
   * If there are no updates left (no keys), then return None
   * This is synchronized, so updates can't be added as they are being
   * retrieved
   */
  private def popUpdates = {
    lock.synchronized{
      val keysIt = viewToUpdates.keysIterator;
      if (keysIt.hasNext) {
        val r = viewToUpdates.remove(keysIt.next)
        if (r == None) throw new RuntimeException("Got None for a key in viewToUpdates")
        r
      } else {
        None
      }
    }
  }
  
  def jfxRun(r : => Unit) {
    Platform.runLater(new Runnable(){
      override def run = r
    })
  }

}

trait JFXView {
  def node():Node

  private[boxes] def addUpdate(update: => Unit) = JFXView.addUpdate(this, update)
  private[boxes] def replaceUpdate(update: => Unit) = JFXView.replaceUpdate(this, update)
}

object LabelView {
  def apply(v:Box[String,_]) = new LabelOptionView(v, new TConverter[String]).asInstanceOf[JFXView]
}

object LabelOptionView {
  def apply(v:Box[Option[String],_]) = new LabelOptionView(v, new OptionTConverter[String]).asInstanceOf[JFXView]
}

//TODO use a renderer to customise display
private class LabelOptionView[G](v:Box[G,_], c:GConverter[G, String]) extends JFXView {

  val node = new LinkingLabel(this)

  val view = View{
    //Store the value for later use on Swing Thread
    val newV = v()
    //This will be called from Swing Thread
    replaceUpdate {display(newV)}
  }

  //Update display if necessary
  private def display(s:G) {
    val text = c.toOption(s).getOrElse("")
    if (!node.getText.equals(text)) {
      node.setText(text)
    }
  }
}

//Special versions of components that link back to the JFXView using them,
//so that if users only retain the component, they still also retain the JFXView.
class LinkingLabel(val sv:JFXView) extends Label {}

object StringView {
  def apply(v:VarBox[String,_], multiline:Boolean = false) = new StringOptionView(v, new TConverter[String], multiline).asInstanceOf[JFXView]
}

object StringOptionView {
  def apply(v:VarBox[Option[String],_], multiline:Boolean = false) = new StringOptionView(v, new OptionTConverter[String], multiline).asInstanceOf[JFXView]
}

private class StringOptionView[G](v:VarBox[G,_], c:GConverter[G, String], multiline:Boolean) extends JFXView {

  val text = if (multiline) new LinkingTextArea(this) else new LinkingTextField(this)
  val node = if (multiline) {
    val s = new ScrollPane()
    s.setContent(text)
    s
  } else {
    text
  }

  {
    text.focusedProperty.addListener((focused: java.lang.Boolean) => if (focused) display(v()) else commit)
    text match {
      case tf: TextField => tf.setOnAction((event: ActionEvent) => commit)
      case t => t.setOnKeyPressed((event: KeyEvent) => if (event.getCode == KeyCode.ENTER) commit)
    }
  }

  val view = View{
    //Store the value for later use on Swing Thread
    val newV = v()
    //This will be called from Swing Thread
    replaceUpdate {display(newV)}
  }

  private def commit = {
    v() = c.toG(text.getText)
  }

  //Update display if necessary
  private def display(s:G) {
    val (e, t) = c.toOption(s).map((true, _)).getOrElse(false, "")
    text.setDisable(!e)
    if (!text.getText.equals(t)) text.setText(t)
  }
}

object NumberView {
  def apply[N](v:VarBox[N,_])(implicit n:Numeric[N], nc:NumericClass[N]):JFXView = apply(v, nc.defaultSequence)
  def apply[N](v:VarBox[N,_], s:Sequence[N])(implicit n:Numeric[N], nc:NumericClass[N]) = new NumberOptionView(v, new TConverter[N], s, n, nc): JFXView
}

object NumberOptionView {
  def apply[N](v:VarBox[Option[N],_])(implicit n:Numeric[N], nc:NumericClass[N]):JFXView = apply(v, nc.defaultSequence)
  def apply[N](v:VarBox[Option[N],_], s:Sequence[N])(implicit n:Numeric[N], nc:NumericClass[N]):JFXView = new NumberOptionView(v, new OptionTConverter[N], s, n, nc): JFXView
}

private class NumberOptionView[G, N](v:VarBox[G,_], c:GConverter[G, N], s:Sequence[N], n:Numeric[N], nc:NumericClass[N]) extends JFXView {

  val node = new LinkingTextField(this);
  {
    node.setOnAction((event: ActionEvent) => commit)
    node.focusedProperty.addListener((focused: java.lang.Boolean) => if (focused) display(v()) else commit)
  }

  val view = View{
    //Store the value for later use on Swing Thread
    val newV = v()
    //This will be called from Swing Thread
    replaceUpdate {display(newV)}
  }

  private def commit = {
    nc.parseOption(node.getText) match {
      case Some(n) => v() = c.toG(n)
      case None => display(v())
    }
  }

  //Update display if necessary
  private def display(s:G) {
    val (e, t) = c.toOption(s).map((n: N) => (true, nc.format(n))).getOrElse(false, "")
    node.setDisable(!e)
    if (!node.getText.equals(t)) node.setText(t)
  }
}

class LinkingTextField(val v:JFXView) extends TextField
class LinkingTextArea(val v:JFXView) extends TextArea

object BooleanView {
  def apply(v:VarBox[Boolean,_], n:Box[String,_] = Val(""), controlType:BooleanControlType = SLIDECHECK, icon:Box[Option[Node], _] = Val(None), toggle:Boolean = true) = new BooleanOptionView(v, n, new TConverter[Boolean], controlType, icon, toggle).asInstanceOf[JFXView]
}

object BooleanOptionView {
  def apply(v:VarBox[Option[Boolean],_], n:Box[String,_] = Val(""), controlType:BooleanControlType = SLIDECHECK, icon:Box[Option[Node], _] = Val(None), toggle:Boolean = true) = new BooleanOptionView(v, n, new OptionTConverter[Boolean], controlType, icon, toggle).asInstanceOf[JFXView]
}

private class BooleanOptionView[G](v:VarBox[G,_], n:Box[String,_], c:GConverter[G, Boolean], controlType:BooleanControlType, icon:Box[Option[Node], _], toggle:Boolean = true) extends JFXView {

  val node = new LinkingCheckBox(this)
//    controlType match {
//    case CHECKBOX => new LinkingJCheckBox(this)
//    case RADIO => new LinkingJRadioButton(this)
//    case TOGGLEBUTTON => new LinkingJToggleButton(this)
//    case TOOLBARBUTTON => new LinkingToolbarToggleButton(this)
//    case SLIDECHECK => new LinkingSlideCheckButton(this)
//    case TAB => new LinkingTabButton(this)
//  }

  
//  private val model = new AutoButtonModel()
//
//  {
//    component.setModel(model)
//    component.addActionListener(new ActionListener(){
//      //On action, toggle value if it is not None
//      override def actionPerformed(e:ActionEvent) = {
//        c.toOption(v()) match {
//          case None => None
//          case Some(b) => v() = if (toggle) c.toG(!b) else c.toG(true)
//        }
//      }
//    })
//  }
//
  
  //TODO how do we handle toggle = false?
  node.selectedProperty.addListener((selected: java.lang.Boolean) => {
    if (toggle) {
      v() = c.toG(selected)
    } else {
      if (selected) {
        v() = c.toG(selected)
      } else {
        val newV = v()
        val newN = n()
        val newIcon = icon()
        //This will be called from Swing Thread
        replaceUpdate { display(newV, newN, newIcon) } 
      }
    }
  })
  
  val view = View{
    //Store the values for later use on Swing Thread
    val newV = v()
    val newN = n()
    val newIcon = icon()
    //This will be called from Swing Thread
    replaceUpdate { display(newV, newN, newIcon) }
  }

  //Update display if necessary
  private def display(newV:G, newN:String, newIcon:Option[Node]) {
    c.toOption(newV) match {  
      case None => {
        node.setDisable(true)
        node.setSelected(false)
      }
      case Some(b) => {
        node.setDisable(false)
        node.setSelected(b)
      }
    }
    
    if (newN != node.getText) {
      node.setText(newN)
    }
//    val iconOrNull = newIcon.getOrElse(null)
//    if (iconOrNull != component.getIcon) {
//      component.setIcon(iconOrNull)
//    }
  } 

}

class LinkingCheckBox(val v:JFXView) extends CheckBox

