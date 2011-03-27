package boxes

import scala.collection._
import util.CoalescingResponder
import java.awt.event.{FocusEvent, FocusListener, ActionEvent, ActionListener}
import java.awt.Component
import javax.swing._
import javax.swing.JToggleButton.ToggleButtonModel

object SwingView {

  val viewToUpdates = new mutable.WeakHashMap[SwingView, mutable.ListBuffer[() => Unit]]()
  val responder = new CoalescingResponder(respond)
  val lock = new Object()

  def addUpdate(v:SwingView, update: => Unit) = {
    lock.synchronized{
      viewToUpdates.get(v) match {
        case None => viewToUpdates.put(v, mutable.ListBuffer(() => update))
        case Some(list) => list.append(() => update)
      }
      responder.request
    }
  }

  def replaceUpdate(v:SwingView, update: => Unit) = {
    lock.synchronized{
      viewToUpdates.put(v, mutable.ListBuffer(() => update))
      responder.request
    }
  }

  private def respond = {
    SwingUtilities.invokeLater(new Runnable() {
      override def run = {
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
    })
  }

  /**
   * If there are any updates stored in map, get a list of updates
   * for some SwingView, remove them from the map, and return them.
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

}

trait SwingView {
  def component():JComponent

  private[boxes] def addUpdate(update: => Unit) = SwingView.addUpdate(this, update)
  private[boxes] def replaceUpdate(update: => Unit) = SwingView.replaceUpdate(this, update)
}

//Type G may be Option[T] or the bare type T.
//toOption gets us from G to DEFINITELY an Option[T]
//toG gets us from the bare type T to G
//This means we can have a Var[Option[T]] or a Var[T] in
//a view, and use the converter to treat it as a Var[Option[T]]
//in a fairly straight forward way.
trait GConverter[G, T] {
  def toOption(g:G):Option[T]
  def toG(t:T):G
}

//GConverter for where G is just T
class TConverter[T] extends GConverter[T, T] {
  override def toOption(g:T):Option[T] = Some(g)
  override def toG(t:T):T = t
}

//GConverter for where G is Option T
class OptionTConverter[T] extends GConverter[Option[T], T] {
  override def toOption(g:Option[T]):Option[T] = g
  override def toG(t:T):Option[T] = Some(t)
}

object StringView {
  def apply(v:Var[String], multiline:Boolean = false) = new StringOptionView(v, new TConverter[String], multiline).asInstanceOf[SwingView]
}

object StringOptionView {
  def apply(v:Var[Option[String]], multiline:Boolean = false) = new StringOptionView(v, new OptionTConverter[String], multiline).asInstanceOf[SwingView]
}

private class StringOptionView[G](v:Var[G], c:GConverter[G, String], multiline:Boolean) extends SwingView {

  val text = if (multiline) new JTextArea(10, 20) else new LinkingJTextField(this)
  val component = if (multiline) new LinkingJScrollPane(this, text) else text

  {
    if (!multiline) {
      text.asInstanceOf[JTextField].addActionListener(new ActionListener() {
				override def actionPerformed(e:ActionEvent) = commit
			})
    }

    text.addFocusListener(new FocusListener() {
      override def focusLost(e:FocusEvent) = commit
      override def focusGained(e:FocusEvent) = display(v())
    })
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
    val enableAndText = c.toOption(s) match {
      case None => (false, "")
      case Some(string) => (true, string)
    }
    text.setEnabled(enableAndText._1)
    if (!text.getText.equals(enableAndText._2)) {
      text.setText(enableAndText._2)
    }
  }
}

//Special versions of components that link back to the SwingView using them,
//so that if users only retain the component, they still also retain the SwingView.
class LinkingJScrollPane(sv:SwingView, contents:Component) extends JScrollPane(contents) {}
class LinkingJTextField(sv:SwingView) extends JTextField {}


object BooleanView {
  def apply(v:Var[Boolean], n:Ref[String], button:Boolean = false) = new BooleanOptionView(v, n, new TConverter[Boolean], button).asInstanceOf[SwingView]
}

object BooleanOptionView {
  def apply(v:Var[Option[Boolean]], n:Ref[String], button:Boolean = false) = new BooleanOptionView(v, n, new OptionTConverter[Boolean], button).asInstanceOf[SwingView]
}

private class BooleanOptionView[G](v:Var[G], n:Ref[String], c:GConverter[G, Boolean], button:Boolean) extends SwingView {

  val component = if (button) new LinkingJCheckBox(this) else new LinkingJToggleButton(this)
  private val model = new AutoButtonModel()

  {
    component.setModel(model)
    component.addActionListener(new ActionListener(){
      //On action, toggle value if it is not None
      override def actionPerformed(e:ActionEvent) = {
        c.toOption(v()) match {
          case None => None
          case Some(b) => v() = c.toG(!b)
        }
      }
    })
  }

  val view = View{
    //Store the values for later use on Swing Thread
    val newV = v()
    val newN = n()
    //This will be called from Swing Thread
    replaceUpdate { display(newV, newN) }
  }

  //Update display if necessary
  private def display(s:G, newN:String) {
    model.fire
    if (newN != component.getText) {
      component.setText(newN)
    }
  }

  private class AutoButtonModel extends ToggleButtonModel {

    def fire() {
      fireStateChanged();
    }

    override def isSelected = {
      c.toOption(v()) match {
        case None => false
        case Some(b) => b
      }
    }

    override def isEnabled = {
      c.toOption(v()) match {
        case None => false
        case Some(_) => true
      }
    }
  }

}

class LinkingJCheckBox(sv:SwingView) extends JCheckBox {}
class LinkingJToggleButton(sv:SwingView) extends JCheckBox {}

