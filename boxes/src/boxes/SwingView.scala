package boxes

import scala.collection._
import util.CoalescingResponder
import java.awt.event.{FocusEvent, FocusListener, ActionEvent, ActionListener}
import java.awt.Component
import javax.swing._

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
          someUpdates match {
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
  private def someUpdates = {
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

  def addUpdate(update: => Unit) = SwingView.addUpdate(this, update)
  def replaceUpdate(update: => Unit) = SwingView.replaceUpdate(this, update)
}

class StringView(v:Var[String], multiline:Boolean = false) extends SwingView {

  val text = if (multiline) new JTextArea(10, 20) else new LinkingJTextField(this);
  val comp = if (multiline) new LinkingJScrollPane(this, text) else text;
  val defaultBackground = text.getBackground()

  //Note this is only updated from swing thread, so no need to sync
  var valueAtStartOfEditing:Option[String] = None

  {
    if (!multiline) {
      text.asInstanceOf[JTextField].addActionListener(new ActionListener() {
				override def actionPerformed(e:ActionEvent) = commit
			})
    }

    text.addFocusListener(new FocusListener() {
      override def focusLost(e:FocusEvent) = commit

      override def focusGained(e:FocusEvent) = {
        display(v())
        valueAtStartOfEditing = Some(text.getText())
      }
    })
  }

  //View the value of v, and when it changes,
  //schedule an update to display the new value
  val view = View{
    val newV = v()
    replaceUpdate{
      //If not focused, always display
      if(!text.isFocusOwner) {
        display(newV)

      //If focused, only update if value has actually changed since we started
      } else {
        valueAtStartOfEditing match {
          case None => valueAtStartOfEditing = Some(text.getText)
          case Some(string) => {
            if (!string.equals(newV)) display(newV)
            valueAtStartOfEditing = Some(text.getText)
          }
        }
      }
    }
  }

  private def commit = {
    v() = text.getText
  }

  //Update display if necessary
  private def display(s:String) {
    if (!text.getText.equals(s)) {
      text.setText(s)
    }
  }

  def component() = comp

}

//Special versions of components that link back to the SwingView using them,
//so that if users only retain the component, they still also retain the SwingView.
class LinkingJTextField(sv:SwingView) extends JTextField {}
class LinkingJScrollPane(sv:SwingView, contents:Component) extends JScrollPane(contents) {}