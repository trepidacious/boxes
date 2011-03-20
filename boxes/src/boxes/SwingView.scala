package boxes

import scala.collection._
import util.CoalescingResponder
import javax.swing.SwingUtilities

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

}