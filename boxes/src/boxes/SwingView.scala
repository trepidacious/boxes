package boxes

import scala.collection._
import util.CoalescingResponder

object SwingView {

  val viewToUpdates = new mutable.WeakHashMap[SwingView, mutable.ListBuffer[() => Unit]]()
  val responder = new CoalescingResponder(respond)
  val lock = new Object()
  val responding = false;

  def addUpdate(v:SwingView, update: => Unit) = {
    lock.synchronized{
      viewToUpdates.get(v) match {
        case None => viewToUpdates.put(v, mutable.ListBuffer(() => update))
        case Some(list) => list.append(() => update)
      }
      request
    }
  }

  private def request = {
    //If we are already responding, we will deal with the new stuff
    //in the same response, so no need to request again
    if (!responding) {
      responder.request
    }
  }

  def replaceUpdate(v:SwingView, update: => Unit) = {
    lock.synchronized{
      viewToUpdates.put(v, mutable.ListBuffer(() => update))
      request
    }
  }

  private def respond = {
    var finished = false
    do {
      lock.synchronized{
        someUpdates match {
          case Some(updates) => {
            for {
              update <- updates
            } update
          }
          case None => finished = true
        }

      }
    } while (!finished)

  }

  /**
   * If there are any updates stored in map, get a list of updates
   * for some SwingView, remove them from the map, and return them.
   * If there are no updates left (no keys), then return None
   * This is synchronized, so updates can't be added as they are being
   * retrieved
   */
  private def someUpdates = {
    val keysIt = viewToUpdates.keysIterator;
    if (keysIt.hasNext) {
      val swingView = keysIt.next
      viewToUpdates.remove(swingView) match {
        case Some(updates) => updates
        case None => throw new RuntimeException("Failed to retrieve any updates for key in viewToUpdates")
      }
    } else {
      None
    }
  }

}

trait SwingView {

}