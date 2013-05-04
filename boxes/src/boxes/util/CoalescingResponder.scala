package boxes.util

import java.util.concurrent.{TimeUnit, ThreadFactory, Executors}

class CoalescingResponder(response: => Unit, fusionInterval:Long = 5, tickInterval:Long = 20) {

  var lastRequestTime = 0L
  var requestPending = false
  var executor = Executors.newSingleThreadScheduledExecutor(new DaemonThreadFactory())
  val lock = new Object()

  {
    executor.scheduleAtFixedRate(
      new Runnable(){
        override def run = respond
      },
      tickInterval, tickInterval,
      TimeUnit.MILLISECONDS
    )
  }

  private def respond = {
    lock.synchronized {
      if (requestPending) {
        response
        requestPending = false
      }
    }
  }

  def request() = {
    lock.synchronized {
      //Work out timing
      val time = System.currentTimeMillis();
      val interval = time - lastRequestTime;
      lastRequestTime = time;

      //Now have a pending request
      requestPending = true;

      //If we can't coalesce with the last request, then respond immediately
      //Otherwise, the regular responder will catch it and respond sometime soon
      if (interval > fusionInterval) {
        respond
      }
    }
	}

}

class DaemonThreadFactory extends ThreadFactory {
  override def newThread(r:Runnable) = {
    val t = Executors.defaultThreadFactory.newThread(r)
    t.setDaemon(true)
    t
  }
}