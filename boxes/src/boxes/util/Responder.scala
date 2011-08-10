package boxes.util

import java.util.concurrent.{Executor, Executors}
import java.util.concurrent.atomic.AtomicBoolean

object Responder {
	val defaultExecutorService = Executors.newFixedThreadPool(4, new DaemonThreadFactory());

  def apply(executor:Executor = defaultExecutorService) = new Responder(executor)
}

class Responder(val executor:Executor) {

  private val responseLock = new Object()
  private var nextResponse:Option[(AtomicBoolean => Unit)] = None //The next response function, if one is pending
  private var currentlyRunning = false
  private val shouldCancel = new AtomicBoolean(false)

  def request(response: (AtomicBoolean => Unit)) {
    responseLock.synchronized {
      nextResponse = Some(response)

      //Cancel current response, and make sure another runs immediately afterwards
      if (currentlyRunning) {
        shouldCancel.set(true)

      //If we are not running, then we can service the update immediately
      } else {
        launchNewResponse()
      }
    }
  }

  //If required, schedule a new response
  private def launchNewResponse() {
    responseLock.synchronized {
      nextResponse.foreach(response => {
        nextResponse = None
        shouldCancel.set(false)
        executor.execute(new ResponseRunnable(response))
        currentlyRunning = true
      })
    }
  }

  //Call response, then start another response immediately if needed
  private class ResponseRunnable(val response:(AtomicBoolean => Unit)) extends Runnable {

    override def run() {
      try {
        response.apply(shouldCancel)
      } catch {
        case e:Exception => e.printStackTrace()
      } finally {
        responseLock.synchronized {
          currentlyRunning = false
          launchNewResponse()
        }
      }
    }
  }

}