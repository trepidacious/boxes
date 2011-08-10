package boxes

import util.DaemonThreadFactory
import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.{CancellationException, Executor, Executors}

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

class BackgroundReaction(responseSource: => (AtomicBoolean => Unit)) extends Reaction {

  val responder = Responder()

  def respond : (()=>Unit) = {
    val response = responseSource

    //Now schedule a response, will be performed in a background thread, and we also
    //assert that it doesn't perform any reads. If any reading WERE to be done,
    //it would not result in the data being registered as a source for the reaction,
    //and so the reaction might not be called when necessary
    responder.request{
      Box.withoutReading(response)
    }

    //Work is done by the responder, not immediately
    {() => Unit}
  }

  def isView = true

}

object BackgroundReaction {

  /**
   * Note that it is essential to retain this reaction somewhere - as it is returned from this method,
   * it is not retained by ANY strong references, since it is actually a View.
   */
  def apply(responseSource: => (AtomicBoolean => Unit)):BackgroundReaction = {
    val r = new BackgroundReaction(responseSource)
    Box.registerReaction(r)
    r
  }

  /**
   * Note that it is essential to retain this reaction somewhere - as it is returned from this method,
   * it is not retained by ANY strong references, since it is actually a View.
   */
  def applyWithoutCancel(response: => Unit):BackgroundReaction = {
    apply((cancel:AtomicBoolean) => response)
  }

}
