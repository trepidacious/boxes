package boxes

import util.DaemonThreadFactory
import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.{Executor, Executors}

object Responder {
	val defaultExecutorService = Executors.newFixedThreadPool(4, new DaemonThreadFactory());

  def apply[D](response:((D, AtomicBoolean) => Unit), executor:Executor = defaultExecutorService) = new Responder(response, executor)
}

class Responder[D](val response:((D, AtomicBoolean) => Unit), val executor:Executor) {

  private val responseLock = new Object()
  private var responseData:Option[D] = None
  private var currentlyRunning = false
  private val shouldCancel = new AtomicBoolean(false)

  def request(data:D) {
    responseLock.synchronized {
      responseData = Some(data)

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
      responseData.foreach(data => {
        responseData = None
        shouldCancel.set(false)
        executor.execute(new ResponseRunnable(data))
        currentlyRunning = true
      })
    }
  }

  //Call response, then start another response immediately if needed
  private class ResponseRunnable(val d:D) extends Runnable {

    override def run() {
      try {
        response.apply(d, shouldCancel)
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

class BackgroundReaction[D](gather: =>D, process:((D, AtomicBoolean) => Unit), name:String) extends Reaction {

  //The process should NOT read any state, only perform calculations on the
  //state, then perform writes as appropriate. If any reading WERE to be done,
  //it would not result in the data being registered as a source for the reaction,
  //and so the reaction might not be called when necessary
  val responder = Responder((d:D, a:AtomicBoolean) => {
    Box.withoutReading(process(d, a))
  })

  def respond : (()=>Unit) = {
    //Gather the data, so we make sure we read all relevant data
    val d = gather

    //Now schedule a response, will be performed in a background thread
    responder.request(d)

    //We are a view, so do nothing immediately, the background thread may have some effect
    {() => Unit}
  }

  def isView = true

  override def toString = "BR: " + name

}

object BackgroundReaction {

  /**
   * Note that it is essential to retain this reaction somewhere - as it is returned from this method,
   * it is not retained by ANY strong references, since it is actually a View.
   */
  def apply[D](gather: =>D, process:((D, AtomicBoolean) => Unit), name:String = "Unnamed Background Reaction") = {
    val r = new BackgroundReaction(gather, process, name)
    Box.registerReaction(r)
    r
  }

}
