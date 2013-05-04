package boxes

import java.util.concurrent.atomic.AtomicBoolean
import util.Responder

class BackgroundReaction(responseSource: => (AtomicBoolean => Unit)) extends Reaction {

  val responder = Responder()

  def respond : (()=>Unit) = {
    
    //At this point, the responseSource can perform any Box reading (BUT NOT WRITING) it needs to, storing the
    //immutable data it gets in the returned long-running background task closure.
    val response = responseSource
    
    //Now schedule a response, will be performed in a background thread, and we also
    //assert that it doesn't perform any reads. If any reading WERE to be done,
    //it would not result in the data being registered as a source for the reaction,
    //and so the reaction might not be called when necessary.
    //However the background thread may do as much writing as it wants. Note that if it
    //writes to Boxes that are read by the responseSource, it may produce a (possibly undersirable)
    //cycle of responses.
    responder.request(
      //Explicitly create the new (AtomicBoolean=>Unit), which wraps the response in the withoutReading 
      (cancel : AtomicBoolean) => {
        Box.withoutReading{
          response.apply(cancel);
        }
      }
    )

    //Work is done by the responder, not immediately
    {() => Unit}
  }

  def isView = true
  def react {respond.apply()}

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
