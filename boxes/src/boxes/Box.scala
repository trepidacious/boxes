package boxes

import collection._
import mutable.MultiMap
import util.WeakHashSet
import actors.threadpool.locks.ReentrantLock
import scala.math.Numeric._
import Numeric.Implicits._

object BoxImplicits {
  implicit def closureToPathViaOption[T](path : => Option[Var[T]]) = PathViaOption(path)
  implicit def closureToPath[T](path : =>Var[T]) = Path(path)
  implicit def closureToPathToOption[T](path : => Option[Var[Option[T]]]) = PathToOption(path)
  implicit def valueToVal[T](t:T) = Val(t)
}

class NumericVar[N](v: Var[N])(implicit n: Numeric[N]) {
  
  def from(min: N) = {v << n.max(min, v()); v}
  def to(max: N) = {v << n.min(max, v()); v}
  def from(min: RefGeneral[N, _]) = {v << n.max(min(), v()); v}
  def to(max: RefGeneral[N, _]) = {v << n.min(max(), v()); v}
}

object NumericVarImplicits {
  implicit def var2NumericVar[N: Numeric](v: Var[N]): NumericVar[N] = new NumericVar[N](v)
}

object Box {

  //Currently active reaction - if there is one, it is responsible
  //for reads/writes, if there is none then reads/writes are external
  private var activeReaction:Option[Reaction] = None

  //Reactions that WILL be processed this cycle
  private val reactionsPending = mutable.ListBuffer[Reaction]()

  //Reactions that are also views
  private val viewReactionsPending = mutable.ListBuffer[Reaction]()

  //Track reactions that are newly added to the system (added AFTER the most recent full cycle), and so may need extra checks.
  private val newReactions = new mutable.HashSet[Reaction]()

  //During a cycle, maps each written box to a list of changes applied to that list in the current cycle
  //Actually the values are a pair of change index and change
  //Note that we don't bother with type for the values -
  //they are Queues of (Long, C) where C matches the Box[C] used as a key
  private val boxToChanges = new mutable.HashMap[Box[_], Object]

  //Threads that have asked not to be allowed to read
  private val nonReadingThreads = new mutable.HashSet[Thread]

  //For each reaction that has had any source change, maps to the set of boxes that have changed for that reaction. Allows
  //reactions to see why they have been called in any given cycle. Empty outside cycles. Note that from one call to
  //a reaction to the next, may acquire additional entries, etc.
  private val changedSourcesForReaction = new mutable.HashMap[Reaction, mutable.Set[Box[_]]] with MultiMap[Reaction, Box[_]]

  //The next change index to assign
  private var changeIndex = 0L

  private var canRead = true
  private var canWrite = true
  private var checkingConflicts = false

  private var cycling = false
  private var applyingReaction = false

  private val lock = new ReentrantLock()

  private var decoding = false

  private var _cycleIndex = -1

  def cycleIndex = _cycleIndex

  def changedSources(r:Reaction) = immutable.Set(changedSourcesForReaction.get(r).getOrElse(Set[Box[_]]()).toList:_*)

  def decode[T](decode : =>T):T = {
    beforeDecode
    try {
      return decode
    } finally {
      afterDecode
    }
  }

  def withoutReading[T](action : =>T):T = {
    nonReadingThreads.add(Thread.currentThread())
    try {
      return action
    } finally {
      nonReadingThreads.remove(Thread.currentThread())
    }
  }

  def transact[T](transaction : =>T):T = {
    lock.lock
    try {
      return transaction
    } finally {
      lock.unlock
    }
  }

  private def beforeDecode() ={
    lock.lock
    decoding = true
  }

  private def afterDecode() = {
    if (!decoding) throw new RuntimeException("afterDecode called when not decoding")
    decoding = false

    //Any changes that have been made during deserialisation are considered
    //to be irrelevant, since serialised state is known to be valid. We will
    //still cycle though, so that all reactions can be treated as new, and
    //so will be run to establish sources and targets, etc.
    boxToChanges.clear
    cycle
    lock.unlock
  }

  def beforeRead[C](b:Box[C]) = {
    lock.lock
    if (!canRead) throw new InvalidReadException(b)

    //We allow reading when we are servicing reactions, since
    //these have their own enforcement of the timing of reading,
    //done using canRead above.
    if (activeReaction == None) {
      if (nonReadingThreads.contains(Thread.currentThread())) {
        throw new InvalidReadException(b)
      }
    }
  }

  def afterRead[C](b:Box[C]) = {
    //This box is a source of any active reaction
    activeReaction.foreach(r => associateReactionSource(r, b))
    lock.unlock
  }

  /**
   * Boxes must call before possibly writing. Returns whether
   * there is a reaction being applied, which can be used by Boxes
   * to reject writes. E.g. Cal will only accept writes from Reactions.
   */
  def beforeWrite[C](b:Box[C]) = {
    lock.lock
    if (!canWrite) throw new InvalidWriteException(b)

    //This box is a source of any active reaction
    activeReaction.foreach(r => associateReactionTarget(r, b))
    applyingReaction
  }

  /**
   * Call after an actual write has been carried out on a box,
   * with a list of one or more changes that have been made, in
   * order. Note that there MUST be at least one change, to allow
   * for reactions to find the order of changes, etc.
   */
  def commitWrite[C](b:Box[C], changes:C*) = {

    if (checkingConflicts) {
      activeReaction match {
        case Some(r) => throw new InvalidReactionException("Conflicting reaction", r, b)
        case None => {
          throw new RuntimeException("Conflicting reaction with no active reaction - code error")
        }
      }
    }

    var q = boxToChanges.get(b).getOrElse(immutable.Queue[(Long,C)]()).asInstanceOf[immutable.Queue[(Long,C)]]
    changes.foreach(newChange => {
      q = q:+(changeIndex, newChange)
      changeIndex += 1
    })
    boxToChanges.put(b, q)


    //Any reactions on this box are now pending
    for {
      reaction <- b.sourcingReactions
    } pendReaction(reaction, List(b))

    cycle
  }

  def afterWrite[C](b:Box[C]) = {
    lock.unlock
  }

  private def associateReactionSource(r:Reaction, b:Box[_]) {
    r.sources.add(b)
    b.sourcingReactions.add(r)
  }

  private def associateReactionTarget(r:Reaction, b:Box[_]) {
    r.targets.add(b)
    b.targetingReactions.add(r)
  }

  private def pendReaction(r:Reaction, sourceBoxes:List[Box[_]] = List()) = {
    sourceBoxes.foreach(b => changedSourcesForReaction.addBinding(r, b))
    r.isView match {
      case false => {
        if (!reactionsPending.contains(r)) {
          reactionsPending.append(r)
        }
      }
      case true => {
        if (!viewReactionsPending.contains(r)) {
          viewReactionsPending.append(r)
        }
      }
    }
  }

  def boxFirstChangeIndex[C](b:Box[C]) : Option[Long] = {
    //Reading a box's write index counts as reading it, and
    //so for example makes a reaction have the box as a source
    try {
      beforeRead(b)
      boxToChanges.get(b) match {
        case None => return None
        case Some(o) => return Some(o.asInstanceOf[immutable.Queue[(Long,C)]].head._1)
      }
    } finally {
      afterRead(b)
    }
  }

  def boxChanges[C](b:Box[C]):Option[immutable.Queue[(Long,C)]] = {
    //Reading a box's changes counts as reading it, and
    //so for example makes a reaction have the box as a source
    try {
      beforeRead(b)
      return boxToChanges.get(b) match {
        case None => None
        //Note this is safe because we only ever accept changes
        //from a Box[C] of type C, and add them to list
        case Some(o) => Some(o.asInstanceOf[immutable.Queue[(Long,C)]])
      }
    } finally {
      afterRead(b)
    }
  }

  /**
   * Register a reaction - MUST be called after a reaction
   * is created, to allow it to trigger. Reaction will
   * trigger after this is called - either immediately
   * if we are NOT in a cycle, or towards the end of the cycle
   * if we are
   */
  def registerReaction(r:Reaction) = {
    //Requires lock
    lock.lock
    try {
      newReactions.add(r)
      pendReaction(r)
      cycle
    } finally {
      lock.unlock
    }
  }

  private def cycle = {

    //Only enter one cycle at a time, and we don't cycle while decoding
    if (!cycling && !decoding) {
      cycling = true

      _cycleIndex += 1

      val failedReactions = new mutable.HashSet[Reaction]()

      //Which reaction (if any) has written each box, this cycle. Used to detect conflicts.
      val targetsToCurrentCycleReaction = new mutable.HashMap[Box[_], Reaction]()

      val conflictReactions = new mutable.HashSet[Reaction]()

      //Keep cycling until we clear all reactions
      while (!reactionsPending.isEmpty || !viewReactionsPending.isEmpty) {

        val nextReaction = if (!reactionsPending.isEmpty) reactionsPending.remove(0) else viewReactionsPending.remove(0);

        //TODO at this point we should consider delaying
        //execution of reactions that have sources which
        //are also targets of pending reactions, to avoid
        //having to re-apply them later if those sources
        //are written as expected. This has the disadvantage
        //of not propagating reactions strictly "outwards"
        //from the first change, but this propagation order
        //is only really intended to assist in achieving the
        //expected result (e.g. for small cycles), not be a
        //firm guarantee, since in any case the propagation
        //order may not be completely predictable.

        //Clear this targets expected targets and sources,
        //so that they can be added from fresh by calling
        //reaction.respond and then applying that response
      //TODO should use temp set for tracking new sources, then
      //modify the sourceReactions from this, to allow for keeping the same
      //weak references (if appropriate) rather than regenerating every cycle.
        clearReactionSourcesAndTargets(nextReaction)

        try {

          reactionRespondAndApply(nextReaction)

          //We now have the correct targets for this reaction, so
          //we can track them for conflicts
          for {
            target <- nextReaction.targets
            conflictReaction <- targetsToCurrentCycleReaction.put(target, nextReaction)
          } conflictReactions.add(conflictReaction)

        } catch {
          case e:BoxException => {
            //Remove the reaction completely from the system, but remember that it failed
            clearReactionSourcesAndTargets(nextReaction)
            conflictReactions.remove(nextReaction)
            newReactions.remove(nextReaction)
            failedReactions.add(nextReaction)
          }
          case e:Exception => {
              //TODO need to respond better, but can't allow uncaught exception to just stop cycling
            e.printStackTrace()
            clearReactionSourcesAndTargets(nextReaction)
            conflictReactions.remove(nextReaction)
            newReactions.remove(nextReaction)
            failedReactions.add(nextReaction)
          }
        }

      }

      //Now that we know the targets affected by each new reaction, we will
      //mark any reactions targeting those same targets as conflictReactions.
      //Consider the case where reaction r targets a box b, and so does a reaction
      //s. In this case, if we add register r, then register s, reaction r won't be
      //applied in the cycle caused by adding s. But it may conflict with s, and so
      //needs to be checked at the end of the cycle where s is registered (when s is a
      //newReaction). Again note that this is different from registering a new reaction
      //which targets the SOURCE of another reaction, which is handled in the main while
      //loop above.
      for {
        newReaction <- newReactions
        newReactionTarget <- newReaction.targets
        targetConflictingReaction <- newReactionTarget.targetingReactions
      } conflictReactions.add(targetConflictingReaction)

      newReactions.clear

      //Check all reactions whose TARGETS were changed
      //by other reactions are still happy with the state of their targets,
      //if they are not, this indicates a conflict and should generate a warning.
      //Note this is NOT the same as when a reaction is applied then has a source
      //changed, this should just result in the reaction being reapplied without
      //the expectation of no writes to its targets.
      //TODO should we have a specific way of checking this, by asking reactions
      //whether they are valid? NOTE we can't just ask them to return something special
      //from respond if they will do nothing, since this introduces a read of their source
      //which is bad to have - adds lots of false sources on reactions that may well
      //only want to apply in one direction. The current system is fine as long as boxes
      //all check for and ignore writes that make no difference, OR reactions return
      //responses that do nothing if they are valid. Actually this is probably best.
      checkingConflicts = true
      conflictReactions.foreach{
        r => {
          try {
            reactionRespondAndApply(r)
          } catch {
            case e:BoxException => {
              //Remove the reaction completely from the system, but remember that it failed
              clearReactionSourcesAndTargets(r)
              failedReactions.add(r)
            }
            case e:Exception => {
              //TODO need to respond better, but can't allow uncaught exception to just stop cycling
              e.printStackTrace()
              //Remove the reaction completely from the system, but remember that it failed
              clearReactionSourcesAndTargets(r)
              failedReactions.add(r)
            }
          }
        }
      }
      checkingConflicts = false

      //Only valid during cycling
      boxToChanges.clear()
      changedSourcesForReaction.clear()


      //Done for this cycle
      cycling = false

      if (!failedReactions.isEmpty) {
        println("Failed Reactions: " + failedReactions)
        //TODO make immutable copy of failed reactions for exception
        throw new FailedReactionsException()//Set(failedReactions))
      }
    }
  }

  def clearReactionSourcesAndTargets(r:Reaction) = {
    for {
      target <- r.targets
    } target.targetingReactions.remove(r)
    r.targets.clear
    for {
      source <- r.sources
    } source.sourcingReactions.remove(r)
    r.sources.clear
  }

  def reactionRespondAndApply(r:Reaction) = {

    activeReaction = Some(r)
    try {

      canWrite = false
      val response = r.respond
      canWrite = true

      if (!r.isView) {
        canRead = false
        applyingReaction = true
        response.apply
      }

    } finally {
      activeReaction = None
      applyingReaction = false
      canRead = true
      canWrite = true
    }

  }

}

/**
 * One part of the boxes system. The other part is Reaction.
 *
 * Be VERY careful making Boxes equal each other when they are not the SAME
 * Box (identical). This is because maps and sets are used for storing Boxes,
 * for example which Boxes are affected by a Reaction, and so equal Boxes will
 * be treated as the same Box - for example, only one of an equal set of Boxes
 * will be updated by a Reaction that might be intended to update more than one of them.
 *
 * However it is unlikely that you will need to implement a new Box in any case.
 */
trait Box[C] {

  private[boxes] val sourcingReactions = new WeakHashSet[Reaction]()
  
  //FIXME this used to be non-weak, and probably can be without problems, but need to work out what is best
  private[boxes] val targetingReactions = new WeakHashSet[Reaction]()

  private[boxes] val retainedReactions = mutable.Set[Reaction]()

  def changes = Box.boxChanges(this)
  def firstChangeIndex = Box.boxFirstChangeIndex(this)

  def retainReaction(r:Reaction) = retainedReactions.add(r)
  def releaseReaction(r:Reaction) = retainedReactions.remove(r)
  
  def react (r:Reaction) = {
    Box.registerReaction(r)
    retainReaction(r)
    r
  }

  def react (r: =>(()=>Unit)) = Reaction(this, r)
  
}
