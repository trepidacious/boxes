package boxes

import collection.mutable._
import util.WeakHashSet

object Box {

  //Currently active reaction - if there is one, it is responsible
  //for reads/writes, if there is none then reads/writes are external
  private var activeReaction:Option[Reaction] = None

  //Reactions that WILL be processed this cycle
  private val reactionsPending = ListBuffer[Reaction]()

  //Reactions that are also views
  private val viewReactionsPending = ListBuffer[Reaction]()

  //Track reactions that are newly added to the system (added AFTER the most recent full cycle), and so may need extra checks.
  private val newReactions = new scala.collection.mutable.HashSet[Reaction]()

  //During a cycle, maps each written box to the write index when it was FIRST written in the current cycle
  private val boxToWriteIndex = new HashMap[Box[_], Int]()

  //During a cycle, maps each written box to a list of changes applied to that list in the current cycle
  private val boxToChanges = new HashMap[Box[_], Object]

  //The next write index to assign during cycling
  private var writeIndex = 0

  private var canRead = true
  private var canWrite = true
  private var checkingConflicts = false

  private var cycling = false
  private var applyingReaction = false

  def beforeRead[C](b:Box[C]) = {
    if (!canRead) throw new InvalidReadException(b)
  }

  def afterRead[C](b:Box[C]) = {
    //This box is a source of any active reaction
    activeReaction.foreach(r => associateReactionSource(r, b))
  }

  /**
   * Boxes must call before possibly writing. Returns whether
   * there is a reaction being applied, which can be used by Boxes
   * to reject writes. E.g. Cal will only accept writes from Reactions.
   */
  def beforeWrite[C](b:Box[C]) = {
    if (!canWrite) throw new InvalidWriteException(b)
    applyingReaction
  }

  private def associateReactionSource(r:Reaction, b:Box[_]) {
    r.sources.add(b)
    b.sourcingReactions.add(r)
  }

  private def associateReactionTarget(r:Reaction, b:Box[_]) {
    r.targets.add(b)
    b.targettingReactions.add(r)
  }

  def commitWrite[C](b:Box[C], change:C) = {

    if (checkingConflicts) {
      activeReaction match {
        case Some(r) => throw new InvalidReactionException("Conflicting reaction", r, b)
        case None => {
          println("Code error - no active reaction")
          throw new RuntimeException("Conflicting reaction with no active reaction - code error")
        }
      }
    }

    boxToWriteIndex.getOrElseUpdate(b, writeIndex)
    writeIndex = writeIndex + 1

    boxToChanges.get(b) match {
      case None => boxToChanges.put(b, Queue(change))
      case Some(existingChanges) => boxToChanges.put(b, existingChanges.asInstanceOf[Queue[C]] :+ change)
    }

    //This box is a target of any active reaction
    activeReaction.foreach(r => associateReactionTarget(r, b))

    //Any reactions on this box are now pending
    for {
      reaction <- b.sourcingReactions
    } pendReaction(reaction)

    cycle
  }

  private def pendReaction(r:Reaction) = {
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

  def afterWrite[C](b:Box[C]) = {
  }

  def boxWriteIndex(b:Box[_]) : Option[Int] = {
    //Reading a box's write index counts as reading it, and
    //so for example makes a reaction have the box as a source
    beforeRead(b)
    try {
      return boxToWriteIndex.get(b)
    } finally {
      afterRead(b)
    }
  }

  def boxChanges[C](b:Box[C]):Option[Queue[C]] = {
    boxToChanges.get(b) match {
      case None => None
      //Note this is safe because we only ever accept changes
      //from a Box[C] of type C, and add them to list
      case Some(o) => Some(o.asInstanceOf[Queue[C]])
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
    newReactions.add(r)
    pendReaction(r)
    cycle
  }

  private def cycle = {

    //Only enter one cycle at a time
    if (!cycling) {
      cycling = true

      val failedReactions = new HashSet[Reaction]()

      //Which reaction (if any) has written each box, this cycle. Used to detect conflicts.
      val targetsToCurrentCycleReaction = new HashMap[Box[_], Reaction]()

      val conflictReactions = new HashSet[Reaction]()

      //Keep cycling until we clear all reactions
      while (!reactionsPending.isEmpty || !viewReactionsPending.isEmpty) {

        val nextReaction = if (!reactionsPending.isEmpty) reactionsPending.remove(0) else viewReactionsPending.remove(0);

        //FIXME at this point we should consider delaying
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
      //FIXME should use temp set for tracking new sources, then
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
        targetConflictingReaction <- newReactionTarget.targettingReactions
      } conflictReactions.add(targetConflictingReaction)

      newReactions.clear

      //Check all reactions whose TARGETS were changed
      //by other reactions are still happy with the state of their targets,
      //if they are not, this indicates a conflict and should generate a warning.
      //Note this is NOT the same as when a reaction is applied then has a source
      //changed, this should just result in the reaction being reapplied without
      //the expectation of no writes to its targets.
      //FIXME should we have a specific way of checking this, by asking reactions
      //whether they are valid? NOTE we can't just ask them to return something special
      //from respond if they will do nothing, since this introduces a read of their target
      //which is bad to have - adds lots of false sources on reactions that may well
      //only want to apply in one direction. The current system is fine as long as boxes
      //all check for and ignore writes that make no difference, OR reactions return
      //responses that do nothing if they are valid. Actually this is probably best.
      //println("Checking for conflicts...")
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
          }
        }
      }
      checkingConflicts = false

      //Write order is only valid during cycling
      boxToWriteIndex.clear
      writeIndex = 0
      boxToChanges.clear

      //Done for this cycle
      cycling = false

      if (!failedReactions.isEmpty) {
        //TODO make immutable copy of failed reactions for exception
        throw new FailedReactionsException()//scala.collection.immutable.Set(failedReactions))
      }
    }
  }

  def clearReactionSourcesAndTargets(r:Reaction) = {
    for {
      target <- r.targets
    } target.targettingReactions.remove(r)
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

trait Box[C] {

  private[boxes] val sourcingReactions = new WeakHashSet[Reaction]()
  private[boxes] val targettingReactions = Set[Reaction]()

  def changes = Box.boxChanges(this)
  def writeIndex = Box.boxWriteIndex(this)

}