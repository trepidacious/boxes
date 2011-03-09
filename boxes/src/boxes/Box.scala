package boxes

import collection.mutable._
import util.{WeakHashSet, WeakMultiMap}

object Box {


  //Currently active reaction - if there is one, it is responsible
  //for reads/writes, if there is none then reads/writes are external
  private var activeReaction:Option[Reaction] = None

  //Reactions that WILL be processed this cycle
  private val reactionsPending = ListBuffer[Reaction]()

  //Track reactions that are newly added to the system (added AFTER the most recent full cycle), and so may need extra checks.
  private val newReactions = new scala.collection.mutable.HashSet[Reaction]()

  //During a cycle, maps each written box to the write index when it was FIRST written in the current cycle
  private val boxToWriteIndex = new HashMap[Box, Int]()

  //The next write index to assign during cycling
  private var writeIndex = 0

  private var canRead = true
  private var canWrite = true
  private var checkingConflicts = false

  private var cycling = false
  private var applyingReaction = false

  def beforeRead(b:Box) = {
    //println("beforeRead " + b)
    if (!canRead) throw new RuntimeException("Attempted to read incorrectly")
  }

  def afterRead(b:Box) = {
    //println("afterRead " + b)

    //This box is a source of any active reaction
    activeReaction.foreach(r => associateReactionSource(r, b))
  }

  /**
   * Boxes must call before possibly writing. Returns whether
   * there is a reaction being applied, which can be used by Boxes
   * to reject writes. E.g. Cal will only accept writes from Reactions.
   */
  def beforeWrite(b:Box) = {
    //println("beforeWrite " + b)
    if (!canWrite) throw new RuntimeException("Attempted to write incorrectly")
    applyingReaction
  }

  private def associateReactionSource(r:Reaction, b:Box) {
    r.sources.add(b)
    b.sourcingReactions.add(r)
    //println(b + " is now source of " + r)
  }

  private def associateReactionTarget(r:Reaction, b:Box) {
    r.targets.add(b)
    b.targettingReactions.add(r)
    //println(b + " is now target of " + r)
  }

  def commitWrite(b:Box) = {
    //println("commitWrite " + b)

    if (checkingConflicts) {
      activeReaction match {
        case Some(r) => throw new ConflictException(r, b)
        case None => {
          println("Code error - no active reaction")
          throw new RuntimeException("Conflicting reaction with no active reaction - code error")
        }
      }
    }

    boxToWriteIndex.getOrElseUpdate(b, writeIndex)
    writeIndex = writeIndex + 1

    //This box is a target of any active reaction
    activeReaction.foreach(r => associateReactionTarget(r, b))

    //Any reactions on this box are now pending
    for {
      reaction <- b.sourcingReactions
    } pendReaction(reaction)

    cycle
  }

  private def pendReaction(r:Reaction) = {
    if (!reactionsPending.contains(r)) {
      reactionsPending.append(r)

      //println("Pending reaction " + r + ", pending count " + reactionsPending.size)
    }
  }

  def afterWrite(b:Box) = {
    //println("afterWrite " + b)
  }

  def boxWriteIndex(b:Box) : Option[Int] = {
    //Reading a box's write index counts as reading it, and
    //so for example makes a reaction have the box as a source
    beforeRead(b)
    try {
      return boxToWriteIndex.get(b)
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
    newReactions.add(r)
    pendReaction(r)
    cycle
  }

  private def cycle = {

    //FIXME Bad reactions should be disconnected form system and stored in a list during cycling, then
    //an exception thrown when system is back in a valid state. This prevents exceptions destroying the system.

    //Only enter one cycle at a time
    if (!cycling) {
      cycling = true

      //Which reaction (if any) has written each box, this cycle. Used to detect conflicts.
      val targetsToCurrentCycleReaction = new HashMap[Box, Reaction]()

      val conflictReactions = new HashSet[Reaction]()

      //Keep cycling until we clear all reactions
      while (!reactionsPending.isEmpty) {
        val nextReaction = reactionsPending.remove(0);

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
        for {
          target <- nextReaction.targets
        } target.targettingReactions.remove(nextReaction)
        nextReaction.targets.clear
        for {
          source <- nextReaction.sources
          //FIXME should use temp set for tracking new sources, then
          //modify the sourceReactions from this, to allow for keeping the same
          //weak references (if appropriate) rather than regenerating every cycle.
        } source.sourcingReactions.remove(nextReaction)
        nextReaction.sources.clear

        //println("Reaction " + nextReaction + " about to respond")
        reactionRespondAndApply(nextReaction)
        //println("Reaction " + nextReaction + " applied")

        //We now have the correct targets for this reaction, so
        //we can track them for conflicts
        
        for {
          target <- nextReaction.targets
          conflictReaction <- targetsToCurrentCycleReaction.put(target, nextReaction)
        } conflictReactions.add(conflictReaction)

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
        r => reactionRespondAndApply(r)
      }
      checkingConflicts = false
      //println("No conflicts")

      //Write order is only valid during cycling
      boxToWriteIndex.clear
      writeIndex = 0

      //Done for this cycle
      cycling = false

    }
  }

  def reactionRespondAndApply(r:Reaction) = {

    activeReaction = Some(r)

    canWrite = false
    val response = r.respond
    canWrite = true
    //println("Reaction " + nextReaction + " responded")

    //println("Reaction " + nextReaction + " about to apply")
    canRead = false
    applyingReaction = true
    response.apply
    applyingReaction = false
    canRead = true

    activeReaction = None

  }

}

trait Box {

  private[boxes] val sourcingReactions = new WeakHashSet[Reaction]()
  private[boxes] val targettingReactions = Set[Reaction]()

}