package boxes

import boxes.util.WeakMultiMap
import collection.mutable.{WeakHashMap, ListBuffer, HashMap}
import ref.WeakReference
import util.WeakMultiMap

object Box {


  //Weak maps used to associate reactions to sources and targets.
  //Weakness is used to ensure we do not retain anything except
  //the reactions required for Boxes that are retained OUTSIDE these
  //maps. See comments on targetsToReactions for more details.

  //Current sources for each known reaction - weak for keys AND values
  private val reactionsToSources = new WeakMultiMap[Reaction, Box]()

  //Current reactions for each known source - weak for keys AND values
  private val sourcesToReactions = new WeakMultiMap[Box, Reaction]()

  //Current targets for each known reaction - weak for keys AND values
  private val reactionsToTargets = new WeakMultiMap[Reaction, Box]()

  //Current reactions for each known target - weak for keys only
  //Note that this does NOT use weak references to reactions in the Set.
  //This is deliberate, with the intention that we retain any Reaction as long as
  //it targets a Box that is otherwise retained. So if Box b is still in this map, that implies
  //it is referenced outside this map. This causes the set of Reactions to be retained, and
  //in turn causes each individual reaction to be retained.
  //Note that there are two types of reactions:
  // Firstly, reactions that have no side effects other than writing to their targets. By definition
  // these should be retained exactly as long as their targets. Once their targets are GCed, there is
  // no reason for the targets still to be executed, and so the reactions can be GCed too.
  // Secondly, reactions that act as views, and so do not write to their targets, but do have
  // side effects (updating a UI, logging changes, etc.). These must be retained by whatever
  // external objects require the side effects, and so we should not retain them at all. For example,
  // a UI will have a hierarchy of UI components, and each component should retain whatever reactions
  // it uses to update itself, otherwise they will be GCed.
  private val targetsToReactions = new WeakHashMap[Box, scala.collection.mutable.Set[Reaction]]() with collection.mutable.MultiMap[Box, Reaction]


  //Currently active reaction - if there is one, it is responsible
  //for reads/writes, if there is none then reads/writes are external
  private var activeReaction:Option[Reaction] = None

  //Reactions that WILL be processed this cycle
  private val reactionsPending = ListBuffer[Reaction]()

  //Current PENDING reactions for each known target
  private val targetsToPendingReactions = new HashMap[Box, scala.collection.mutable.Set[Reaction]]() with collection.mutable.MultiMap[Box, Reaction]

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
    sourcesToReactions.addBinding(b, r)
    reactionsToSources.addBinding(r, b)
    //println(b + " is now source of " + r)
  }

  private def associateReactionTarget(r:Reaction, b:Box) {
    targetsToReactions.addBinding(b, r)
    reactionsToTargets.addBinding(r, b)
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
      reactions <- sourcesToReactions.get(b)
      reaction <- reactions
    } pendReaction(reaction)

    cycle
  }

  private def pendReaction(r:Reaction) = {
    if (!reactionsPending.contains(r)) {
      reactionsPending.append(r)

      //println("Pending reaction " + r + ", pending count " + reactionsPending.size)

      //Now reaction is pending, it should be in the map for each expected target
      for {
        targets <- reactionsToTargets.get(r)
        target <- targets
      } targetsToPendingReactions.addBinding(target, r)
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

    //FIXME consider resetting state in a finally block, to allow surviving a runtime exception

    //Only enter one cycle at a time
    if (!cycling) {
      cycling = true

      //Which reaction (if any) has written each box, this cycle. Used to detect conflicts.
      val targetsToCurrentCycleReaction = new HashMap[Box, Reaction]()

      val conflictReactions = new scala.collection.mutable.HashSet[Reaction]()

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
        reactionsToTargets.remove(nextReaction)
        targetsToReactions.foreach{p => p._2.remove(nextReaction)}
        reactionsToSources.remove(nextReaction)
        sourcesToReactions.foreach{p => p._2.remove(nextReaction)}

        //Same for pending reactions map
        targetsToPendingReactions.foreach{p => p._2.remove(nextReaction)}

        //println("Reaction " + nextReaction + " about to respond")
        reactionRespondAndApply(nextReaction)
        //println("Reaction " + nextReaction + " applied")

        //We now have the correct targets for this reaction, so
        //we can track them for conflicts
        reactionsToTargets.get(nextReaction).foreach(s => s.foreach(b => {
          targetsToCurrentCycleReaction.put(b, nextReaction).foreach(conflictBox => conflictReactions.add(nextReaction))
        }))

      }

      //           for each new reaction           get its targets, and if there are any                 get other reactions with same targets, and if ther are any, add them all as conflict reactions
//      newReactions.foreach(newReaction =>
//        reactionsToTargets.get(newReaction).foreach(newReactionTargetOption =>
//          newReactionTargetOption.foreach(newReactionTarget =>
//            targetsToReactions.get(newReactionTarget).foreach(targetConflictingReactionsOption =>
//              targetConflictingReactionsOption.foreach(targetConflictingReaction =>
//                conflictReactions.add(targetConflictingReaction)
//              )
//            )
//          )
//        )
//      )
//      newReactions.clear

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
        newReactionTargetSet <- reactionsToTargets.get(newReaction)
        newReactionTarget <- newReactionTargetSet
        targetConflictingReactionSet <- targetsToReactions.get(newReactionTarget)
        targetConflictingReaction <- targetConflictingReactionSet
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
      conflictReactions.foreach{r =>
        reactionRespondAndApply(r)
      }
      checkingConflicts = false
      //println("No conflicts")

      //Write order is only valid during cycling
      boxToWriteIndex.clear
      writeIndex = 0

      //Done for this cycle
      cycling = false

      //Check map is clear as expected, not counting empty sets
      targetsToPendingReactions.foreach{p => if(!p._2.isEmpty) throw new RuntimeException("Finished a cycle with targetsToPendingReactions not empty")}
      targetsToPendingReactions.clear
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

//  val reactions = HashMap[Reaction,Int]()
//
//  def retainReaction(r:Reaction) = {
//    val v = reactions.get(r) match {
//      case Some(x) => x + 1
//      case None => 1
//    }
//    reactions.put(r, v)
//  }
//
//  def releaseReaction(r:Reaction) = {
//    reactions.get(r) match {
//      case Some(1) => reactions.remove(r)
//      case Some(x) => reactions.put(r, x-1)
//      case None => 1
//    }
//  }

}