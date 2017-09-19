package oscar.cbls.lib.search.combinators

import oscar.cbls._
import oscar.cbls.core.search._


class BasicSaveBest(a: Neighborhood, o: Objective) extends NeighborhoodCombinator(a) {

  protected val s = o.model

  require(s != null, "you are using an objective function that has no attached model, so "
    + this.getClass.getSimpleName + " cannot save the model; pass it explicitely to the Objective creation to solve this issue")

  protected var bestObj = if (currentSolutionIsAcceptable) o.value else Int.MaxValue
  protected var best = if (currentSolutionIsAcceptable) s.solution() else null

  //this resets the internal state of the move combinators
  override def reset() {
    super.reset()
    bestObj = Int.MaxValue
    best = null
  }

  override def getMove(obj: Objective, initialObj:Int, acceptanceCriteria: (Int, Int) => Boolean): SearchResult = {

    //we record the obj before move to prevent an additional useless propagation
    val objBeforeMove = o.value

    a.getMove(obj, initialObj, acceptanceCriteria) match {
      case NoMoveFound => NoMoveFound
      case MoveFound(m) =>
        if (m.objAfter > objBeforeMove && objBeforeMove < bestObj && currentSolutionIsAcceptable) {
          //solution degrades, and we were better than the best recorded
          //so we save
          best = s.solution(true)
          bestObj = objBeforeMove
          if (verbose >= 2) println("saving best solution before degradation (obj:" + bestObj + ")")
        }
        MoveFound(m)
    }
  }

  protected def currentSolutionIsAcceptable = true

  def restoreBest() {
    val isCurrentAccepteable = currentSolutionIsAcceptable
    if (best == null && !isCurrentAccepteable) {
      if (verbose >= 1) println("no single acceptable solution seen")
    } else if (o.value > bestObj || !isCurrentAccepteable) {
      s.restoreSolution(best)
      if (verbose >= 1) println("restoring best solution (obj:" + bestObj + ")")
    } else if (verbose >= 1) println("no better solution to restore")
  }

  def anythingToRestore:Boolean = {
    best != null && (o.value > bestObj || !currentSolutionIsAcceptable)
  }

  def getBestSolutionToRestore:Option[(Solution,Int)] = {
    if(best != null && (o.value > bestObj || !currentSolutionIsAcceptable)) {
      Some((best,bestObj))
    }else {
      None
    }
  }

  /**
   * same as doAllImprovingMoves and calling restoreBest after.
   *
   * @param shouldStop a function that takes the iteration number and returns true if search should be stopped
   *                   eg if the problem is considered as solved
   *                   you can evaluate some objective function there such as a violation degree
   *                   notice that although you can use it to stop your algorithm, the primary purpose is to avoid blow-up.
   *                   Smarter stop criterion cen be made using combinators, and this stop should be considered only as a protection againt blow up.
   * @param acceptanceCriterion a criterion for accepting a move
   *                            by default, we only accept strictly improving moves
   * @return the number of moves performed
   */
  def doAllMovesAndRestoreBest(shouldStop: Int => Boolean = _ => false, obj: Objective, acceptanceCriterion: (Int, Int) => Boolean = (oldObj, newObj) => oldObj > newObj): Int = {
    val toReturn = doAllMoves(shouldStop, obj, acceptanceCriterion)
    restoreBest()
    toReturn
  }

  def restoreBestOnExhaust: RestoreBestOnExhaust = new RestoreBestOnExhaust(this)
}



class SaveBest(a: Neighborhood, o: Objective) extends BasicSaveBest(a: Neighborhood, o: Objective) {

  def whenEmpty(violation: SetValue) = new SaveBestWhen(a, o, () => violation.value.isEmpty)
  def whenZero(violation: IntValue) = new SaveBestWhen(a, o, () => violation.value == 0)

  /**
   * this method restricts the save operation to only the situation where "shouldSave" returns true
   * notice that this is an override of the "when" method found in neighborhood.
   *
   * @param shouldSave
   * @return
   */
  def saveWhen(shouldSave: () => Boolean) = new SaveBestWhen(a, o, shouldSave)
}

class SaveBestWhen(a: Neighborhood, o: Objective, shouldSave: () => Boolean) extends BasicSaveBest(a, o) {
  override protected def currentSolutionIsAcceptable: Boolean = shouldSave()
}

class RestoreBestOnExhaust(a: BasicSaveBest) extends NeighborhoodCombinator(a) {

  var childExhausted = false

  def restoreBest(): Unit = {
    a.restoreBest()
  }

  override def reset() : Unit = {
    childExhausted = false
    super.reset()
  }

  /**
   * same as doAllImprovingMoves and calling restoreBest after.
   *
   * @param shouldStop a function that takes the iteration number and returns true if search should be stopped
   *                   eg if the problem is considered as solved
   *                   you can evaluate some objective function there such as a violation degree
   * @param acceptanceCriterion a criterion for accepting a move
   *                            by default, we only accept strictly improving moves
   * @return the number of moves performed
   */
  def doAllMovesAndRestoreBest(shouldStop: Int => Boolean, obj: Objective, acceptanceCriterion: (Int, Int) => Boolean = (oldObj, newObj) => oldObj > newObj): Int = {
    a.doAllMovesAndRestoreBest(shouldStop, obj, acceptanceCriterion)
  }

  override def getMove(obj: Objective, initialObj:Int, acceptanceCriteria: (Int, Int) => Boolean): SearchResult = {
    if(childExhausted) {
      childExhausted = false
      NoMoveFound
    } else {
      a.getMove(obj, initialObj, acceptanceCriteria) match {
        case m : MoveFound => m
        case x =>
          a.getBestSolutionToRestore match {
            case None => NoMoveFound
            case Some((s, bestObj)) =>
              childExhausted = true
              MoveFound(LoadSolutionMove(s, bestObj, "RestoreBestOnExhaust"))
          }
      }
    }
  }
}