package oscar.cbls.lib.search.combinators

import oscar.cbls._
import oscar.cbls.core.search._

//there is no API here because the relevant api are all available infix.

/**
 * saves the best solution when encountered during the search
 * IT DOES NOT RESTORE THE SOLUTION, but it provides methods to get the best solution, know about it, and restore it.
 * @note this combinator is not in te the API because it is kinda primitive for the relevant ones
 * @param a a neighborhood
 * @param o the objective function
 */
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
