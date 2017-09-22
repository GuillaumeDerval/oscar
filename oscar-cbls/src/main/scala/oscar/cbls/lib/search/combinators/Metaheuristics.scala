package oscar.cbls.lib.search.combinators

import oscar.cbls._
import oscar.cbls.core.objective.{Objective, CascadingObjective}
import oscar.cbls.core.search._

object Restart{
  /**
   * performs a restart of the search for a number of time.
   * it queries neighborhood on the left every time (this is the search neighborhood)
   * if the search neighborhood is exhausted, it queries the randomizationNeighborhood once (this is the randomization neighborhood, and resets the neighborhood on the left
   * the process of restarting is allowed maxRestartWithoutImprovement time without improvement over the objective function obj,
   * that is: every time the search neighborhood is exhausted, it checks if the search delivered an improvement over the objective function,
   * and the restart is only performed if it could find an improvement at least once in the last "maxRestartWithoutImprovement" descents.
   *
   * the best solution is reloaded at exhaustion of this neighborhood.
   *
   * @param randomizationNeighborhood the neighborhood that will randomize the current solution
   * @param maxRestartWithoutImprovement the stop criterion of the restarting
   * @param obj the objective function
   */
  def apply(n:Neighborhood,randomizationNeighborhood:Neighborhood, maxRestartWithoutImprovement:Int, obj:Objective) = {
    (n orElse (randomizationNeighborhood
      maxMoves maxRestartWithoutImprovement withoutImprovementOver obj improvementBeignMeasuredBeforeNeighborhoodExploration)
      ) saveBestAndRestoreOnExhaust obj
  }
}


/**
 * this combinator injects a metropolis acceptation function.
 * the criterion accepts all improving moves, and for worsening moves, it applies the metropolis criterion:
 * accept if math.random(0.0; 1.0) < base exponent (-gain / temperatureValue)
 *
 * @param a the original neighborhood
 * @param temperature a function that inputs the number of moves of a that have been actually taken,
 *                    and outputs a temperature, for use in the criterion
 *                    the number of steps is reset to zero when the combinator is reset
 *                    by default, it is the constant function returning 100
 * @param base the base for the exponent calculation. default is 2
 */
class Metropolis(a: Neighborhood, temperature: Int => Float = _ => 100, base: Float = 2) extends NeighborhoodCombinator(a) {

  var moveCount = 0
  var temperatureValue: Float = temperature(moveCount)
  override def getMove(obj: Objective, initialObj:Int, acceptanceCriterion: (Int, Int) => Boolean): SearchResult =
    a.getMove(obj, initialObj:Int, acceptation) match {
      case NoMoveFound => NoMoveFound
      case MoveFound(m) => InstrumentedMove(m, notifyMoveTaken)
    }

  def acceptation(oldObj: Int, newObj: Int): Boolean = {
    val gain = oldObj - newObj
    if (gain > 0) return true
    // metropolis criterion
    return math.random < math.pow(base, -gain / temperatureValue)
  }

  def notifyMoveTaken() {
    moveCount += 1
    temperatureValue = temperature(moveCount)
  }

  //this resets the internal state of the move combinators
  override def reset() {
    super.reset()
    moveCount = 0
    temperatureValue = temperature(moveCount)
  }
}

/**
 * This represents a guided local search where a series of objective criterion are optimized one after the other
 * the switching is performed on exhaustion, and a is reset on switching.
 * Notice that if you want to use different neighborhoods depending on the objective function, you should rather use a series of neighborhood with the objectiveFucntion combinator
 *
 * @param a the neighborhood to consider
 * @param objectives the list of objective to consider
 * @param resetOnExhaust  on exhaustion of the current objective, restores the best value for this objective before switching to the next objective
 */
//TODO: test this and add to API
class GuidedLocalSearch(a: Neighborhood, objectives: List[Objective], resetOnExhaust: Boolean) extends NeighborhoodCombinator(a) {

  var currentObjective: Objective = null
  var tailObjectives: List[Objective] = objectives
  var currentSun: Neighborhood = null

  def switchToNext(): Boolean = {
    tailObjectives match {
      case h :: t =>
        currentObjective = h
        tailObjectives = t
        currentSun = if (resetOnExhaust) new SaveBest(a, h) else a
        true
      case _ =>
        currentObjective = null
        tailObjectives = null
        currentSun = null
        false
    }
  }

  switchToNext()

  /**
   * the method that returns a move from the neighborhood.
   * The returned move should typically be accepted by the acceptance criterion over the objective function.
   * Some neighborhoods are actually jumps, so that they might violate this basic rule however.
   *
   * @param obj the objective function. notice that it is actually a function. if you have an [[oscar.cbls.core.objective.Objective]] there is an implicit conversion available
   * @param acceptanceCriterion
   * @return
   */
  override def getMove(obj: Objective, initialObj:Int, acceptanceCriterion: (Int, Int) => Boolean): SearchResult = {
    if (currentSun == null) {
      NoMoveFound
    } else {
      currentSun.getMove(currentObjective, initialObj:Int, acceptanceCriterion) match {
        case NoMoveFound =>
          if (resetOnExhaust) currentSun.asInstanceOf[SaveBest].restoreBest()
          switchToNext()
          getMove(obj, initialObj:Int, acceptanceCriterion)
        case m: MoveFound => m
      }
    }
  }

  //this resets the internal state of the Neighborhood
  override def reset() {
    tailObjectives = objectives
    switchToNext()
    if (currentSun != null) currentSun.reset()
    else super.reset()
  }
}

/**
 * This represents an accumulatingSearch: it searches on a given objective until this objective gets to zero,
 * then it switches to the second one, and rejects all update that would actually decrease the first objective
 * it will use the acceptance criterion, but extend it in the second phase
 *
 * @param a the neighborhood
 * @param firstObjective the first objective function
 * @param secondObjective the second objective function
 */
//TODO: test this and add to API
class AccumulatingSearch(a: Neighborhood, firstObjective: Objective, secondObjective: Objective) extends NeighborhoodCombinator(a) {

  val fullSecondObjective = new CascadingObjective(firstObjective, secondObjective)

  /**
   * the method that returns a move from the neighborhood.
   * The returned move should typically be accepted by the acceptance criterion over the objective function.
   * Some neighborhoods are actually jumps, so that they might violate this basic rule however.
   *
   * @param obj the objective function. notice that it is actually a function. if you have an [[oscar.cbls.core.objective.Objective]] there is an implicit conversion available
   * @param acceptanceCriterion
   * @return
   */
  override def getMove(obj: Objective, initialObj:Int, acceptanceCriterion: (Int, Int) => Boolean): SearchResult = {
    if (firstObjective() != 0) {
      a.getMove(firstObjective, initialObj:Int, acceptanceCriterion)
    } else {
      a.getMove(fullSecondObjective, initialObj:Int, acceptanceCriterion)
    }
  }
}
