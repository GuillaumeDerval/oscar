package oscar.cbls.lib.search.combinators

import oscar.cbls._
import oscar.cbls.core.computation.Store
import oscar.cbls.core.objective.{CascadingObjective, FunctionObjective, Objective}
import oscar.cbls.core.search._

import scala.language.postfixOps

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
  def apply(n:Neighborhood,randomizationNeighborhood:Neighborhood, maxRestartWithoutImprovement:Long, obj:Objective) = {
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
  * @param iterationToTemperature a function that inputs the number of moves of a that have been actually taken,
  *                    and outputs a temperature, for use in the criterion
  *                    the number of steps is reset to zero when the combinator is reset
  *                    by default, it is the constant function returning 100L
  * @param base the base for the exponent calculation. default is 2L
  */
class Metropolis(a: Neighborhood, iterationToTemperature: Long => Double = _ => 100, base: Double = 2) extends NeighborhoodCombinator(a) {

  var moveCount = 0L
  var temperatureValue: Double = iterationToTemperature(moveCount)

  override def getMove(obj: Objective, initialObj:Long, acceptanceCriterion: (Long, Long) => Boolean): SearchResult =
    a.getMove(obj, initialObj:Long, acceptation) match {
      case NoMoveFound => NoMoveFound
      case MoveFound(m) => InstrumentedMove(m, notifyMoveTaken _)
    }

  def acceptation(oldObj: Long, newObj: Long): Boolean = {
    val gain = oldObj - newObj
    if (gain > 0L){
      true
    } else {
      // metropolis criterion

      val relativeIncrease = - gain.toFloat / oldObj.toFloat

      //println("relativeIncrease: " + relativeIncrease)
      //println("temp:" + temperatureValue)

      val toReturn = math.random < math.pow(base, - relativeIncrease / temperatureValue)

      //println("metropolis decision: " + toReturn)

      toReturn
    }
  }

  def notifyMoveTaken() {
    moveCount += 1L
    temperatureValue = iterationToTemperature(moveCount)
  }

  //this resets the internal state of the move combinators
  override def reset() {
    super.reset()
    moveCount = 0L
    temperatureValue = iterationToTemperature(moveCount)
  }
}

/**
  * This is a combination of a constraint with an objective function.
  * the idea is to consider the constraint as a weak constraint, and sum this violation to the objective function with weighting.
  * throughout the search, the relative weighing of the constraint is increased until it gets to a strong constraint.
  *
  * @param a the neighborhood to consider
  * @param additionalConstraint an additional constraint, considered as a weak constraint at startup, and gradually, as a strong constraint.
  * @maxValueForObj the maximal value for the objective function and for the constraint (do not exceed MaxInt)
  */
class GeneralizedLocalSsearch(a: Neighborhood, additionalConstraint:Objective, maxValueForObj:Int, switchToStrongAsap:Boolean) extends NeighborhoodCombinator(a) {

  var currentWeightOfObj:Int = maxValueForObj
  val store = additionalConstraint.model
  
  override def reset(): Unit = {
    currentWeightOfObj = maxValueForObj
    super.reset()
  }

  override def getMove(obj: Objective, initialObj: Long, acceptanceCriterion: (Long, Long) => Boolean): SearchResult = {

    if(currentWeightOfObj > 0) {
      //it is still a soft constraint

      def evaluate(): Long = {
        val objValue = obj.value
        if(objValue == Long.MaxValue) objValue
        else (maxValueForObj * additionalConstraint.value) + (currentWeightOfObj * objValue)
      }


      val initValueOFConstaint = additionalConstraint.value
      if(switchToStrongAsap && initValueOFConstaint == 0){
        //we are going GeneralizedLocalSearch, but the strong constraint is fine,
        //we can swith to a string constraint
        currentWeightOfObj = -1
        return getMove(obj, initialObj, acceptanceCriterion)
      }

      a.getMove(
        new FunctionObjective(evaluate, store),
        (maxValueForObj * initValueOFConstaint) + (currentWeightOfObj * initialObj),
        acceptanceCriterion) match {
        case NoMoveFound =>
          //it's time to change the weighting
          if(currentWeightOfObj == 1){
            //we cannot go below this, so now, time for a sprint towards satisfiability
            currentWeightOfObj = 0
            this.getMove(obj, initialObj, acceptanceCriterion)
          } else {
            currentWeightOfObj = currentWeightOfObj / 2
            this.getMove(obj, initialObj, acceptanceCriterion)
          }
        case m: MoveFound =>
          //when do we change the weighting?
          currentWeightOfObj = 1 max (currentWeightOfObj-1)
          MoveFound(new MoveWithOtherObj(m.m, Long.MaxValue))
      }
    }else if(currentWeightOfObj == 0){

      val constraintViolation = additionalConstraint.value

      if(constraintViolation == 0){
        //great, we can just post it as a strong constraint
        currentWeightOfObj = -1
        this.getMove(obj, initialObj, acceptanceCriterion)
      }else{
        //we have a problem; there is a violation and we cannot go smaller, so temporarily, we forget the obj at all
        this.getMove(additionalConstraint, constraintViolation, acceptanceCriterion)
      }
    }else{
      //currentWeightOfObj <0,; just for the symbol of the thing
      a.getMove(new CascadingObjective(additionalConstraint,obj),initialObj,acceptanceCriterion)
    }
  }
}

