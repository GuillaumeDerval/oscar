package oscar.cbls.scheduling.solver

import oscar.cbls.core.objective.{IntVarObjective, Objective}
import oscar.cbls.business.scheduling.model.{Planning, Activity}
import oscar.cbls.core.search.EasyNeighborhood
import oscar.cbls.core.search.Move

/**
 * this neighborhood wants to flatten while minimizing the makepan increase
 * so what is the objective?
 * the objective is the makespan.
 * so this neighborhood will always probably worsen the objective function
 * the problem is that it can chose to minimise the worsening, or maxmize the flattening, and we have to specify the tradeoff at some point
 *
 * idea: tell it where to flatten (a set of points where there is an overhead, and the resource to flatten
 * and ask it to serch for the best move?
 * also there are estimators for he flattening, based on lsd and led, we want to use that!
 * how about and objetive function that uses lsd and led?? this function must understand the flattening and cie operations, but it seems a good idea, actually.
 * so the move will perform a flattening,
 * you need to accept all, and you can chose first or best, and use estimators via the objective function.
 * seems fine
 */

/**
 * this traits adds a scheduling-specific method to estimate
 */
trait SchedulingObjective extends Objective{
  def addDPrecedenceVal(from: Activity, to: Activity): Int = {
    if(to.hasPredecessor(from)) Int.MaxValue
    else {
      to.addDynamicPredecessor(from)
      val toReturn = this.value
      to.removeDynamicPredecessor(from)
      toReturn
    }
  }
}

/**
 * This objective function computes an estimate of the MakeSpan expansion if a given precedence is added.
 * This estimate is completely wrong in itself, as a constant factor is added to each estimate .
 * Since it is the same factor, you can use this method to chose among a set of precedence
 * because this will erase about the correcting factor.
 * the precedence is based on the LSD/LED values of activities.
 * also this estimate is only available through the newDependencyValueEstimate method
 * BEWARE: you cannot use this one if you are playing around with AndThen combinator,
 * but you re still free to use the trait [[SchedulingObjective]] for this purpose
 * */
class MakeSpanWithDifferentialEstimatorThroughLSDLED(p:Planning) extends IntVarObjective(p.makeSpan) with SchedulingObjective{
  override def detailedString(short: Boolean, indent: Int): String = "MakeSpanDifferentialEstimatorThroughLSDLED"

  override def addDPrecedenceVal(from: Activity, to: Activity): Int = {
    from.earliestEndDate.value - to.latestStartDate.value
  }
}

abstract class EasySchedulingNeighborhood[M<:Move](best:Boolean = false, neighborhoodName:String=null) extends EasyNeighborhood[M](best,neighborhoodName){
  var private_obj: SchedulingObjective = null
  override protected def obj_=(o:Objective){
    o match{
      case s:SchedulingObjective => private_obj = s
      case _ => throw new Error("scheduling neighborhood requires scheduling objectives, add eg. \"with SchedulingObjective\" to your objective class")
    }
  }
  override protected def obj: SchedulingObjective = private_obj
}

