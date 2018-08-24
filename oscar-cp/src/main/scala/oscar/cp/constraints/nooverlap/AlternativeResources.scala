package oscar.cp.constraints.nooverlap

package unifiedunaryconstraint

import oscar.cp._
import oscar.cp.constraints.nooverlap.util.TransitionTimesUtils
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.variables.CPVar

class AlternativeResources(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar], runOnResource: Array[CPIntVar]) extends Constraint(starts(0).store) {
  override def associatedVars(): Iterable[CPVar] = starts ++ durations ++ ends ++ runOnResource

  protected[this] val resourceIds = runOnResource.map(_.toArray.toSet).foldLeft(Set[Int]())(_.union(_))

  override def setup(l: CPPropagStrength): Unit = {
    for (r <- resourceIds){
      s.post(new NoOverlap(starts, durations, ends, runOnResource, r))
    }
  }
}

class AlternativeResourcesTransitionTimes(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar], runOnResource: Array[CPIntVar], transitionMatrix: Array[Array[Int]]) extends AlternativeResources(starts, durations, ends, runOnResource) {
  override def associatedVars(): Iterable[CPVar] = starts ++ durations ++ ends ++ runOnResource

  private[this] val nTasks = starts.length

  protected def postBinaryPrecedences() = {
    for(t1 <- 0 until nTasks; t2 <- 0 until t1 if(transitionMatrix(t1)(t2) > 0 || transitionMatrix(t2)(t1) > 0)){
      val runOnSameResource = (runOnResource(t1) ?=== (runOnResource(t2)))
      val binaryNoOverlap = ((ends(t1) + transitionMatrix(t1)(t2) ?<= starts(t2)) || (ends(t2) + transitionMatrix(t2)(t1) ?<= starts(t1)))
      s.post(runOnSameResource ==> binaryNoOverlap)
    }
  }
  override def setup(l: CPPropagStrength): Unit = {
    for (r <- resourceIds){
      s.post(new NoOverlapTransitionTimes(starts, durations, ends, transitionMatrix, runOnResource, r, false))
    }
    postBinaryPrecedences()
  }
}

class AlternativeResourcesTransitionTimesFamilies(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar], familyMatrix: Array[Array[Int]], family: Array[Int], runOnResource: Array[CPIntVar], resourceId : Int, exactLB: Boolean=false, postBinaryPrecedences: Boolean = true) extends AlternativeResourcesTransitionTimes(starts, durations, ends, runOnResource, TransitionTimesUtils.transitionMatrixFromFamilyMatrix(family, familyMatrix)) {
  override def setup(l: CPPropagStrength): Unit = {
    for (r <- resourceIds){
      s.post(new NoOverlapTransitionTimesFamilies(starts, durations, ends, familyMatrix, family, runOnResource, r, exactLB, false))
    }
    postBinaryPrecedences()
  }
}
