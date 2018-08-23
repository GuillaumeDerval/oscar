package oscar.cbls.business.seqScheduling.neighborhood

import oscar.cbls.business.seqScheduling.model.SchedulingSolver
import oscar.cbls.core.computation.Variable
import oscar.cbls.core.search.{Move, Neighborhood}

abstract class SchedulingMove(schSolver: SchedulingSolver,
                              val neighborhood:Neighborhood,
                              override val neighborhoodName:String = "",
                              override val objAfter: Int)
  extends Move(objAfter, neighborhoodName) {

  override def touchedVariables: List[Variable] = List(schSolver.activitiesSequence)

  def impactedActivities: Iterable[Int]
}
