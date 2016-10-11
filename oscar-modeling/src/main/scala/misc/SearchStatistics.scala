package misc

case class SPSearchStatistics(nNodes: Int, nFails: Int, time: Long, completed: Boolean,
                              timeInTrail: Long, maxTrailSize: Int, nSols: Int) extends Serializable {
  /**
    * Copy constructor
    */
  def this(a: oscar.algo.search.SearchStatistics) = {
    this(a.nNodes, a.nFails, a.time, a.completed, a.timeInTrail, a.maxTrailSize, a.nSols)
  }

  override val toString: String = s"nNodes: $nNodes\nnFails: $nFails\ntime(ms): $time\ncompleted: $completed\ntimeInTrail: $timeInTrail\nnSols: $nSols\n"
}

case class SearchStatistics(nNodes: Int, nFails: Int, time: Long, completed: Boolean,
                            timeInTrail: Long, maxTrailSize: Int, nSols: Int, timeToLastSolution: Long) extends Serializable {

  override val toString: String = s"nNodes: $nNodes\nnFails: $nFails\ncpu time(ms): $time\nclock time to last solution(ms): $timeToLastSolution\ncompleted: $completed\ntimeInTrail: $timeInTrail\nnSols: $nSols\n"
}