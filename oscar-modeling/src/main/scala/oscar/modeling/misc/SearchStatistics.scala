/*******************************************************************************
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License  for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
 ******************************************************************************/

package oscar.modeling.misc

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

  /**
    * Copy constructor
    */
  def this(a: oscar.algo.search.SearchStatistics) = {
    this(a.nNodes, a.nFails, a.time, a.completed, a.timeInTrail, a.maxTrailSize, a.nSols, 0) //todo
  }

  override val toString: String = s"nNodes: $nNodes\nnFails: $nFails\ncpu time(ms): $time\nclock time to last solution(ms): $timeToLastSolution\ncompleted: $completed\ntimeInTrail: $timeInTrail\nnSols: $nSols\n"
}