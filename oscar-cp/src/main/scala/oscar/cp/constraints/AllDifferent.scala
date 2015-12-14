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

package oscar.cp.constraints

import oscar.cp.core._
import oscar.cp.core.CPOutcome._
import oscar.cp.constraints._
import oscar.cp.modeling._
import oscar.algo.DisjointSets
import scala.collection.mutable.ArrayBuffer
import oscar.cp.core.variables.CPIntVar

/**
 * Alldifferent constraint
 * @author Pierre Schaus pschaus@gmail.com
 */
class AllDifferent(x: Array[CPIntVar]) extends Constraint(x(0).store) {
  
  
  def this(x: CPIntVar*) =  this(x.toArray)

  /**
   * Post the constraint that for every pair of variables in x[i], x[j], we have x[i] != x[j] <br>
   * Available propagation strength are Weak (default) and Strong
   * @see CPPropagStrength
   * @param x
   */
  override def setup(l: CPPropagStrength): CPOutcome = {

    val ok = l match {
      case CPPropagStrength.Weak => {
        s.post(new AllDiffFWC(x))
      }
      case CPPropagStrength.Medium => {
        val minVal = x.map(_.min).min
        val maxVal = x.map(_.max).max
        val cards = Array.fill(maxVal - minVal + 1)(1)
        s.post(Array(new AllDiffFWC(x), new GCCUpperBC(x, minVal, cards)))
      }
      case CPPropagStrength.Strong => s.post(new AllDiffAC(x))
      case CPPropagStrength.Automatic => {
        val minVal = x.map(_.min).min
        val maxVal = x.map(_.max).max
        val cards = Array.fill(maxVal - minVal + 1)(1)
        s.post(Array(new AllDiffFWC(x), new GCCUpperBC(x, minVal, cards)))        
      }
      

    }
    if (ok == CPOutcome.Failure) return CPOutcome.Failure;
    else return CPOutcome.Success;

  }

}
