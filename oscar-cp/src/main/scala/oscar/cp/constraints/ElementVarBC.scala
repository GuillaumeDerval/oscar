/**
 * *****************************************************************************
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
 * ****************************************************************************
 */

package oscar.cp.constraints

import oscar.cp.core.CPOutcome
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.variables.CPIntVar
import oscar.cp.core.Constraint
import oscar.cp.util.ArrayUtils
import oscar.algo.reversible.ReversibleInt
import scala.math.min
import scala.math.max
import oscar.cp.core._
import oscar.cp.core.CPOutcome._
import oscar.cp.core.CPSolver
import oscar.cp.core.delta.DeltaIntVar

/**
 * Bound Consistent Element Constraint: y(x) == z
 *
 * @author Pierre Schaus pschaus@gmail.com
 * @author Renaud Hartert ren.hartert@gmail.com
 */
final class ElementVarBC(y: Array[CPIntVar], x: CPIntVar, z: CPIntVar) extends Constraint(y(0).store, "ElementVarBC") {

  priorityL2 = CPStore.MaxPriorityL2 - 1
  
  private[this] var supMin: CPIntVar = null
  private[this] var supMax: CPIntVar = null
  private[this] var zMin: Int = 0
  private[this] var zMax: Int = 0

  private[this] val xValues = new Array[Int](x.size)
  
  override def setup(l: CPPropagStrength): CPOutcome = {
    if (init() == Failure) Failure
    else {
      for (i <- x) y(i).callPropagateWhenBoundsChange(this)
      x.callPropagateWhenDomainChanges(this)
      z.callPropagateWhenBoundsChange(this)
      propagate()
    }
  }

  @inline private def init(): CPOutcome = {
    if (x.updateMin(0) == Failure) Failure
    else if (x.updateMax(y.length - 1) == Failure) Failure
    else propagate()
  }

  override def propagate(): CPOutcome = {
    zMin = z.min
    zMax = z.max
    if (x.isBound) equalityPropagate()
    else {
      if (filterX() == Failure) Failure
      else if (x.isBound) equalityPropagate()
      else if (z.updateMin(supMin.min) == Failure) Failure
      else if (z.updateMax(supMax.max) == Failure) Failure
      else Suspend
    }
  }

  @inline private def equalityPropagate(): CPOutcome = {
    val id = x.min
    val yVar = y(id)
    if (yVar.updateMin(zMin) == Failure) Failure
    else if (yVar.updateMax(zMax) == Failure) Failure
    else if (z.updateMin(yVar.min) == Failure) Failure
    else if (z.updateMax(yVar.max) == Failure) Failure
    else Suspend
  }

  @inline private def filterX(): CPOutcome = {
    var min = Int.MaxValue
    var max = Int.MinValue
    var i = x.fillArray(xValues)
    while (i > 0) {
      i -= 1
      val id = xValues(i)
      val yVar = y(id)
      val yMin = yVar.min
      val yMax = yVar.max
      if (yMax < zMin || yMin > zMax) {
        if (x.removeValue(id) == Failure) return Failure
      } else {
        if (yMin < min) {
          min = yMin
          supMin = yVar
        }
        if (yMax > max) {
          max = yMax
          supMax = yVar
        }
      }
    }
    Suspend
  }
}
