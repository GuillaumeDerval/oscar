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

import oscar.algo.Inconsistency
import oscar.algo.reversible.{ReversibleInt, ReversibleSparseSet}
import oscar.cp.core.{CPPropagStrength, Constraint}
import oscar.cp.core.variables.{CPIntVar, CPVar}


/**
  * @author Pierre Schaus - pschaus@gmail.com
  * @author Guillaume Derval
  */
class AtMostNValue(val x: Array[CPIntVar], val c: CPIntVar) extends Constraint(x(0).store, "At Most NValue") {
  val n = x.size

  val nUnbounds = new ReversibleInt(s,n)
  val unBounds = Array.tabulate(n)(i => i)

  val minVal = x.map(_.min).min
  val maxVal = x.map(_.max).max

  val nVal = maxVal-minVal +1

  val unAssignedValues = new ReversibleSparseSet(s,minVal,maxVal)

  private[this] val values = Array.ofDim[Int](nVal)

  override def associatedVars(): Iterable[CPVar] = x ++ Array(c)

  override def setup(l: CPPropagStrength): Unit = {
    x.foreach(_.callPropagateWhenBind(this))
    c.callPropagateWhenBoundsChange(this)

    propagate()
  }

  override def propagate(): Unit = {
    var i = nUnbounds.value
    var nU = nUnbounds.value
    while (i > 0) {
      i -= 1
      if (x(unBounds(i)).isBound) {
        nU -= 1
        unAssignedValues.removeValue(x(unBounds(i)).min)
        val tmp = unBounds(i)
        unBounds(i) = unBounds(nU)
        unBounds(nU) = tmp

      }
    }
    nUnbounds.value = nU

    c.updateMin(nVal - unAssignedValues.size)

    if ((nVal - unAssignedValues.size) == c.max) {
      var i = unAssignedValues.fillArray(values)
      while (i > 0) {
        i -= 1
        var j = nUnbounds.value
        while (j > 0) {
          j -= 1
          x(unBounds(j)).removeValue(values(i))
        }
      }
      deactivate() //this constraint is satisfied from here
    }
    else if((nVal - unAssignedValues.size) > c.max)
      throw Inconsistency

    /*else if((nVal - unAssignedValues.size) == c.max - 1){

    }*/
  }
  
 

}
