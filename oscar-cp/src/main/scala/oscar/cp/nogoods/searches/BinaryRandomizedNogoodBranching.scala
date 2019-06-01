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

package oscar.cp.nogoods.searches

import oscar.cp._
import oscar.algo.reversible.ReversibleInt
import oscar.cp.nogoods.decisions._

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

/**
  * Abstract Binary Branching:
  * You can specify your variable heuristics
  * @author Pierre Schaus pschaus@gmail.com
  * @author Renaud Hartert ren.hartert@gmail.com
  * @author Steven Gay steven.gay@uclouvain.be
  *
  * @param varHeuris is a variable heuristic, it will select preferably first the unbound
  *        variables(i) such that varHeuris(i) is the smallest
  */
class BinaryRandomizedNogoodBranching(variables: Array[CPIntVar], varHeuris: Int => Double, valHeuris: Int => Int)
  extends NogoodBranching {

  private[this] val cp = variables(0).store
  private[this] val rand = new Random(42)
  private[this] val nVariables = variables.length
  private[this] val indexes = Array.tabulate(nVariables)(i => i)
  private[this] val nBounds = new ReversibleInt(cp, 0)

  @inline private def bound(i: Int): Unit = {
    val id = nBounds.incr() - 1
    val tmp = indexes(id)
    indexes(id) = indexes(i)
    indexes(i) = tmp
  }

  protected def allBounds(): Boolean = {
    var i = nBounds.value
    while (i < nVariables) {
      val varId = indexes(i)
      val variable = variables(varId)
      if (variable.isBound) bound(i)
      else return false
      i += 1
    }
    true
  }

  protected def nextVar(): Int = {
    var equalGood = ArrayBuffer[Int]()
    var i = nBounds.value
    var bestH = varHeuris(indexes(i))
    equalGood += indexes(i)

    i += 1
    while (i < nVariables) {
      val varId = indexes(i)
      val variable = variables(varId)
      if (variable.isBound) bound(i)
      else {
        val h = varHeuris(varId)
        if (h > bestH) {
          equalGood = ArrayBuffer[Int](varId)
          bestH = h
        }
        else if (h == bestH) {
          equalGood += varId
        }
      }
      i += 1
    }
    //return random value in equalGood
    equalGood(rand.nextInt(equalGood.size))
  }

  def nextDecision: Decision = {
    if (allBounds()) null
    else {
      val i = nextVar()
      val variable = variables(i)
      val value = valHeuris(i)
      new Assign(variable, value)
    }
  }
}
