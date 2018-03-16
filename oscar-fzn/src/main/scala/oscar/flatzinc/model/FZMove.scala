/*
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

package oscar.flatzinc.model

/**
  * @author Gustav Bjordal
  */
abstract class FZMove {
  def getControlledVariables:Array[Variable];
}

case class FZAssignMove(lhs: Variable, rhs: Variable) extends FZMove {
  override def getControlledVariables: Array[Variable] = {
    Array(lhs)
  }

  override def toString: String = {"Assign("+lhs+", "+rhs+")"}
}

case class FZAssignArrayMove(lhs: Array[Variable], index: Variable, rhs: Variable) extends FZMove {
  override def getControlledVariables: Array[Variable] = {
    lhs
  }
  override def toString: String = {"AssignArray("+lhs.mkString("[",", ","]")+", "+index+", "+rhs+")"}

}

case class FZSwapMove(lhs: Variable, rhs: Variable) extends FZMove {
  override def getControlledVariables: Array[Variable] = {
    Array(lhs)
  }
  override def toString: String = {"Swap("+lhs+", "+rhs+")"}

}

case class FZSwapArrayMove(lhs: Array[Variable], leftIndex: Variable, rhs: Array[Variable], rightIndex: Variable) extends FZMove {
  override def getControlledVariables: Array[Variable] = {
    lhs ++ rhs
  }
  override def toString: String = {"SwapArray("+lhs.mkString("[",", ","]")+", "+leftIndex+", "+rhs.mkString("[",", ","]")+", "+rightIndex+")"}

}