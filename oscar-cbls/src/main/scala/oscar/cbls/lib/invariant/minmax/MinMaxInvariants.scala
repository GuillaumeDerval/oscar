package oscar.cbls.lib.invariant.minmax

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

import oscar.cbls.core.computation.{IntValue, SetValue}

/**
 * modeling interface presenting the min-max invariants
 * @author renaud.delandtsheer@cetic.be
 */
trait MinMaxInvariants{

  /** Maintains {i in indices of (vars Inter cond) | vars[i] == max(vars(i in indices of (vars Inter cond))}
    * @param vars is an array of IntVar, which can be bulked
    * @param cond is the condition, supposed fully acceptant if not specified
    * @param default is the value returned when cond is empty
    * update is O(log(n))
    * */
  def argMax(vars: Array[IntValue], cond: SetValue = null,default:Int = Int.MinValue) = ArgMax(vars, cond,default)


  /** Maintains {i in indices of (varss Inter cond) | varss[i] == min(varss(i in indices of (varss Inter cond))}
    * @param varss is an array of IntVar, which can be bulked
    * @param ccond is the condition, supposed fully acceptant if not specified (must be specified if varss is bulked)
    * @param default is the value returned when cond is empty
    * update is O(log(n))
    * */
  def argMin(varss: Array[IntValue], ccond: SetValue = null, default:Int = Int.MaxValue) = ArgMin(varss, ccond, default)

  /** maintains output = Max(a,b)
    * where output, a, and b are an IntVar
    * use this if you only have two variables to max, otherwise, refer to log iplementations
    * */
  def max2(a: IntValue, b: IntValue) = Max2(a, b)

  /** maintains output = Min(a,b)
    * where output, a, and b are an IntVar
    * use this if you only have two variables to max, otherwise, refer to log iplementations
    * */
  def min2(a: IntValue, b: IntValue) = Min2(a: IntValue, b: IntValue)

  /** Maintains Max(Var(i) | i in cond)
    * @param varss is an array of IntVar, which can be bulked
    * @param ccond is the condition, supposed fully acceptant if not specified (must be specified if varss is bulked)
    * update is O(log(n))
    * */
  def maxArray(varss: Array[IntValue], ccond: SetValue = null, default: Int = Int.MinValue) = MaxArray(varss, ccond, default)

  /** Maintains Min(Var(i) | i in cond)
    * @param varss is an array of IntVar, which can be bulked
    * @param ccond is the condition, supposed fully acceptant if not specified (must be specified if varss is bulked)
    * update is O(log(n))
    * */
  def minArray(varss: Array[IntValue], ccond: SetValue = null, default: Int = Int.MaxValue) = MinArray(varss, ccond, default)

  /** maintains output = Min(v)
    * where
    * * output is an IntVar
    * * v is an IntSetVar
    * @param default is the default value if v is empty
    * update is O(log(n))
    * */
  def minSet(v: SetValue, default: Int = Int.MaxValue) = MinSet(v, default)

  /** maintains output = Max(v)
    * where
    * * output is an IntVar
    * * v is an IntSetVar
    * @param default is the default value if v is empty
    * update is O(log(n))
    * */
  def maxSet(v: SetValue, default: Int = Int.MinValue) = new MaxSet(v, default)
}


