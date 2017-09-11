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

import oscar.cbls._

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
  def maxNaive(varss: Array[IntValue], ccond: SetValue = null, default: Int = Int.MinValue) = MaxArray(varss, ccond, default)

  /** Maintains Min(Var(i) | i in cond)
    * @param varss is an array of IntVar, which can be bulked
    * @param ccond is the condition, supposed fully acceptant if not specified (must be specified if varss is bulked)
    * update is O(log(n))
    * */
  def minNaive(varss: Array[IntValue], ccond: SetValue = null, default: Int = Int.MaxValue) = MinArray(varss, ccond, default)

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


  /**
   * Maintains Max(Var(i) | i in cond)
   * @param varss is an array of IntVar, which can be bulked
   * @param ccond is the condition, supposed fully acceptant if not specified (must be specified if varss is bulked)
   * update is O(log(n))
   * @author renaud.delandtsheer@cetic.be
   * */
  def maxConstArray(varss: Array[Int], ccond: SetValue, default: Int = Int.MinValue) = MaxConstArray(varss, ccond, default)

  /**
   * Maintains Min(Var(i) | i in cond)
   * @param varss is an array of Int
   * @param ccond is the condition, supposed fully acceptant if not specified (must be specified if varss is bulked)
   * update is O(log(n))
   * @author renaud.delandtsheer@cetic.be
   * */
  def minConstArray(varss: Array[Int], ccond: SetValue, default: Int = Int.MaxValue) = MinConstArray(varss, ccond, default)

  /**
   * Maintains Max(Var(i) | i in cond)
   * this is a variant that is lazy, and maintains a TODO-list of postponed updates.
   * postponed updates are ones that do not impact on the outout of the invariant.
   * when there is an update, it is first checked against the TODO-list, for cancellation.
   * if the update does not impact the output, it is postponed
   * if it affects the output, it is performed
   * @param varss is an array of IntVar, which can be bulked
   * @param ccond is the condition, supposed fully acceptant if not specified (must be specified if varss is bulked)
   * @param default the value if ccond is empty
   * @param maxBackLogSize is the maximal number of postponed updates (TODOlist is handled as a FIFO)
   * update is O(log(n)), faster (O(1) if you do updates and backtracks
   * @author renaud.delandtsheer@cetic.be
   * */
  def maxConstArrayLazy(varss: Array[Int], ccond: SetValue, default: Int = Int.MaxValue, maxBackLogSize:Int = 10) =
    MaxConstArrayLazy(varss, ccond, default, maxBackLogSize)


  /**
   * Maintains Min(Var(i) | i in cond)
   * this is a variant that is lazy, and maintains a TODO-list of postponed updates.
   * postponed updates are ones that do not impact on the outout of the invariant.
   * when there is an update, it is first checked against the TODO-list, for cancellation.
   * if the update does not impact the output, it is postponed
   * if it affects the output, it is performed
   * @param varss is an array of Int
   * @param ccond is the condition, supposed fully acceptant if not specified (must be specified if varss is bulked)
   * @param default the value if ccond is empty
   * @param maxBackLogSize is the maximal number of postponed updates (TODOlist is handled as a FIFO)
   * update is O(log(n)), faster (O(1) if you do updates and backtracks
   * @author renaud.delandtsheer@cetic.be
   * */
  def minConstArrayLazy(varss: Array[Int], ccond: SetValue, default: Int = Int.MaxValue, maxBackLogSize:Int = Int.MaxValue)
   = MinConstArrayLazy(varss, ccond, default, maxBackLogSize)


  /**
   *
   * @param constArray
   * @param condSet
   * @param default
   * @param maxDiameter is the maximal number of values in condSet that are monitored in the set, must be >=1.
   *                    the actual diameter is kept between 1 and tis value, lazily
   */
  def minConstArrayValueWise(constArray: Array[Int], condSet: SetValue, default: Int, maxDiameter:Int = 2) =
    new MinConstArrayValueWise(constArray, condSet, default, maxDiameter)


}


