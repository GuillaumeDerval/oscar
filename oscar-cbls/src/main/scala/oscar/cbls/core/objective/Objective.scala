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
package oscar.cbls.core.objective

/*******************************************************************************
  * Contributors:
  *     This code has been initially developed by CETIC www.cetic.be
  *         by Renaud De Landtsheer
  ******************************************************************************/

import oscar.cbls.core.computation._

import scala.language.implicitConversions

object Objective{
  implicit def objToChangingIntValue(o:IntVarObjective):ChangingIntValue = o.objective
  implicit def objToFun(o:Objective):()=>Int = ()=>o.value
  implicit def funToObj(f:()=>Int) = new FunctionObjective(f)

  def apply(f:()=>Int,model:Store = null) = new FunctionObjective(f,model)

  implicit def apply(objective:IntValue) =
    objective match {
      case c: ChangingIntValue => new IntVarObjective(c)
      case c: CBLSIntConst => throw new Error("you do not want to have an objective that is actually a constant value!")
    }
}

/**
 * a common class for modeling an objective, and querying its variations on different basic moves
 *
 * All queries on the variation on the objective following moves are performed
 * by explicitly performing the move, evaluating the objective, and backtracking the move.
 *
 * hence all these queries return the value of the objective function after the move, and not a delta
 * as you will find in Comet for instance.
 *
 * The reason is that this value is computed through explicit state change and restore,
 * and since all moves are performed lazily,
 * computing a delta would not enable OscaR.cbls to perform the state restore
 * and the next state change in a single model propagation
 * (you generally call a bunch of such methods in a row to find the move you want to perform actually)
 * hence it would be much less efficient to compute a delta at this level than computing a value.
 *
 * If you need a delta, you should compute it by yourself.
 *
 * @author renaud.delandtsheer@cetic.be
 */
class IntVarObjective(val objective: ChangingIntValue) extends Objective {

  model.registerForPartialPropagation(objective)

  /**
   * This method returns the actual objective value.
   * It is easy to override it, and perform a smarter propagation if needed.
   * @return the actual objective value.
   */
  def value = objective.value

  def model:Store = objective.model

  def detailedString(short:Boolean,indent:Int = 0):String = "IntVarObjective(" + objective + ")"
}

/**
 * if (objective1.value > 0) Int.MaxValue/2 + objective1.value
 *   else objective2.value
 *
 *   this is computed partially both for objective and mustBeZeroObjective
 * @param mustBeZeroObjective
 */
class CascadingObjective(mustBeZeroObjective: Objective, secondObjective:Objective,cascadeSize:Int = Int.MaxValue) extends Objective {

  override def detailedString(short: Boolean, indent:Int = 0): String =
    (if(short) {
      if (mustBeZeroObjective.value == 0) {
        nSpace(indent) + "CascadingObjective(\n" +
          nSpace(indent + 2) + "mustBeZeroObjective :=0 \n" +
          nSpace(indent + 2) + "secondObjective:" + secondObjective.detailedString(true, indent + 2) + "\n" +
          nSpace(indent) + ")"
      } else {
        nSpace(indent) + "CascadingObjective(\n" +
          nSpace(indent + 2) + "mustBeZeroObjective:" + mustBeZeroObjective.detailedString(true, indent + 4) + "\n" +
          nSpace(indent) + ")"
      }
    }else {
      nSpace(indent) + "CascadingObjective(\n" +
        nSpace(indent + 2) + "mustBeZeroObjective:" + mustBeZeroObjective.detailedString(true, indent + 4) + "\n" +
        nSpace(indent + 2) + "secondObjective:" + secondObjective.detailedString(true, indent + 4) + "\n" +
        nSpace(indent) + ")"
    })

  /**
   * This method returns the actual objective value.
   * It is easy to override it, and perform a smarter propagation if needed.
   * @return the actual objective value.
   */
  override def value = {
    val firstObjectiveValue = mustBeZeroObjective.value
    if (firstObjectiveValue!=0) cascadeSize
    else secondObjective.value
  }

  override def model: Store = mustBeZeroObjective.model
}

class FunctionObjective(f:()=>Int, m:Store = null) extends Objective{
  override def model: Store = m

  /**
   * This method returns the actual objective value.
   * It is easy to override it, and perform a smarter propagation if needed.
   * @return the actual objective value.
   */
  override def value: Int = f()

  override def detailedString(short: Boolean,indent:Int = 0): String = nSpace(indent) + "FunctionObjective(" + value + ")"
}

trait Objective {

  protected def nSpace(n:Int):String = if(n <= 0) "" else " " + nSpace(n-1)
  override def toString: String = detailedString(false)
  def detailedString(short:Boolean, indent:Int = 0):String

  def model:Store

  /**
   * This method returns the actual objective value.
   * It is easy to override it, and perform a smarter propagation if needed.
   * @return the actual objective value.
   */
  def value:Int
  def isZero:Boolean = value == 0

  /**returns the value of the objective variable if the two variables a and b were swapped values.
    * This proceeds through explicit state change and restore.
    * this process is efficiently performed as the objective Variable is registered for partial propagation
    * @see registerForPartialPropagation() in [[oscar.cbls.core.computation.Store]]
    */
  def swapVal(a: CBLSIntVar, b: CBLSIntVar): Int = {
    a :=: b
    val newVal = value
    a :=: b
    newVal
  }

  /**returns the value of the objective variable if variable a was assigned the value v.
    * This proceeds through explicit state change and restore.
    * this process is efficiently performed as the objective Variable is registered for partial propagation
    * @see registerForPartialPropagation() in [[oscar.cbls.core.computation.Store]]
    */
  def assignVal(a: CBLSIntVar, v: Int): Int = assignVal(List((a,v)))

  /**returns the value of the objective variable if the assignment described by parameter a was performed
    * This proceeds through explicit state change and restore.
    * this process is efficiently performed as the objective Variable is registered for partial propagation
    * @see registerForPartialPropagation() in [[oscar.cbls.core.computation.Store]]
    */
  def assignVal(a: Iterable[(CBLSIntVar, Int)]): Int = {
    //memorize
    val oldvals: Iterable[(CBLSIntVar, Int)] = a.foldLeft(List.empty[(CBLSIntVar, Int)])(
      (acc, IntVarAndInt) => ((IntVarAndInt._1, IntVarAndInt._1.value)) :: acc)
    //excurse
    for (assign <- a)
      assign._1 := assign._2
    val newObj = value
    //undo
    for (assign <- oldvals)
      assign._1 := assign._2
    newObj
  }


  /**returns the value of the objective variable if i is inserted to a
    * this process is efficiently performed as the objective Variable is registered for partial propagation
    * @see registerForPartialPropagation() in [[oscar.cbls.core.computation.Store]]
    */
  def insertValAssumeNotAlreadyIn(a: CBLSSetVar, i:Int): Int = {
    a :+= i
    val newVal = value
    a :-= i
    newVal
  }

  /**returns the value of the objective variable if i is inserted to a
    * this process is efficiently performed as the objective Variable is registered for partial propagation
    * @see registerForPartialPropagation() in [[oscar.cbls.core.computation.Store]]
    */
  def insertVal(a: CBLSSetVar, i:Int): Int = {
    if(a.value.contains(i)) return value
    insertValAssumeNotAlreadyIn(a, i)
  }

  /**returns the value of the objective variable if i is removed from a
    * this process is efficiently performed as the objective Variable is registered for partial propagation
    * @see registerForPartialPropagation() in [[oscar.cbls.core.computation.Store]]
    */
  def removeValAssumeIn(a: CBLSSetVar, i:Int): Int = {
    a :-= i
    val newVal = value
    a :+= i
    newVal
  }

  /**returns the value of the objective variable if i is removed from a
    * this process is efficiently performed as the objective Variable is registered for partial propagation
    * @see registerForPartialPropagation() in [[oscar.cbls.core.computation.Store]]
    */
  def removeVal(a: CBLSSetVar, i:Int): Int = {
    if(!a.value.contains(i)) return value
    removeValAssumeIn(a, i)
  }
}

/**
 * This is a stub that logs the toString of the baseObjective every time its value is needed.
 * these logs can be retrieved through the getAndCleanEvaluationLog method.
 * The value of this objective function is really the one of baseObjective
 *
 * @param baseObjective the value of this objective function
 */
class LoggingObjective(baseObjective:Objective) extends Objective{
  private var evaluationsLog:List[String] = List.empty

  override def detailedString(short: Boolean, indent:Int = 0): String = nSpace(indent) + "LoggingObjective(" + baseObjective.detailedString(short) + ")"

  override def model: Store = baseObjective.model

  override def value: Int = {
    val toReturn = baseObjective.value
    evaluationsLog = baseObjective.detailedString(true) :: evaluationsLog
    toReturn
  }

  def getAndCleanEvaluationLog:List[String] = {
    val toReturn = evaluationsLog
    evaluationsLog = List.empty
    toReturn
  }
}