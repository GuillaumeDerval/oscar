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
/******************************************************************************
 * Contributors:
 *     This code has been initially developed by CETIC www.cetic.be
 *         by Renaud De Landtsheer
 ******************************************************************************/


package oscar.cbls.constraints.lib.global

import oscar.cbls.constraints.core.Constraint
import oscar.cbls.invariants.core.computation._
import oscar.cbls.invariants.core.propagation.Checker
import oscar.cbls.invariants.lib.numeric.Sum

import scala.collection.immutable.SortedMap
;

/**implments the sequence constraint:
 *
 * @param variables the "history variables"
 * @param length the length of the sequence
 * @param Max the max number of elements matching pred in all sequences of the history
 * @param predicate a predicate to say which values belong to the constraint
  * @author renaud.delandtsheer@cetic.be
 */
case class Sequence(variables: Array[IntValue], length:Int, Max:Int, predicate:(Int=>Boolean))
  extends Invariant with Constraint{

  assert(Max <= length, "Sequence: Max > length")

  registerStaticAndDynamicDependencyArrayIndex(variables)
  registerConstrainedVariables(variables)

  finishInitialization()

  /**the number of items in the sequence starting here that enforce the predicate*/
  val count:Array[Int] = Array.tabulate(sequences.size)(i => 0)

  /**the violation of the sequence starting here*/
  val violated = Array.tabulate(sequences.size)(i => CBLSIntVar(model,0, 0 to length - Max, "is_violated_sequence" + i))

  for(v <- violated) v.setDefiningInvariant(this)

  /**the violation of a variable is the sum of the violation of each sequence it is involved in*/
  var Violations = SortedMap.empty[IntValue, IntValue]

  for(i <- 0 to variables.length - length){
    Violations = Violations + ((variables(i),Sum(sequencesInvolving(i).map(violated(_)))))
  }

  val Violation = CBLSIntVar(model,0, 0 to variables.length * length ,"sequence_violations")
  Violation.setDefiningInvariant(this)

  for(i <- variables.indices){
    if(predicate(variables(i).value)){
      for(j <- sequencesInvolving(i)){
        count(j) += 1
        if(count(j) > Max){
          violated(j) :+=1
          Violation :+= 1
        }
      }
    }
  }

  private def sequences = 0 to variables.length - length

  /** returns the sequences that involve this position
    *
    * @param i the position
    * @return
    */
  private def sequencesInvolving(i:Int):Range = {
    val lb = 0 max 1+i-length
    val ub = i min variables.length - length
    lb to ub
  }

  @inline
  override def notifyIntChanged(v: ChangingIntValue, i: Int, OldVal: Int, NewVal: Int){
    if (predicate(OldVal)){
      if(!predicate(NewVal)){
        //decrease the count
        for(j <- sequencesInvolving(i)){
          count(j) -= 1
          if(count(j) >= Max){
            violated(j) :-=1
            Violation :-= 1
          }
        }
      }
    }else{
      if(predicate(variables(i).value)){
        //increase the count
        for(j <- sequencesInvolving(i)){
          count(j) += 1
          if(count(j) > Max){
            violated(j) :+=1
            Violation :+= 1
          }
        }
      }
    }
  }

  private def min(a:Int, b:Int):Int = if (a>b) b else a

  def violation(v: Value): IntValue = Violations.getOrElse(v.asInstanceOf[IntValue],null)

  def violation: CBLSIntVar = Violation

  /** To override whenever possible to spot errors in invariants.
    * this will be called for each invariant after propagation is performed.
    * It requires that the Model is instantiated with the variable debug set to true.
    */
  override def checkInternals(c: Checker) {
    val countCheck:Array[Int] = Array.tabulate(sequences.size)(i => 0)
    /**the violation of the sequence starting here*/
    val violatedCheck = Array.tabulate(sequences.size)(i => 0)
    var violationCheck = 0

    for(i <- variables.indices){
      if(predicate(variables(i).value)){
        for(j <- sequencesInvolving(i)){
          countCheck(j) += 1
          if(countCheck(j) > Max){
            violatedCheck(j) +=1
            violationCheck += 1
          }
        }
      }
    }

    for(s <- sequences)c.check(countCheck(s) == count(s),Some("countCheck(s) == count(s)"))
    for(s <- sequences)c.check(violatedCheck(s) == violated(s).value,Some("violatedCheck(s) == violated(s).value"))
    c.check(violationCheck == violation.value)

  }
}
