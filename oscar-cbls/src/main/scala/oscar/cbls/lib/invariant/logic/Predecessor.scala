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
/*******************************************************************************
  * Contributors:
  *     This code has been initially developed by Ghilain Florent.
  ******************************************************************************/

package oscar.cbls.lib.invariant.logic

import oscar.cbls.core.computation._
import oscar.cbls.core.propagation.Checker

/**
 * This invariant maintains the predecessors of each node.
 *
 * Info :
 *  Convention:
      - value 0 to N-1 for routed node in preds array.
      - value N for unrouted node in preds array.
 * @param next the array of successors of each points (deposits and customers) of the VRP.
 * @param V the number of vehicles.
 * @author renaud.delandtsheer@cetic.be
 * */
case class Predecessor(next:Array[IntValue],V:Int)
  extends Invariant
  with IntNotificationTarget{

  val N = next.length
  registerStaticAndDynamicDependencyArrayIndex(next)
  finishInitialization()
  val preds:Array[CBLSIntVar] = Array.tabulate(N)(i => if (i<V) CBLSIntVar(model, i, 0 to N, "preds" + i)
    else CBLSIntVar(model, N, 0 to N, "preds" + i))

  for(p <- preds) p.setDefiningInvariant(this)

  def length = N
  def apply(i:Int) = preds(i)

  override def notifyIntChanged(v:ChangingIntValue,index:Int,OldVal:Int,NewVal:Int){
    assert(next(index) == v)
    // it unroutes a node
    if(NewVal == N) preds(index) := N
    else preds(NewVal) := index
  }

  override def checkInternals(c:Checker){
    for(n<- 0 until N){
      //n is unrouted
      if(next(n).value==N) c.check(preds(n).value==N, Some("preds(n).value==N"))
      // n is routed
      else  c.check(n == preds(next(n).value).value, Some("n == preds(next(n).value).value"))
      }
  }

  override def toString ={
    var toReturn = ""
    toReturn +="\npreds array: ["
    for (v <- preds){toReturn += (""+v.newValue +",")}
    toReturn = toReturn.substring(0, toReturn.length - 1)+"]\n"
    toReturn
  }
}

