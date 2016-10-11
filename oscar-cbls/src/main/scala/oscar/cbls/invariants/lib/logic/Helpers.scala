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
  *     This code has been initially developed by CETIC www.cetic.be
  *         by Renaud De Landtsheer
  ******************************************************************************/


package oscar.cbls.invariants.lib.logic

import oscar.cbls.invariants.core.computation._
import oscar.cbls.invariants.core.propagation.Checker

/** This is a helper to define an invariant from an Int -> Int function.
  * Ths invariant is not incremental, so it should only be used for very simple functions.
  * it maintains output = fun(a)
  * @param a the parameter of the function
  * @param fun the function to maintain, it is supposed not to listen to any variable in the model
  * @param domain the expected domain of the output
  * @param cached set to true to have a cache of size 1, zero to have no cache. cache can provide speedup if fun is time-consuming
  * @author renaud.delandtsheer@cetic.be
  * */
class Int2Int(a:IntValue, fun:Int => Int, domain:Domain = FullRange,cached:Boolean = false) extends IntInvariant(fun(a.value),domain) with IntNotificationTarget{

  registerStaticAndDynamicDependency(a)
  finishInitialization()
  this := fun(a.value)

  var cachedIn:Int = a.value
  var cachedOut:Int = this.newValue

  @inline
  override def notifyIntChanged(v: ChangingIntValue, id:Int, OldVal: Int, NewVal: Int) {
    assert(v == a)
    if(cached){
      if(NewVal == cachedIn) {
        val tmp = cachedOut
        cachedIn = OldVal
        cachedOut = this.newValue
        this := tmp
      }else{
        cachedIn = OldVal
        cachedOut = this.newValue
        this := fun(NewVal)
      }
    }else{
      this := fun(NewVal)
    }
  }

  override def checkInternals(c:Checker){
    c.check(this.value == fun(a.value), Some("output.value == fun(a.value)"))
  }
}

/** This is a helper to define an invariant from an Int x Int -> Int function.
  * Ths invariant is not incremental, so this should only be used for very simple functions.
  * it maintains output = fun(a,b)
  * @param a the first parameter of the function
  * @param b the second parameter of the function
  * @param fun the function to maintain, it is supposed not to listen to any variable in the model
  * @param domain the expected domain of the output
  * @author renaud.delandtsheer@cetic.be
  * */
class IntInt2Int(a:IntValue, b:IntValue, fun:((Int, Int) => Int), domain:Domain = FullRange)
  extends IntInvariant(fun(a.value,b.value),domain)
  with IntNotificationTarget{

  registerStaticAndDynamicDependenciesNoID(a,b)
  finishInitialization()

  @inline
  override def notifyIntChanged(v: ChangingIntValue, id:Int, OldVal: Int, NewVal: Int) {
    this := fun(a.value,b.value)
  }

  override def checkInternals(c:Checker){
    c.check(this.value == fun(a.value,b.value), Some("output.value == fun(a.value,b.value)"))
  }
}

/** This is a helper to define an invariant from an Int x Int -> Int function.
  * Ths invariant is not incremental, so this should only be used for very simple functions.
  * it maintains output = fun(a,b) The difference with [[oscar.cbls.invariants.lib.logic.IntInt2Int]] is that this one performs the computation only after both variables have been updated.
  * @param a the first parameter of the function
  * @param b the second parameter of the function
  * @param fun the function to maintain, it is supposed not to listen to any variable in the model
  * @param domain the expected domain of the output
  * @author renaud.delandtsheer@cetic.be
  * */
class LazyIntInt2Int(a:IntValue, b:IntValue, fun:((Int, Int) => Int), domain:Domain = FullRange)
  extends IntInvariant(fun(a.value,b.value),domain)
  with IntNotificationTarget{

  registerStaticAndDynamicDependenciesNoID(a,b)
  finishInitialization()

  @inline
  override def notifyIntChanged(v: ChangingIntValue, id:Int, OldVal: Int, NewVal: Int) {
    scheduleForPropagation()
  }

  override def performInvariantPropagation(){
    this := fun(a.value,b.value)
  }

  override def checkInternals(c: Checker){
    c.check(this.value == fun(a.value,b.value), Some("checking output of LazyIntVarIntVar2IntVarFun"))
  }
}
