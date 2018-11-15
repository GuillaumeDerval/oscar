package oscar.cbls.core.draft.computation

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

import oscar.cbls.core.draft.computation.core.{AtomicInvariant, AtomicNotificationTarget, CBLSAtomicConst, CBLSAtomicVar}

class IntVar(store: Store,
                initialValue: Int,
                minValue:Int = Int.MinValue,
                maxValue:Int = Int.MaxValue,
                givenName: String = null)
  extends CBLSAtomicVar[Int](store: Store,
    initialValue,
    givenName: String ){

  require(minValue <= initialValue && initialValue <= maxValue,
    "initialValue:" + initialValue+ " is not between minValue:" + minValue + " and maxValue:" + maxValue)

  override def value_=(v: Int): Unit = {
    require(minValue <= v && v <= maxValue,
      "assigned value:" + v + " is not between minValue:" + minValue + " and maxValue:" + maxValue)
    super.value_=(v)
  }

  override def createClone:IntVar = {
    val clone = new IntVar(
      store,
      this.value,
      minValue:Int,
      maxValue:Int,
      "clone of " + this.name)

    clone <== this
    clone
  }
}

class IntConst(store:Store, override val value:Int)
  extends CBLSAtomicConst[Int](store, value){
  override def createClone: CBLSAtomicVar[Int] = this
}

class IntInvariant(store:Store,
                      initMinValue:Int = Int.MinValue,
                      initMaxValue:Int = Int.MaxValue,
                      initialValue:Int)
  extends AtomicInvariant[Int](store:Store, initialValue){

  require(minValue <= initialValue && initialValue <= maxValue,
    "initialValue:" + initialValue+ " is not between minValue:" + minValue + " and maxValue:" + maxValue)

  private[this] var _minValue = initMinValue
  private[this] var _maxValue = initMaxValue

  def minValue = _minValue
  def maxValue = _maxValue

  protected def restrictDomain(restrictMinValue:Int,restrictMaxValue:Int): Unit = {
    _minValue = _minValue max restrictMinValue
    _maxValue = _maxValue min restrictMaxValue

    require(minValue <= newValue && newValue <= maxValue,
      "value:" + newValue + " is not between minValue:" + minValue + " and maxValue:" + maxValue)
  }

  override def value_=(v: Int): Unit = {
    require(minValue <= v && v <= maxValue,
      "assigned value:" + v + " is not between minValue:" + minValue + " and maxValue:" + maxValue)
    super.value_=(v)
  }

  override def createClone:IntVar = {
    val clone = new IntVar(
      store,
      minValue:Int,
      maxValue:Int,
      this.value,
      "clone of " + this.name)

    clone <== this
    clone
  }
}

