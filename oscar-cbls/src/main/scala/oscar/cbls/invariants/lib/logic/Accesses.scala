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
  *            Yoann Guyot
  * ****************************************************************************
  */

package oscar.cbls.invariants.lib.logic

import oscar.cbls.invariants.core.computation._
import oscar.cbls.invariants.core.propagation.{Checker, KeyForElementRemoval}

/**
 * if (ifVar > pivot) then thenVar else elveVar
 * @param ifVar the condition (IntVar)
 * @param thenVar the returned value if ifVar > pivot
 * @param elseVar the returned value if ifVar <= pivot
 * @author renaud.delandtsheer@cetic.be
 * */
case class IntITE(ifVar: IntValue, thenVar: IntValue, elseVar: IntValue, pivot: Int = 0)
  extends IntInvariant(if(ifVar.value > pivot) thenVar.value else elseVar.value, thenVar.domain union elseVar.domain)
  with VaryingDependencies {

  var KeyToCurrentVar: KeyForElementRemoval = null

  registerStaticDependencies(ifVar, thenVar, elseVar)
  registerDeterminingDependency(ifVar)
  KeyToCurrentVar = registerDynamicDependency(if (ifVar.value > pivot) thenVar else elseVar)
  finishInitialization()

  @inline
  override def notifyIntChanged(v: ChangingIntValue, OldVal: Int, NewVal: Int) {
    if (v == ifVar) {
      if (NewVal > pivot && OldVal <= pivot) {
        //modifier le graphe de dependances
        KeyToCurrentVar.performRemove()
        KeyToCurrentVar = registerDynamicDependency(thenVar)
        this := thenVar.value
      } else if (NewVal <= pivot && OldVal > pivot) {
        //modifier le graphe de dependances
        KeyToCurrentVar.performRemove()
        KeyToCurrentVar = registerDynamicDependency(elseVar)
        this := elseVar.value
      }
    } else { //si c'est justement celui qui est affiche.
      this := NewVal
    }
  }

  override def toString: String = {
    "ITE(" + ifVar + ',' + thenVar + "," + elseVar + ")"
  }

  override def checkInternals(c: Checker) {
    c.check(this.value == (if (ifVar.value <= pivot) elseVar.value else thenVar.value),
      Some("output.value (" + this.value
        + ") == (if (ifVar.value (" + ifVar.value + ") <= " + pivot + ") elseVar.value (" + elseVar.value
        + ") else thenVar.value (" + thenVar.value + "))"))
  }
}

case class ConstantIntElement(index: IntValue, inputArray: Array[Int])
  extends Int2Int(index, inputArray(_), InvariantHelper.getMinMaxRangeInt(inputArray))

/**
 * inputarray[index]
 * @param inputarray is an array of IntVar
 * @param index is the index accessing the array
 * @author renaud.delandtsheer@cetic.be
 * */
case class IntElement(index: IntValue, inputarray: Array[IntValue])
  extends IntInvariant(initialValue = inputarray(index.value).value)
  with Bulked[IntValue, Domain]
  with VaryingDependencies {

  registerStaticDependency(index)
  registerDeterminingDependency(index)

  restrictDomain(bulkRegister(inputarray))

  var KeyToCurrentVar = registerDynamicDependency(inputarray(index.value))

  finishInitialization()

  override def performBulkComputation(bulkedVar: Array[IntValue]): Domain = {
    InvariantHelper.getMinMaxRange(bulkedVar)
  }

  @inline
  override def notifyIntChanged(v: ChangingIntValue, OldVal: Int, NewVal: Int) {
    if (v == index) {
      //modifier le graphe de dependances
      KeyToCurrentVar.performRemove()
      KeyToCurrentVar = registerDynamicDependency(inputarray(NewVal))
      this := inputarray(NewVal).value
    } else { //si c'est justement celui qui est affiche.
      assert(v == inputarray.apply(index.value), "access notified for non listened var")
      this := NewVal
    }
  }

  override def checkInternals(c: Checker) {
    c.check(this.value == inputarray(index.value).value,
      Some("output.value (" + this.value + ") == inputarray(index.value ("
        + index.value + ")).value (" + inputarray(index.value).value + ")"))
  }

  override def toString: String = {
    val inputs = inputarray.toList
    if(inputs.length > 4){
      "Array(" +inputs.take(4).map(_.toString).mkString(",") + ", ...)"+ "[" + index.toString + "]"
    }else{
      "Array(" +inputs.map(_.toString).mkString(",") + ", ...)"+ "[" + index.toString + "]"
    }
  }
}

/**
 * inputarray[index]
 * @param inputarray is an array of Int
 * @param index is the index accessing the array (its value must be always inside the array range)
 * @author renaud.delandtsheer@cetic.be
 * @author jean-noel.monette@it.uu.se
 * */
case class IntElementNoVar(index: IntValue, inputarray: Array[Int])
  extends IntInvariant(initialValue = inputarray(index.value),DomainRange(inputarray.min,inputarray.max)) {

  registerStaticAndDynamicDependency(index)
  finishInitialization()

  @inline
  override def notifyIntChanged(v: ChangingIntValue, OldVal: Int, NewVal: Int) {
   // println(OldVal + " "+ NewVal)
    this := inputarray(NewVal)
  }

  override def checkInternals(c: Checker) {
    c.check(this.value == inputarray(index.value),
      Some("output.value (" + this.value + ") == inputarray(index.value ("
        + index.value + ")) (" + inputarray(index.value) + ")"))
  }

  override def toString: String = {
    val inputs = inputarray.toList
    if(inputs.length > 4){
      "Array(" +inputs.take(4).map(_.toString).mkString(",") + ", ...)"+ "[" + index.toString + "] = " + this.value
    }else{
      "Array(" +inputs.map(_.toString).mkString(",") + ", ...)"+ "[" + index.toString + "] = " + this.value
    }
  }
}

/**
 * Union(i in index) (array[i])
 * @param index is an IntSetVar denoting the set of positions in the array to consider
 * @param inputarray is the array of intvar that can be selected by the index
 * @author renaud.delandtsheer@cetic.be
 * */
case class Elements[T <:IntValue](index: SetValue, inputarray: Array[T])
  extends SetInvariant
  with Bulked[T, Domain]
  with VaryingDependencies {

  val KeysToInputArray: Array[KeyForElementRemoval] = new Array(inputarray.length)

  registerStaticDependency(index)
  registerDeterminingDependency(index)

  restrictDomain(bulkRegister(inputarray))

  for (v <- index.value) KeysToInputArray(v) = registerDynamicDependency(inputarray(v), v)

  finishInitialization()

  //this array is the number of elements with value i-myMin
  var ValueCount: Array[Int] = Array.tabulate(max-min+1)(_ => 0)

  for (arrayPosition <- index.value) {
    val value = inputarray(arrayPosition).value
    internalInsert(value)
  }

  override def performBulkComputation(bulkedVar: Array[T]): Domain =
    InvariantHelper.getMinMaxRange(bulkedVar)

  @inline
  override def notifyIntChanged(v: ChangingIntValue, OldVal: Int, NewVal: Int) {
    internalDelete(OldVal)
    internalInsert(NewVal)
  }

  @inline
  override def notifyInsertOn(v: ChangingSetValue, value: Int) {
    assert(index == v)
    KeysToInputArray(value) = registerDynamicDependency(inputarray(value))
    val NewVal: Int = inputarray(value).value

    internalInsert(NewVal)
  }

  @inline
  override def notifyDeleteOn(v: ChangingSetValue, value: Int) {
    assert(index == v)
    assert(KeysToInputArray(value) != null)

    KeysToInputArray(value).performRemove()
    KeysToInputArray(value) = null

    val OldVal:Int = inputarray(value).value
    internalDelete(OldVal)

  }

  private def internalInsert(value:Int){
    if (ValueCount(value - min) == 0){
      ValueCount(value - min) = 1
      this :+= value
    }else{
      ValueCount(value - min) += 1
    }
    assert(ValueCount(value - min) > 0)
  }

  private def internalDelete(value:Int){
    assert(ValueCount(value - min) > 0)
    if (ValueCount(value - min) == 1){
      ValueCount(value - min) = 0
      this :-= value
    }else{
      ValueCount(value - min) -= 1
    }
  }

  override def checkInternals(c: Checker) {
    c.check(KeysToInputArray.indices.forall(i => (KeysToInputArray(i) != null) == index.value.contains(i)),
      Some("KeysToInputArray.indices.forall(i => ((KeysToInputArray(i) != null) == index.value.contains(i)))"))
    c.check(index.value.forall((i: Int) =>
      this.value.contains(inputarray(i).value)),
      Some("index.value.forall((i: Int) => output.value.contains(inputarray(i).value))"))
    c.check(this.value.size <= index.value.size,
      Some("output.value.size (" + this.value.size + ") <= index.value.size (" + index.value.size + ")"))
  }

  override def toString: String = {
    val inputs = inputarray.toList
    if(inputs.length > 4){
      "Array(" +inputs.take(4).map(_.toString).mkString(",") + ", ...)"+ "[" + index.toString + "]"
    }else{
      "Array(" +inputs.map(_.toString).mkString(",") + ", ...)"+ "[" + index.toString + "]"
    }
  }
}

/**
 * inputarray[index] on an array of IntSetVar
 * @param inputarray is the array of intsetvar
 * @param index is the index of the array access
 * @author renaud.delandtsheer@cetic.be
 * */
case class SetElement(index: IntValue, inputarray: Array[SetValue])
  extends SetInvariant(inputarray.apply(index.value).value) with Bulked[SetValue, Domain] with VaryingDependencies {

  var KeyToCurrentVar: KeyForElementRemoval = null

  registerStaticDependency(index)
  registerDeterminingDependency(index)

  restrictDomain(bulkRegister(inputarray))

  KeyToCurrentVar = registerDynamicDependency(inputarray(index.value))

  finishInitialization()

  override def performBulkComputation(bulkedVar: Array[SetValue]): Domain =
    InvariantHelper.getMinMaxBoundsSet(bulkedVar)

  @inline
  override def notifyIntChanged(v: ChangingIntValue, OldVal: Int, NewVal: Int) {
    assert(v == index)
    //modifier le graphe de dependances
    KeyToCurrentVar.performRemove()
    KeyToCurrentVar = registerDynamicDependency(inputarray(NewVal))
    this := inputarray(NewVal).value
  }

  @inline
  override def notifyDeleteOn(v: ChangingSetValue, value: Int) {
    assert(v == inputarray.apply(index.value))
    this.deleteValue(value)
  }

  @inline
  override def notifyInsertOn(v: ChangingSetValue, value: Int) {
    assert(v == inputarray.apply(index.value))
    this.insertValue(value)
  }

  override def checkInternals(c: Checker) {
    c.check(this.value.intersect(inputarray(index.value).value).size == this.value.size,
      Some("output.value.intersect(inputarray(index.value (" + index.value + ")).value ("
        + inputarray(index.value).value + ")).size ("
        + this.value.intersect(inputarray(index.value).value).size
        + ") == output.value.size (" + this.value.size + ")"))
  }

  override def toString: String = {
    val inputs = inputarray.toList
    if(inputs.length > 4){
      "Array(" +inputs.take(4).map(_.toString).mkString(",") + ", ...)"+ "[" + index.toString + "]"
    }else{
      "Array(" +inputs.map(_.toString).mkString(",") + ", ...)"+ "[" + index.toString + "]"
    }
  }
}
