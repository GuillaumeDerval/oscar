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

package oscar.cbls.lib.invariant.numeric

import oscar.cbls.core.computation._
import oscar.cbls.core.propagation._

import scala.collection.immutable.SortedSet


/** sum(i in cond) vars(i)
  * @param vars is an array of IntVars
  * @param cond is the condition for selecting variables in the array of summed ones, cannot be null
  * @author renaud.delandtsheer@cetic.be
  * */
case class SumConstants(vars: Array[Int], cond: SetValue)
  extends IntInvariant(cond.value.foldLeft(0)((acc, i) => acc + vars(i)))
  with SetNotificationTarget{

  registerStaticAndDynamicDependency(cond)
  finishInitialization()

  override def notifySetChanges(v: ChangingSetValue, d: Int, addedValues: Iterable[Int], removedValues: Iterable[Int], oldValue: SortedSet[Int], newValue: SortedSet[Int]) : Unit = {
    for (added <- addedValues)  this :+= vars(added)
    for(deleted <- removedValues) this :-= vars(deleted)
  }

  /** To override whenever possible to spot errors in invariants.
    * this will be called for each invariant after propagation is performed.
    * It requires that the Model is instantiated with the variable debug set to true.
    */
  override def checkInternals() {
    require(this.value == cond.value.foldLeft(0)((acc, i) => acc + vars(i)),
      Some("output.value == cond.value.foldLeft(0)((acc, i) => acc + vars(i).value)"))
  }
}

/** sum(i in cond) vars(i)
 * @param vars is an array of IntVars
 * @param cond is the condition for selecting variables in the array of summed ones, cannot be null
  * @author renaud.delandtsheer@cetic.be
  * */
case class SumElements(vars: Array[IntValue], cond: SetValue)
  extends IntInvariant(initialValue=cond.value.foldLeft(0)((acc, i) => acc + vars(i).value))
  with Bulked[IntValue, Unit]
  with VaryingDependencies
  with IntNotificationTarget
  with SetNotificationTarget{

  assert(vars.size > 0, "Invariant SumElements declared with zero vars to max")
  assert(cond != null, "cond cannot be null for SumElements")

  val keyForRemoval: Array[KeyForElementRemoval] =  Array.fill(vars.length) {null}

  registerStaticDependency(cond)
  registerDeterminingDependency(cond)

  bulkRegister(vars)

  for(i <- cond.value){
    keyForRemoval(i) = registerDynamicDependency(vars(i),i)
  }
  finishInitialization()

  @inline
  override def notifyIntChanged(v: ChangingIntValue, index: Int, OldVal: Int, NewVal: Int) {
    //it is always a listened one, but we could check this here
    assert(vars(index)==v)
    assert(keyForRemoval(index)!=null)
    this :+= (NewVal - OldVal)
  }

  override def notifySetChanges(v: ChangingSetValue, d: Int, addedValues: Iterable[Int], removedValues: Iterable[Int], oldValue: SortedSet[Int], newValue: SortedSet[Int]) : Unit = {
    for (added <- addedValues) notifyInsertOn(v: ChangingSetValue, added)
    for(deleted <- removedValues) notifyDeleteOn(v: ChangingSetValue, deleted)
  }

  @inline
  def notifyInsertOn(v: ChangingSetValue, value: Int) {
    assert(v == cond)
    assert(keyForRemoval(value) == null)
    keyForRemoval(value) = registerDynamicDependency(vars(value),value)

    this :+= vars(value).value
  }

  @inline
  def notifyDeleteOn(v: ChangingSetValue, value: Int) {
    assert(v == cond)
    assert(keyForRemoval(value) != null)
    keyForRemoval(value).performRemove()
    keyForRemoval(value) = null

    this :-= vars(value).value
  }

  override def checkInternals() {
    require(this.value == cond.value.foldLeft(0)((acc, i) => acc + vars(i).value),
        Some("output.value == cond.value.foldLeft(0)((acc, i) => acc + vars(i).value)"))
  }
}


/** sum(i in cond) vars(i)
  * @param vars is an array of IntVars
  * @param cond is the condition for selecting variables in the array of summed ones, cannot be null
  * @author renaud.delandtsheer@cetic.be
  * */
case class ProdConstants(vars: Array[Int], cond: SetValue)
  extends IntInvariant()
  with SetNotificationTarget{

  registerStaticAndDynamicDependency(cond)
  finishInitialization()

  var NullVarCount = cond.value.count(i => vars(i) == 0)
  var NonNullProd = cond.value.foldLeft(1)((acc,i) => if(vars(i) == 0){acc}else{acc*vars(i)})
  affectOutput()

  @inline
  private def affectOutput(){
    if (NullVarCount == 0){
      this := NonNullProd
    }else{
      this := 0
    }
  }

  override def notifySetChanges(v: ChangingSetValue, d: Int, addedValues: Iterable[Int], removedValues: Iterable[Int], oldValue: SortedSet[Int], newValue: SortedSet[Int]) : Unit = {
    for (added <- addedValues) notifyInsertOn(v: ChangingSetValue, added)
    for(deleted <- removedValues) notifyDeleteOn(v: ChangingSetValue, deleted)
  }

  @inline
  def notifyInsertOn(v: ChangingSetValue, value: Int) {
    assert(v == cond)

    if(vars(value) == 0){
      NullVarCount += 1
    }else{
      NonNullProd *= vars(value)
    }
    affectOutput()
  }

  @inline
  def notifyDeleteOn(v: ChangingSetValue, value: Int) {

    if(vars(value) == 0){
      NullVarCount -= 1
    }else{
      NonNullProd = NonNullProd / vars(value)
    }
    affectOutput()
  }

  override def checkInternals() {
    require(this.value == cond.value.foldLeft(1)((acc, i) => acc * vars(i)),
      Some("output.value (" + this.value
        + ") == cond.value.foldLeft(1)((acc, i) => acc * vars(i).value) ("
        + cond.value.foldLeft(1)((acc, i) => acc * vars(i)) + ")"))
  }
}


/** prod(i in cond) vars(i)
 * This invariant might modify vars array by cloning some variables to ensure that each variable only appears once.
 * @param vars is a set of IntVars
 * @param cond is the condition for selecting variables in the set of summed ones.
  * @author renaud.delandtsheer@cetic.be
  * */
case class ProdElements(vars: Array[IntValue], cond: SetValue)
  extends IntInvariant with Bulked[IntValue, Unit]
  with VaryingDependencies
  with IntNotificationTarget
  with SetNotificationTarget{

  assert(cond != null, "cond cannot be null for ProdElements")

  val keyForRemoval: Array[KeyForElementRemoval] =  Array.fill(vars.length) {null}

  registerStaticDependency(cond)
  registerDeterminingDependency(cond)

  bulkRegister(vars)

  for(i <- cond.value){
    keyForRemoval(i) = registerDynamicDependency(vars(i),i)
  }

  finishInitialization()

  var NullVarCount = cond.value.count(i => vars(i).value == 0)
  var NonNullProd = cond.value.foldLeft(1)((acc,i) => if(vars(i).value == 0){acc}else{acc*vars(i).value})
  affectOutput()

  @inline
  private def affectOutput(){
    if (NullVarCount == 0){
      this := NonNullProd
    }else{
      this := 0
    }
  }

  @inline
  override def notifyIntChanged(v: ChangingIntValue, index: Int, OldVal: Int, NewVal: Int) {
    //it is always a listened one, but we could check this here
    assert(vars(index) == v)
    assert(keyForRemoval(index)!=null)
    if (OldVal == 0 && NewVal != 0){
      NullVarCount -=1
      NonNullProd *=NewVal
    }else if(OldVal != 0 && NewVal == 0){
      NullVarCount +=1
      NonNullProd =NonNullProd/OldVal
    }else{
      NonNullProd = NonNullProd/OldVal
      NonNullProd = NonNullProd * NewVal
    }
    affectOutput()
  }

  override def notifySetChanges(v: ChangingSetValue, d: Int, addedValues: Iterable[Int], removedValues: Iterable[Int], oldValue: SortedSet[Int], newValue: SortedSet[Int]) : Unit = {
    for (added <- addedValues) notifyInsertOn(v: ChangingSetValue, added)
    for(deleted <- removedValues) notifyDeleteOn(v: ChangingSetValue, deleted)
  }

  @inline
  def notifyInsertOn(v: ChangingSetValue, value: Int) {
    assert(v == cond)
    assert(keyForRemoval(value) == null)
    keyForRemoval(value) = registerDynamicDependency(vars(value),value)

    if(vars(value).value == 0){
      NullVarCount += 1
    }else{
      NonNullProd *= vars(value).value
    }
    affectOutput()
  }

  @inline
  def notifyDeleteOn(v: ChangingSetValue, value: Int) {
    assert(v == cond)
    assert(keyForRemoval(value) != null)

    keyForRemoval(value).performRemove()
    keyForRemoval(value) = null

    if(vars(value).value == 0){
      NullVarCount -= 1
    }else{
      NonNullProd = NonNullProd / vars(value).value
    }
    affectOutput()
  }

  override def checkInternals() {
    require(this.value == cond.value.foldLeft(1)((acc, i) => acc * vars(i).value),
        Some("output.value (" + this.value
            + ") == cond.value.foldLeft(1)((acc, i) => acc * vars(i).value) ("
            + cond.value.foldLeft(1)((acc, i) => acc * vars(i).value) + ")"))
  }
}
