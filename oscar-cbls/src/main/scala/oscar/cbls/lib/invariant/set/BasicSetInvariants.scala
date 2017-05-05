/**
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
/**
 * *****************************************************************************
 * Contributors:
 *     This code has been initially developed by CETIC www.cetic.be
 *         by Renaud De Landtsheer
 * ****************************************************************************
 */

package oscar.cbls.lib.invariant.set

import oscar.cbls.core.computation._
import oscar.cbls.core.propagation.Checker

import scala.collection.immutable.{SortedMap, SortedSet};

/**
 * left UNION right
 * @param left is an intvarset
 * @param right is an intvarset
 * @author renaud.delandtsheer@cetic.be
 */
case class Union(left: SetValue, right: SetValue)
  extends SetInvariant(left.value.union(right.value), left.min.min(right.min) to left.max.max(right.max))
with SetNotificationTarget{
  require(left != right, "left and right canot be the same instance for Union!")

  registerStaticAndDynamicDependency(left)
  registerStaticAndDynamicDependency(right)
  finishInitialization()

  //TODO: not obvious at all, if left == right!!
  override def notifySetChanges(v: ChangingSetValue, d: Int, addedValues: Iterable[Int], removedValues: Iterable[Int], oldValue: SortedSet[Int], newValue: SortedSet[Int]) : Unit = {
    for (added <- addedValues) notifyInsertOn(v: ChangingSetValue, added)
    for (deleted <- removedValues) notifyDeleteOn(v: ChangingSetValue, deleted)
  }

  @inline
  def notifyInsertOn(v: ChangingSetValue, value: Int) {
    assert(left == v || right == v)
    this.insertValue(value)
  }

  @inline
  def notifyDeleteOn(v: ChangingSetValue, value: Int) {
    assert(left == v || right == v)
    if (v == left) {
      if (!right.value.contains(value)) {
        this.deleteValue(value)
      }
    } else if (v == right) {
      if (!left.value.contains(value)) {
        this.deleteValue(value)
      }
    } else {
      assert(false)
    }
  }

  override def checkInternals() {
    require(this.value.intersect(left.value.union(right.value)).size == this.value.size,
      Some("this.value.intersect(left.value.union(right.value)).size == this.value.size"))
  }
}

/**
 * UNION(sets(0), sets(1), ..., sets(n))
 * @param sets is an iterable of SetValue
 * @author yoann.guyot@cetic.be
 */
case class UnionAll(sets: Iterable[SetValue])
  extends SetInvariant(initialDomain = InvariantHelper.getMinMaxBoundsSet(sets))
  with SetNotificationTarget{

  val count: Array[Int] = Array.fill(this.max - this.min + 1)(0)
  val offset = -this.min

  sets foreach {
    _.value foreach { value =>
      val i = value + offset
      count(i) = count(i) + 1
      if(count(i) == 1) this :+= value
    }
  }

  sets foreach (registerStaticAndDynamicDependency(_))
  finishInitialization()

  override def notifySetChanges(v: ChangingSetValue, d: Int, addedValues: Iterable[Int], removedValues: Iterable[Int], oldValue: SortedSet[Int], newValue: SortedSet[Int]) : Unit = {
    for (added <- addedValues) notifyInsertOn(v: ChangingSetValue, added)
    for(deleted <- removedValues) notifyDeleteOn(v: ChangingSetValue, deleted)
  }

  @inline
  def notifyInsertOn(v: ChangingSetValue, value: Int) {
    assert(sets.exists(_ == v))
    
    val i = value + offset
    
    if (count(i) == 0) {
      this.insertValue(value)
    }
    count(i) = count(i) + 1
  }

  @inline
  def notifyDeleteOn(v: ChangingSetValue, value: Int) {
    assert(sets.exists(_ == v))
    
    val i = value + offset
    assert(count(i) >= 1)
    
    if (count(i) == 1) this.deleteValue(value)
    count(i) = count(i) - 1
  }

  override def checkInternals() {
    this.min to this.max foreach {
      value =>
        require(this.value.iterator.contains(value) == (count(value - offset) > 0),
          Some("this.value.iterator.contains(value) == (count(value (" + value + ") - offset (" + offset + ")) > 0)"))
    }
  }
}

/**
 * left INTER right
 * @param left is a CBLSSetVar
 * @param right is a CBLSSetVar
 * @author renaud.delandtsheer@cetic.be
 */
case class Inter(left: SetValue, right: SetValue)
  extends SetInvariant(left.value.intersect(right.value),
    left.min.max(right.min) to left.max.min(right.max))
with SetNotificationTarget{
  require(left != right,"left and right cannot hte the same instance for Inter")
  registerStaticAndDynamicDependency(left)
  registerStaticAndDynamicDependency(right)
  finishInitialization()

  //TODO: handle left == right!
  override def notifySetChanges(v: ChangingSetValue, d: Int, addedValues: Iterable[Int], removedValues: Iterable[Int], oldValue: SortedSet[Int], newValue: SortedSet[Int]) : Unit = {
    for (added <- addedValues) notifyInsertOn(v: ChangingSetValue, added)
    for(deleted <- removedValues) notifyDeleteOn(v: ChangingSetValue, deleted)
  }

  @inline
  def notifyInsertOn(v: ChangingSetValue, value: Int) {
    if (v == left) {
      if (right.value.contains(value)) {
        this.insertValue(value)
      }
    } else if (v == right) {
      if (left.value.contains(value)) {
        this.insertValue(value)
      }
    } else {
      assert(false)
    }
  }

  @inline
  def notifyDeleteOn(v: ChangingSetValue, value: Int) {
    assert(left == v || right == v)
    this.deleteValue(value)
  }

  override def checkInternals() {
    require(this.value.intersect(left.value.intersect(right.value)).size == this.value.size,
      Some("this.value.intersect(left.value.intersect(right.value)).size == this.value.size"))
  }
}

case class SetMap(a: SetValue, fun: Int => Int,
                  initialDomain: Domain = FullRange)
  extends SetInvariant(SortedSet.empty, initialDomain)
with SetNotificationTarget{

  registerStaticAndDynamicDependency(a)
  finishInitialization()

  var outputCount: SortedMap[Int, Int] = SortedMap.empty

  for (v <- a.value) {
    val mappedV = fun(v)
    val oldCount = outputCount.getOrElse(mappedV, 0)
    if (oldCount == 0) {
      this :+= mappedV
    }
    outputCount += ((mappedV, oldCount + 1))
  }

  override def notifySetChanges(v: ChangingSetValue, d: Int, addedValues: Iterable[Int], removedValues: Iterable[Int], oldValue: SortedSet[Int], newValue: SortedSet[Int]) : Unit = {
    for (added <- addedValues) notifyInsertOn(v: ChangingSetValue, added)
    for(deleted <- removedValues) notifyDeleteOn(v: ChangingSetValue, deleted)
  }

  @inline
  def notifyInsertOn(v: ChangingSetValue, value: Int) {
    val mappedV = fun(value)
    val oldCount = outputCount.getOrElse(mappedV, 0)
    if (oldCount == 0) {
      this :+= mappedV
    }
    outputCount += ((mappedV, oldCount + 1))
  }

  @inline
  def notifyDeleteOn(v: ChangingSetValue, value: Int) {
    val mappedV = fun(value)
    val oldCount = outputCount.getOrElse(mappedV, 0)
    if (oldCount == 1) {
      this :-= mappedV
    }
    outputCount += ((mappedV, oldCount - 1))

  }

  override def checkInternals() {
    require(this.value.intersect(a.value.map(fun)).size == this.value.size)
  }
}

/**
 * left MINUS right, the set diff operator
 * @param left is the base set
 * @param right is the set that is removed from left
 * @author renaud.delandtsheer@cetic.be
 */
case class Diff(left: SetValue, right: SetValue)
  extends SetInvariant(left.value.diff(right.value), left.min to left.max)
  with SetNotificationTarget{

  require(left != right,"left and right cannot he the same ionstance for Diff")
  registerStaticAndDynamicDependency(left)
  registerStaticAndDynamicDependency(right)
  finishInitialization()

  //TODO: handle left == right
  override def notifySetChanges(v: ChangingSetValue, d: Int, addedValues: Iterable[Int], removedValues: Iterable[Int], oldValue: SortedSet[Int], newValue: SortedSet[Int]) : Unit = {
    for (added <- addedValues) notifyInsertOn(v: ChangingSetValue, added)
    for(deleted <- removedValues) notifyDeleteOn(v: ChangingSetValue, deleted)
  }

  @inline
  def notifyInsertOn(v: ChangingSetValue, value: Int) {
    if (v == left) {
      if (!right.value.contains(value)) {
        this.insertValue(value)
      }
    } else if (v == right) {
      if (left.value.contains(value)) {
        this.deleteValue(value)
      }
    } else {
      assert(false)
    }
  }

  @inline
  def notifyDeleteOn(v: ChangingSetValue, value: Int) {
    if (v == left) {
      if (!right.value.contains(value)) {
        this.deleteValue(value)
      }
    } else if (v == right) {
      if (left.value.contains(value)) {
        this.insertValue(value)
      }
    } else {
      assert(false)
    }
  }

  override def checkInternals() {
    require(this.value.intersect(left.value.diff(right.value)).size == this.value.size,
      Some("this.value.intersect(left.value.diff(right.value)).size == this.value.size"))
  }
}

/**
 * #(v) (cardinality)
 * @param v is an IntSetVar, the set of integers to count
 * @author renaud.delandtsheer@cetic.be
 */
case class Cardinality(v: SetValue)
  extends IntInvariant(v.value.size, 0 to (v.max - v.min +1))
  with SetNotificationTarget{

  registerStaticAndDynamicDependency(v)
  finishInitialization()

  override def notifySetChanges(v: ChangingSetValue, d: Int, addedValues: Iterable[Int], removedValues: Iterable[Int], oldValue: SortedSet[Int], newValue: SortedSet[Int]) : Unit = {
   this := newValue.size
  }

  override def checkInternals() {
    require(this.value == v.value.size, Some("this.value == v.value.size"))
  }
}

/**
 * makes an IntSetVar out of a set of IntVar. If several variables have the same value, the value is present only once in the resulting set
 * @param on is a set of IntVar
 * @author renaud.delandtsheer@cetic.be
 */
case class MakeSet(on: SortedSet[IntValue])
  extends SetInvariant
  with IntNotificationTarget{

  var counts: SortedMap[Int, Int] = on.foldLeft(SortedMap.empty[Int, Int])((acc: SortedMap[Int, Int], intvar: IntValue) => acc + ((intvar.value, acc.getOrElse(intvar.value, 0) + 1)))

  for (v <- on) registerStaticAndDynamicDependency(v)
  finishInitialization()

  this := SortedSet.empty[Int] ++ counts.keySet

  @inline
  override def notifyIntChanged(v: ChangingIntValue, id:Int, OldVal: Int, NewVal: Int) {
    assert(on.contains(v), "MakeSet notified for non interesting var :" + on.toList.exists(_ == v) + " " + on.toList)

    assert(OldVal != NewVal)
    if (counts(OldVal) == 1) {
      //on va en supprimer un
      counts = counts - OldVal
      this.deleteValue(OldVal)
    } else {
      //on en supprime pas un
      counts = counts + ((OldVal, counts(OldVal) - 1))
    }
    if (counts.contains(NewVal)) {
      counts = counts + ((NewVal, counts(NewVal) + 1))
    } else {
      counts = counts + ((NewVal, 1))
      this.insertValue(NewVal)
    }
  }

  override def checkInternals() {
    require(this.value.size <= on.size,
      Some("this.value.size (" + this.value.size
        + ") <= on.size (" + on.size + ")"))
    for (v <- on) require(this.value.contains(v.value),
      Some("this.value.contains(v.value (" + v.value + "))"))

    for (v <- this.value) require(on.exists(i => i.value == v),
      Some("on.exists(i => i.value == " + v + ")"))

  }
}

/**
 * makes a set out of an interval specified by a lower bound and an upper bound. if lb > ub, the set is empty.
 * output = if (lb <= ub) [lb; ub] else empty
 *
 * BEWARE: this invariant is not efficient because if you change a bound with a delta of N,
 * it costs n*log(N) to update its output where N is the initial size of the interval
 *
 * @param lb is the lower bound of the interval
 * @param ub is the upper bound of the interval
 * @author renaud.delandtsheer@cetic.be
 */
case class Interval(lb: IntValue, ub: IntValue)
  extends SetInvariant(initialDomain = lb.min to ub.max)
  with IntNotificationTarget{
  assert(ub != lb)

  registerStaticAndDynamicDependency(lb)
  registerStaticAndDynamicDependency(ub)
  finishInitialization()

  if (lb.value <= ub.value)
    for (i <- lb.value to ub.value) this.insertValue(i)

  @inline
  override def notifyIntChanged(v: ChangingIntValue, id:Int, OldVal: Int, NewVal: Int) {
    if (v == lb) {
      if (OldVal < NewVal) {
        //intervale reduit
        if (OldVal <= ub.value)
          for (i <- OldVal to (ub.value min (NewVal - 1))) this.deleteValue(i)
      } else {
        //intervale plus grand
        if (NewVal <= ub.value)
          for (i <- NewVal to (ub.value min (OldVal - 1))) this.insertValue(i)
      }
    } else {
      if (OldVal > NewVal) {
        //intervale reduit
        if (lb.value <= OldVal)
          for (i <- (NewVal + 1) max lb.value to OldVal) this.deleteValue(i)
      } else {
        //intervale plus grand
        if (lb.value <= NewVal)
          for (i <- (OldVal + 1) max lb.value to NewVal) this.insertValue(i)
      }
    }
  }

  override def checkInternals() {
    require(this.value.size == 0.max(ub.value - lb.value + 1),
      Some("this.value.size (" + this.value.size
        + ") == 0.max(ub.value (" + ub.value
        + ") - lb.value (" + lb.value + ") + 1) ("
        + 0.max(ub.value - lb.value + 1) + ")"))
    if (ub.value >= lb.value) {
      for (i <- lb.value to ub.value)
        require(this.value.contains(i),
          Some("this.value.contains(" + i + ")"))
    }
  }
}

/**
 * maintains the output as any value taken from the intset var parameter.
 * if this set is empty, puts the default value ni output.
 * @param from where we take the value from
 * @param default the default value in case from is empty
 * @author renaud.delandtsheer@cetic.be
 */
case class TakeAny(from: SetValue, default: Int)
  extends IntInvariant(default, from.min to from.max)
with SetNotificationTarget{

  registerStaticAndDynamicDependency(from)
  finishInitialization()

  var wasEmpty: Boolean = false

  wasEmpty = from.value.isEmpty
  if (wasEmpty) {
    this := default
  } else {
    this := from.value.head
  }

  override def notifySetChanges(v: ChangingSetValue, d: Int, addedValues: Iterable[Int], removedValues: Iterable[Int], oldValue: SortedSet[Int], newValue: SortedSet[Int]) : Unit = {
    if (wasEmpty) {
      if(newValue.nonEmpty){
        wasEmpty = false
        this := newValue.head
      }
    }else{
      if(newValue.isEmpty){
        this := default
        wasEmpty = true
      }else{
        if (!newValue.contains(this.newValue)){
          this := newValue.head
        }
      }
    }
  }

  override def checkInternals() {
    if (from.value.isEmpty) {
      require(this.value == default,
        Some("this.value (" + this.value
          + ") == default (" + default + ")"))
    } else {
      require(from.value.contains(this.value),
        Some("from.value.contains(this.value (" + this.value + "))"))
    }
  }
}

/**
 * an invariant that defines a singleton set out of a single int var.
 * @author renaud.delandtsheer@cetic.be
 */
case class Singleton(v: IntValue)
  extends SetInvariant(SortedSet(v.value), v.domain)
  with IntNotificationTarget{

  registerStaticAndDynamicDependency(v)
  finishInitialization()

  override def checkInternals() {
    assert(this.value.size == 1)
    assert(this.value.head == v.value)
  }

  override def notifyIntChanged(v: ChangingIntValue, id:Int, OldVal: Int, NewVal: Int) {
    assert(v == this.v)
    //ici, on propage tout de suite, c'est les variables qui font le stop and go.
    this.deleteValue(OldVal)
    this.insertValue(NewVal)
  }
}

/**
 * maintains the output as a singleton containing any one of the values of the from Set.
 * if from is empty,the output set will be empty as well
 * @param from where we take the value from
 * @author renaud.delandtsheer@cetic.be
 */
case class TakeAnyToSet(from: SetValue)
  extends SetInvariant(SortedSet.empty, from.min to from.max)
  with SetNotificationTarget{

  registerStaticAndDynamicDependency(from)
  finishInitialization()

  var wasEmpty: Boolean = false

  wasEmpty = from.value.isEmpty
  if (wasEmpty) {
    this := SortedSet.empty
  } else {
    this := SortedSet(from.value.head)
  }

  override def notifySetChanges(v: ChangingSetValue, d: Int, addedValues: Iterable[Int], removedValues: Iterable[Int], oldValue: SortedSet[Int], newValue: SortedSet[Int]) : Unit = {
    if (wasEmpty) {
      if(newValue.nonEmpty){
        wasEmpty = false
        this :+= newValue.head
      }
    }else{
      if(newValue.isEmpty){
        this := SortedSet.empty
        wasEmpty = true
      }else{
        if (!newValue.contains(this.newValue.head)){
          this := SortedSet(newValue.head)
        }
      }
    }
  }

  override def checkInternals() {
    if (from.value.isEmpty) {
      require(this.value.isEmpty,
        Some("output.value (" + this.value
          + ") is empty set"))
    } else {
      require(from.value.contains(this.value.head),
        Some("from.value.contains(output.value (" + this.value.head + "))"))
      require(this.value.size == 1,
        Some("output is a singleton"))
    }
  }
}


/**
 * implements v \in set
 * @author renaud.delandtsheer@cetic.be
 */
case class BelongsTo(v: IntValue, set: SetValue)
  extends IntInvariant(0,0 to 1)
  with IntNotificationTarget
  with SetNotificationTarget{

  registerStaticAndDynamicDependenciesNoID(v, set)
  finishInitialization()

  override def notifyIntChanged(v: ChangingIntValue, id:Int, OldVal: Int, NewVal: Int) {
    this := (if (set.value.contains(NewVal)) 1 else 0)
  }

  override def notifySetChanges(v: ChangingSetValue, d: Int, addedValues: Iterable[Int],
                                removedValues: Iterable[Int], oldValue: SortedSet[Int], newValue: SortedSet[Int]) : Unit = {
    if((removedValues.nonEmpty && this.newValue == 1) || (addedValues.nonEmpty && this.newValue == 0)){
      this := (if (newValue.contains(this.v.value)) 1 else 0)
    }
  }

  override def checkInternals() {
    require(this.value == (if (set.value.contains(v.value)) 1 else 0))
  }
}