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


package oscar.cbls.invariants.lib.set

import oscar.cbls.invariants.core.computation._
import oscar.cbls.invariants.core.propagation.Checker

import scala.collection.immutable.{SortedMap, SortedSet};

/**
 * left UNION right
 * @param left is an intvarset
 * @param right is an intvarset
 * @author renaud.delandtsheer@cetic.be
 */
case class Union(left: SetValue, right: SetValue)
  extends SetInvariant(left.value.union(right.value),left.min.min(right.min) to left.max.max(right.max)) {
  assert(left != right)

  registerStaticAndDynamicDependency(left)
  registerStaticAndDynamicDependency(right)
  finishInitialization()

  @inline
  override def notifyInsertOn(v: ChangingSetValue, value: Int) {
    assert(left == v || right == v)
    this.insertValue(value)
  }

  @inline
  override def notifyDeleteOn(v: ChangingSetValue, value: Int) {
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

  override def checkInternals(c: Checker) {
    c.check(this.value.intersect(left.value.union(right.value)).size == this.value.size,
      Some("this.value.intersect(left.value.union(right.value)).size == this.value.size"))
  }
}

/**
 * left INTER right
 * @param left is a CBLSSetVar
 * @param right is a CBLSSetVar
 * @author renaud.delandtsheer@cetic.be
 * */
case class Inter(left: SetValue, right: SetValue)
  extends SetInvariant(left.value.intersect(right.value),
    left.min.max(right.min) to left.max.min(right.max)) {

  registerStaticAndDynamicDependency(left)
  registerStaticAndDynamicDependency(right)
  finishInitialization()

  @inline
  override def notifyInsertOn(v: ChangingSetValue, value: Int) {
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
  override def notifyDeleteOn(v: ChangingSetValue, value: Int) {
    assert(left == v || right == v)
    this.deleteValue(value)
  }

  override def checkInternals(c: Checker) {
    c.check(this.value.intersect(left.value.intersect(right.value)).size == this.value.size,
      Some("this.value.intersect(left.value.intersect(right.value)).size == this.value.size"))
  }
}

case class SetMap(a: SetValue, fun: Int=>Int,
               initialDomain:Domain = FullRange)
  extends SetInvariant(SortedSet.empty, initialDomain) {

  registerStaticAndDynamicDependency(a)
  finishInitialization()

  var outputCount:SortedMap[Int,Int] = SortedMap.empty

    for(v <- a.value){
      val mappedV = fun(v)
      val oldCount = outputCount.getOrElse(mappedV,0)
      if(oldCount == 0){
        this :+= mappedV
      }
      outputCount += ((mappedV, oldCount+1))
    }

  @inline
  override def notifyInsertOn(v: ChangingSetValue, value: Int) {
    val mappedV = fun(value)
    val oldCount = outputCount.getOrElse(mappedV,0)
    if(oldCount == 0){
      this :+= mappedV
    }
    outputCount += ((mappedV, oldCount+1))
  }

  @inline
  override def notifyDeleteOn(v: ChangingSetValue, value: Int) {
    val mappedV = fun(value)
    val oldCount = outputCount.getOrElse(mappedV,0)
    if(oldCount == 1){
      this :-= mappedV
    }
    outputCount += ((mappedV, oldCount-1))

  }

  override def checkInternals(c: Checker) {
    c.check(this.value.intersect(a.value.map(fun)).size == this.value.size)
  }
}

/**
 * left MINUS right, the set diff operator
 * @param left is the base set
 * @param right is the set that is removed from left
 * @author renaud.delandtsheer@cetic.be
 * */
case class Diff(left: SetValue, right: SetValue)
  extends SetInvariant(left.value.diff(right.value), left.min to left.max) {

  registerStaticAndDynamicDependency(left)
  registerStaticAndDynamicDependency(right)
  finishInitialization()

  @inline
  override def notifyInsertOn(v: ChangingSetValue, value: Int) {
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
  override def notifyDeleteOn(v: ChangingSetValue, value: Int) {
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

  override def checkInternals(c: Checker) {
    c.check(this.value.intersect(left.value.diff(right.value)).size == this.value.size,
      Some("this.value.intersect(left.value.diff(right.value)).size == this.value.size"))
  }
}

/**
 * #(v) (cardinality)
 * @param v is an IntSetVar, the set of integers to count
 * @author renaud.delandtsheer@cetic.be
 * */
case class Cardinality(v: SetValue)
  extends IntInvariant(v.value.size, 0 to v.max - v.min) {

  registerStaticAndDynamicDependency(v)
  finishInitialization()

  @inline
  override def notifyInsertOn(v: ChangingSetValue, value: Int) {
    assert(v == this.v)
    this :+= 1
  }

  @inline
  override def notifyDeleteOn(v: ChangingSetValue, value: Int) {
    assert(v == this.v)
    this :-= 1
  }

  override def checkInternals(c: Checker) {
    c.check(this.value == v.value.size, Some("this.value == v.value.size"))
  }
}

/**
 * makes an IntSetVar out of a set of IntVar. If several variables have the same value, the value is present only once in the resulting set
 * @param on is a set of IntVar
 * @author renaud.delandtsheer@cetic.be
 * */
case class MakeSet(on: SortedSet[IntValue])
  extends SetInvariant {

  var counts: SortedMap[Int, Int] = on.foldLeft(SortedMap.empty[Int, Int])((acc:SortedMap[Int,Int], intvar:IntValue) => acc + ((intvar.value, acc.getOrElse(intvar.value, 0) + 1)))

  for (v <- on) registerStaticAndDynamicDependency(v)
  finishInitialization()

    this := SortedSet.empty[Int] ++ counts.keySet


  @inline
  override def notifyIntChanged(v: ChangingIntValue, OldVal: Int, NewVal: Int) {
    assert(on.contains(v), "MakeSet notified for non interesting var :" + on.toList.exists(_==v) + " " + on.toList)

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

  override def checkInternals(c: Checker) {
    c.check(this.value.size <= on.size,
      Some("this.value.size (" + this.value.size
        + ") <= on.size (" + on.size + ")"))
    for (v <- on) c.check(this.value.contains(v.value),
      Some("this.value.contains(v.value (" + v.value + "))"))

    for (v <- this.value) c.check(on.exists(i => i.value == v),
      Some("on.exists(i => i.value == " + v +")"))

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
 * */
case class Interval(lb: IntValue, ub: IntValue)
  extends SetInvariant(initialDomain = lb.min to ub.max) {
  assert(ub != lb)

  registerStaticAndDynamicDependency(lb)
  registerStaticAndDynamicDependency(ub)
  finishInitialization()

    if (lb.value <= ub.value)
      for (i <- lb.value to ub.value) this.insertValue(i)

  @inline
  override def notifyIntChanged(v: ChangingIntValue, OldVal: Int, NewVal: Int) {
    if (v == lb) {
      if (OldVal < NewVal) {
        //intervale reduit
        if (OldVal <= ub.value)
          for (i <- OldVal to (ub.value min (NewVal-1))) this.deleteValue(i)
      }else{
        //intervale plus grand
        if (NewVal <= ub.value)
          for (i <- NewVal to (ub.value min (OldVal-1))) this.insertValue(i)
      }
    } else {
      if (OldVal > NewVal) {
        //intervale reduit
        if (lb.value <= OldVal)
          for (i <- (NewVal+1) max lb.value to OldVal) this.deleteValue(i)
      }else{
        //intervale plus grand
        if (lb.value <= NewVal)
          for (i <- (OldVal+1) max lb.value to NewVal) this.insertValue(i)
      }
    }
  }

  override def checkInternals(c: Checker) {
    c.check(this.value.size == 0.max(ub.value - lb.value + 1),
      Some("this.value.size (" + this.value.size
        + ") == 0.max(ub.value (" + ub.value
        + ") - lb.value (" + lb.value + ") + 1) ("
        + 0.max(ub.value - lb.value + 1) + ")"))
    if (ub.value >= lb.value) {
      for (i <- lb.value to ub.value)
        c.check(this.value.contains(i),
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
 * */
case class TakeAny(from: SetValue, default: Int)
  extends IntInvariant(default, from.min to from.max) {

  registerStaticAndDynamicDependency(from)
  finishInitialization()

  var wasEmpty: Boolean = false

    wasEmpty = from.value.isEmpty
    if (wasEmpty) {
      this := default
    } else {
      this := from.value.head
    }

  override def notifyInsertOn(v: ChangingSetValue, value: Int) {
    if (wasEmpty) {
      this := value
      wasEmpty = false
    }
  }

  override def notifyDeleteOn(v: ChangingSetValue, value: Int) {
    if (value == this.getValue(true)) {
      if (v.value.isEmpty) {
        this := default
        wasEmpty = true
      } else {
        this := from.value.head
      }
    }
  }

  override def checkInternals(c: Checker) {
    if (from.value.isEmpty) {
      c.check(this.value == default,
        Some("this.value (" + this.value
          + ") == default (" + default + ")"))
    } else {
      c.check(from.value.contains(this.value),
        Some("from.value.contains(this.value (" + this.value + "))"))
    }
  }
}

/** an invariant that defines a singleton set out of a single int var.
  * @author renaud.delandtsheer@cetic.be
  */
case class Singleton(v: IntValue)
  extends SetInvariant(SortedSet(v.value),v.domain) {

  registerStaticAndDynamicDependency(v)
  finishInitialization()


  override def checkInternals(c:Checker){
    assert(this.getValue(true).size == 1)
    assert(this.getValue(true).head == v.value)
  }

  override def notifyIntChanged(v:ChangingIntValue,OldVal:Int,NewVal:Int){
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
 * */
case class TakeAnyToSet(from: SetValue)
  extends SetInvariant(SortedSet.empty,from.min to from.max) {

  registerStaticAndDynamicDependency(from)
  finishInitialization()

  var wasEmpty: Boolean = false


    wasEmpty = from.value.isEmpty
    if (wasEmpty) {
      this := SortedSet.empty
    } else {
      this := SortedSet(from.value.head)
    }

  override def notifyInsertOn(v: ChangingSetValue, value: Int) {
    if (wasEmpty) {
      this :+= from.value.head
      wasEmpty = false
    }
  }

  override def notifyDeleteOn(v: ChangingSetValue, value: Int) {
    if (value == this.getValue(true).head){
      if (v.value.isEmpty) {
        this := SortedSet.empty
        wasEmpty = true
      } else {
        this := SortedSet(from.value.head)
      }
    }
  }

  override def checkInternals(c: Checker) {
    if (from.value.isEmpty) {
      c.check(this.value.isEmpty,
        Some("output.value (" + this.value
          + ") is empty set"))
    } else {
      c.check(from.value.contains(this.value.head),
        Some("from.value.contains(output.value (" + this.value.head + "))"))
      c.check(this.value.size == 1,
        Some("output is a singleton"))
    }
  }
}

