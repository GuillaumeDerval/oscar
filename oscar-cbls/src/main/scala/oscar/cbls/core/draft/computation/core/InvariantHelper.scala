package oscar.cbls.core.draft.computation.core

import oscar.cbls.core.draft.computation.ChangingSetValue
import oscar.cbls.core.draft.computation.old.ChangingIntValue

object InvariantHelper{

  def getMinMaxBounds(variables:Iterable[ChangingIntValue]):(Int,Int) = {
    var MyMax = Int.MinValue
    var MyMin = Int.MaxValue
    for (v <- variables) {
      if (MyMax < v.max) MyMax = v.max
      if (MyMin > v.min) MyMin = v.min
    }
    (MyMin, MyMax)
  }

  def getMinMaxRange(variables:Iterable[ChangingIntValue]):Range = {
    val (min,max) = getMinMaxBounds(variables)
    min to max
  }

  def getMinMaxBoundsInt(variables:Iterable[Int]):(Int,Int) = {
    var MyMax = Int.MinValue
    var MyMin = Int.MaxValue
    for (v <- variables) {
      if (MyMax < v) MyMax = v
      if (MyMin > v) MyMin = v
    }
    (MyMin, MyMax)
  }

  def getMinMaxRangeInt(variables:Iterable[Int]):Range = {
    val (min,max) = getMinMaxBoundsInt(variables)
    min to max
  }

  def getMinMaxBoundsSet(variables:Iterable[ChangingSetValue]):(Int,Int) = {
    var MyMax = Int.MinValue
    var MyMin = Int.MaxValue
    for (v <- variables) {
      if (MyMax < v.max) MyMax = v.max
      if (MyMin > v.min) MyMin = v.min
    }
    (MyMin, MyMax)
  }

  def getMinMaxRangeSet(variables:Iterable[ChangingSetValue]):Range = {
    val (min,max) = getMinMaxBoundsSet(variables)
    min to max
  }

  def arrayToString[T](a:Array[T]):String =
    "[" + a.toList.mkString(",")+"]"
}
