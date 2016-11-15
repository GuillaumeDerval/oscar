package oscar.cp.constraints

import oscar.cp.core.Constraint
import oscar.cp.core.CPPropagStrength
import oscar.algo.search.Outcome._
import oscar.algo.search.Outcome
import oscar.cp.core.variables.CPIntVar

/** 
 *  Integer Division with Arc-Consistency
 *  
 *  `a = b / c` with `c > 0`
 *  
 *  @author Renaud Hartert ren.hartert@gmail.com
 */
class IntDivisionAC(val a : CPIntVar, val b: CPIntVar, val c: Int) extends Constraint(a.store, "IntDivisionAC") {
  
  // Checks requirements
  require(c > 0, "c has to be greater than 0")
  
  override def setup(l: CPPropagStrength): Outcome = {
    if (init() == Failure) Failure
    else {
      if (!a.isBound) a.callValRemoveWhenValueIsRemoved(this)
      if (!b.isBound) b.callValRemoveWhenValueIsRemoved(this)
      Suspend
    }
  }
  
  private def init(): Outcome = {
    // Checks values of a
    val valuesA = a.toArray
    var i = 0
    while (i < valuesA.length) {
      val value = valuesA(i)
      val m = value * c
      val n = m + c
      var v = m
      // Searches a support of value
      var supported = false
      while (!supported && v < n) {
        supported = b.hasValue(v)
        v += 1
      }
      if (!supported && a.removeValue(value) == Failure) return Failure
      else i += 1
    }
    // Checks values of b
    val valuesB = b.toArray
    i = 0
    while (i < valuesB.length) {
      val value = valuesB(i)
      if (!a.hasValue(value / c) && b.removeValue(value) == Failure) return Failure
      else i += 1
    }    
    Suspend
  }
  
  override def valRemove(intVar: CPIntVar, value: Int): Outcome = {
    if (intVar == b) {
      var supported = false
      val m = value / c
      var v = m*c
      val n = m*c + c
      while (!supported && v < n) {
        if (b.hasValue(v)) supported = true
        else v += 1
      }
      if (supported) Suspend
      else a.removeValue(value / c)
    }
    else {
      val m = value * c
      val n = m + c
      var v = m
      while (v < n) {
        if (b.removeValue(v) == Failure) return Failure
        else v += 1
      }
      Suspend
    }
  }
}