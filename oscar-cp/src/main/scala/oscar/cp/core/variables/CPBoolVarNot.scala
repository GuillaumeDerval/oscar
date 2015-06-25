package oscar.cp.core.variables

import scala.util.Random
import oscar.cp.core.CPOutcome
import oscar.cp.core.Constraint
import oscar.cp.core.CPStore

/** 
 *  A not view on a boolean variable. 
 *  
 *  @author Renaud Hartert ren.hartert@gmail.com 
 */
class CPBoolVarNot(final override val not: CPBoolVar) extends CPBoolVar {
  
  final override val store: CPStore = not.store
  
  final override val name: String = s"!${not.name}"

  final override def transform(value: Int) = not.transform(1 - value)

  final override def isBound = not.isBound

  final override def size = not.size

  final override def isEmpty = not.isEmpty
  
  final override def min: Int = 1 - not.max

  final override def max: Int = 1 - not.min
  
  final override def isTrue: Boolean = not.isFalse

  final override def isFalse: Boolean = not.isTrue

  final override def isBoundTo(value: Int): Boolean = not.isBoundTo(1 - value)
  
  final override def containsTrue: Boolean = not.containsFalse
  
  final override def containsFalse: Boolean = not.containsTrue

  final override def hasValue(value: Int): Boolean = not.hasValue(1-value)

  final override def valueAfter(value: Int): Int = 1 - not.valueBefore(1 - value)

  final override def valueBefore(value: Int): Int = 1 - not.valueAfter(1 - value)

  final override def randomValue(rand: Random): Int = 1 - not.randomValue(rand)

  final override def updateMin(value: Int): CPOutcome = not.updateMax(1 - value)

  final override def updateMax(value: Int): CPOutcome = not.updateMin(1 - value)
  
  final override def assignTrue(): CPOutcome = not.assignFalse()

  final override def assignFalse(): CPOutcome = not.assignTrue()
    
  final override def assign(value: Int): CPOutcome = not.assign(1 - value)

  final override def removeValue(value: Int) = not.removeValue(1 - value)

  final override def iterator: Iterator[Int] = {
    val size = not.size
    if (size == 2) Iterator(0, 1)
    else if (size == 1) Iterator(1 - not.min)
    else Iterator.empty
  }
  
  final override def constraintDegree: Int = not.constraintDegree

  final override def callPropagateWhenBind(c: Constraint): Unit = not.callPropagateWhenBind(c)

  final override def callPropagateWhenBoundsChange(c: Constraint): Unit = callPropagateWhenBoundsChange(c)
  
  final override def  callPropagateWhenDomainChanges(c: Constraint, watcher: oscar.cp.core.Watcher): Unit = ???

  final override def callPropagateWhenDomainChanges(c: Constraint, trackDelta: Boolean = false): Unit = not.callPropagateWhenDomainChanges(c, trackDelta)

  final override def callValBindWhenBind(c: Constraint): Unit = not.callValBindWhenBind(c, this) 
  
  final override def callValBindWhenBind(c: Constraint, variable: CPIntVar): Unit = not.callValBindWhenBind(c, variable)
  
  final override def callUpdateBoundsWhenBoundsChange(c: Constraint): Unit = not.callUpdateBoundsWhenBoundsChange(c: Constraint, this)

  final override def callUpdateBoundsWhenBoundsChange(c: Constraint, variable: CPIntVar): Unit = not.callUpdateBoundsWhenBoundsChange(c: Constraint, variable: CPIntVar)

  final override def callValRemoveWhenValueIsRemoved(c: Constraint): Unit = not.callValRemoveWhenValueIsRemoved(c: Constraint, this)

  final override def callValRemoveWhenValueIsRemoved(c: Constraint, variable: CPIntVar): Unit = not.callValRemoveWhenValueIsRemoved(c: Constraint, variable: CPIntVar)

  final override def callValRemoveIdxWhenValueIsRemoved(c: Constraint, idx: Int): Unit = not.callValRemoveIdxWhenValueIsRemoved(c: Constraint, this, idx: Int)

  final override def callValRemoveIdxWhenValueIsRemoved(c: Constraint, variable: CPIntVar, idx: Int): Unit = not.callValRemoveIdxWhenValueIsRemoved(c: Constraint, variable: CPIntVar, idx: Int)

  final override def callUpdateBoundsIdxWhenBoundsChange(c: Constraint, idx: Int): Unit = not.callUpdateBoundsIdxWhenBoundsChange(c: Constraint, this, idx: Int)

  final override def callUpdateBoundsIdxWhenBoundsChange(c: Constraint, variable: CPIntVar, idx: Int): Unit = not.callUpdateBoundsIdxWhenBoundsChange(c: Constraint, variable: CPIntVar, idx: Int)

  final override def callValBindIdxWhenBind(c: Constraint, idx: Int): Unit = not.callValBindIdxWhenBind(c: Constraint, this, idx: Int)

  final override def callValBindIdxWhenBind(c: Constraint, variable: CPIntVar, idx: Int): Unit = not.callValBindIdxWhenBind(c: Constraint, variable: CPIntVar, idx: Int)

  final override def fillDeltaArray(oldMin: Int, oldMax: Int, oldSize: Int, arr: Array[Int]): Int = ???    

  final override def delta(oldMin: Int, oldMax: Int, oldSize: Int): Iterator[Int] = ???

  final override def constraintTrue(): Constraint = not.constraintFalse

  final override def constraintFalse(): Constraint = not.constraintTrue
  
  final override def restrict(newDomain: Array[Int], newSize: Int): Unit = {
    assert(newSize > 0 && newSize <= size)
    if (newSize == 1) {
      val value = newDomain(0)
      if (value == 1) {
        assert(!not.isTrue) 
        not.assignFalse()
      } else {
        assert(!not.isFalse) 
        not.assignTrue()    
      }
    }
  }


  final override def toString: String = {
    if (not.isEmpty) "empty"
    else if (not.isTrue) "0"
    else if (not.isFalse) "1"
    else "{0, 1}"
  }
}