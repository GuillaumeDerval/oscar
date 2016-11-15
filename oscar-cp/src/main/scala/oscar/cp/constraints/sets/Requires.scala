package oscar.cp.constraints.sets

import oscar.algo.search.Outcome
import oscar.cp.core.variables.CPSetVar
import oscar.cp.core.Constraint
import oscar.algo.search.Outcome._
import oscar.cp.core.variables.CPBoolVar
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.variables.CPIntVar

/** 
 *  @author Renaud Hartert ren.hartert@gmail.com
 *  @author Pierre Schaus pschaus@gmail.com
 */

class Requires(val set: CPSetVar, elem: Int) extends Constraint(set.store, "Set requires") {
  override def setup(l: CPPropagStrength): Outcome = set.requires(elem)
}

class RequireElem(set: CPSetVar, elem: Int, b: CPBoolVar) extends Constraint(set.store, "RequiredElem") {

  override def setup(l: CPPropagStrength): Outcome = {
    val outcome = propagate()
    if (outcome == Failure) Failure
    else if (outcome == Success) Success
    else {
      set.callValExcludedWhenExcludedValue(this)
      set.callValRequiredWhenRequiredValue(this)
      b.callValBindWhenBind(this)
      Suspend
    }
  }

  override def propagate(): Outcome = {
    if (b.isBound) valBind(b)
    else if (set.isRequired(elem)) setTrue()
    else if (!set.isPossible(elem)) setFalse()
    else Suspend
  }

  @inline
  private def setTrue(): Outcome = {
    if (b.assign(1) == Failure) Failure
    else Success
  }

  @inline
  private def setFalse(): Outcome = {
    if (b.assign(0) == Failure) Failure
    else Success
  }

  @inline
  private def requires(elem: Int): Outcome = {
    if (set.requires(elem) == Failure) Failure
    else Success
  }
  
  @inline
  private def excludes(elem: Int): Outcome = {
    if (set.excludes(elem) == Failure) Failure
    else Success
  }

  override def valRequired(cpSet: CPSetVar, reqElem: Int): Outcome = {
    if (reqElem == elem) setTrue()
    else Suspend
  }

  override def valExcluded(cpSet: CPSetVar, exElem: Int): Outcome = {
    if (exElem == elem) setFalse()
    else Suspend
  }

  override def valBind(cpVar: CPIntVar): Outcome = {
    if (b.isTrue) requires(elem)
    else excludes(elem)
  }
}

object Requires {
  def apply(set: CPSetVar, elem: Int, reifBool: CPBoolVar): Constraint = new RequireElem(set, elem, reifBool)
  def apply(set: CPSetVar, elem: Int): Constraint = new Requires(set, elem)
}