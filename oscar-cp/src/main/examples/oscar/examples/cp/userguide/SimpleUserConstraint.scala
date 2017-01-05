package oscar.examples.cp.userguide

import oscar.cp._
import oscar.cp.core._
import CPOutcome._

object SimpleUserConstraint extends CPModel with App {

  class MyLessOrEqual(val X: CPIntVar, val Y: CPIntVar ) extends Constraint(X.store, "MyLessOrEqual") {

    override def setup(l: CPPropagStrength): CPOutcome =  {
      X.callPropagateWhenBoundsChange(this)
      Y.callPropagateWhenBoundsChange(this)
      propagate()
    }

    override def propagate(): CPOutcome = {
      if (Y.min >= X.max) Success
      else if (Y.updateMin(X.min) == Failure) Failure
      else if (X.updateMax(Y.max) == Failure) Failure
      else Suspend
    }
  }

  val x = CPIntVar(0 to 10)
  val y = CPIntVar(0 to 5)

  add (new MyLessOrEqual(x,y))
}
