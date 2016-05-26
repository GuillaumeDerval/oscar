package oscar.cp.constraints

import oscar.cp.core.variables.CPIntVar
import oscar.cp.core.{CPStore, Constraint, CPPropagStrength, CPOutcome}
import oscar.cp.core.CPOutcome._

/**
 * Minus Constraint
 * @author Pierre Schaus pschaus@gmail.com
 * @author Renaud Hartert ren.hartert@gmail.com
 */
class Minus(x: CPIntVar, y: CPIntVar, z: CPIntVar) extends Constraint(x.store, "Minus") {

  final override def setup(l: CPPropagStrength): CPOutcome = {

    priorityL2 = CPStore.MaxPriorityL2


    if (propagate() == Failure) Failure
    else {
      x.callPropagateWhenBoundsChange(this)
      y.callPropagateWhenBoundsChange(this)
      z.callPropagateWhenBoundsChange(this)
      Suspend
    }
  }

  final override def propagate(): CPOutcome = {   
    // Cache
    val xMin = x.min
    val xMax = x.max
    val yMin = y.min
    val yMax = y.max   
    // Prune z = x - y
    if (z.updateMax(xMax - yMin) == Failure) Failure
    else if (z.updateMin(xMin - yMax) == Failure) Failure
    else {
      // Cache
      val zMin = z.min
      val zMax = z.max
      // Prune y = x - z
      if (y.updateMax(xMax - zMin) == Failure) Failure
      else if (y.updateMin(xMin - zMax) == Failure) Failure
      // Prune x = z + y
      else if (x.updateMax(zMax + yMax) == Failure) Failure
      else if (x.updateMin(zMin + yMin) == Failure) Failure
      else Suspend
    }
  }
}
