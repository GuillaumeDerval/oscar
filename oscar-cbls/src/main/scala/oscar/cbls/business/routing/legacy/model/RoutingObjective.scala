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
 *     This code has been initially developed by De Landtsheer Renaud and Ghilain Florent.
 * ****************************************************************************
 */

package oscar.cbls.business.routing.legacy.model

import oscar.cbls.core.computation._
import oscar.cbls.core.constraint.ConstraintSystem
import oscar.cbls.core.objective.{CascadingObjective, Objective}
import oscar.cbls.lib.invariant.numeric.Sum

/**
 * @author renaud.delandtsheer@cetic.be
 * @author Florent Ghilain (UMONS)
 */
trait VRPObjective extends VRP {

  val accumulationVariable = CBLSIntVar(m, 0, FullRange, "objective of VRP")
  val objectiveFunction = Objective(accumulationVariable)

  var objectiveFunctionTerms: List[IntValue] = List.empty

  /** adds a term top the objective function*/
  def addObjectiveTerm(o: IntValue) {
    objectiveFunctionTerms = o :: objectiveFunctionTerms
  }

  m.addToCallBeforeClose(() => closeObjectiveFunction)

  /**
   * This finished the accumulation of terms in the objective unction.
   * You should not call this, actually.
   * it is called by the model on close
   */
  def closeObjectiveFunction() {
    if (objectiveFunctionTerms.isEmpty) throw new Error("you have set an Objective function to your VRP, but did not specify any term for it, call vrp.addObjectiveTerm, or add an objective trait to your VRP")
    accumulationVariable <== Sum(objectiveFunctionTerms)
  }

  def getObjective(): Objective = objectiveFunction
}

/**
 * This trait maintains strong constraints system.
 * @author renaud.delandtsheer@cetic.be
 */
trait StrongConstraints extends VRPObjective {
  /**
   * the strong constraints system.
   */
  var strongConstraints = ConstraintSystem(m)

  override def getObjective(): Objective = new CascadingObjective(strongConstraints,super.getObjective())
}

/**
 * This trait maintains an additional strong constraints system.
 * the e purpose is that this constraint system will be tested first for
 * truth value, and the primary one of the StrongConstraints trait will only be queried for truth value
 * if this additional constraint system is not violated
 * the proper way to use it in order to get a speedup is to put the constraints
 * that can be checked quickly in the strongConstraintsFast
 * and to keep all the other ones in the strongCon.
 *
 * @author renaud.delandtsheer@cetic.be
 */
trait StrongConstraintsFast extends StrongConstraints {
  /**
   * the strong constraints system.
   */
  var strongConstraintsFast = ConstraintSystem(m)

  override def getObjective(): Objective = new CascadingObjective(strongConstraintsFast,super.getObjective())
}

/**
 * This trait maintains weak constraints system.
 * @author renaud.delandtsheer@cetic.be
 */
trait WeakConstraints extends VRPObjective {
  /**
   * the weak constraints system.
   */
  val weakConstraints = ConstraintSystem(m)

  addObjectiveTerm(weakConstraints.violation)
}


/**
 * Declares an objective function, attached to the VRP.
 * It maintains it equal to the hop distance in the VRP,
 * based either on a matrix, or on another mechanism defined by the distance function.
 * @author renaud.delandtsheer@cetic.be
 * @author Florent Ghilain (UMONS)
 */
trait HopDistanceAsObjectiveTerm extends HopDistance with VRPObjective {
  addObjectiveTerm(overallDistance)
}

trait PenaltyForUnroutedAsObjectiveTerm extends PenaltyForUnrouted with VRPObjective {
  addObjectiveTerm(unroutedPenalty)
}

trait PenaltyForEmptyRouteWithExceptionAsObjectiveTerm extends PenaltyForEmptyRouteWithException with VRPObjective {
  addObjectiveTerm(emptyRoutePenalty)
}

trait PenaltyForEmptyRouteAsObjectiveTerm extends PenaltyForEmptyRoute with VRPObjective {
  addObjectiveTerm(emptyRoutePenalty)
}