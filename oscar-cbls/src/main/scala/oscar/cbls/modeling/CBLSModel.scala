package oscar.cbls.modeling
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

import oscar.cbls.constraints.core.{Constraint, ConstraintSystem}
import oscar.cbls.invariants.core.computation._
import oscar.cbls.invariants.core.propagation.Checker
import oscar.cbls.objective.Objective
import oscar.cbls.search.{SearchEngineTrait, StopWatch}


/** this is a helper object that you can extend to implement your solver with the minimal syntactic overhead.
  * It imports all the methods that you might need to develop your own solver based on the CBLS approach
  *
  * @param verbose requires that the propagation structure prints a trace of what it is doing. all prints are preceded by ''PropagationStruture''
  * @param checker specifies that once propagation is finished, it must call the checkInternals method on all propagation elements.
  * @param noCycle is to be set to true only if the static dependency graph between propagation elements has no cycles. If unsure, set to false, the engine will discover it by itself. See also method isAcyclic to query a propagation structure.
  * @param topologicalSort set to true if you want to use topological sort, to false for layered sort (layered is faster)
  * @param propagateOnToString set to true if a toString triggers a propagation, to false otherwise. Set to false only for deep debugging
  * @author renaud.delandtsheer@cetic.be
  **/
class CBLSModel(val verbose:Boolean = false,
                 val checker:Option[Checker] = None,
                 val noCycle:Boolean = true,
                 val topologicalSort:Boolean = false,
                 val propagateOnToString:Boolean = true)
  extends SearchEngineTrait
  with AlgebraTrait
  with Constraints
  with ClusterInvariants
  with ComplexLogicInvariants
  with ElementInvariants
  with MinMaxInvariants
  with NumericInvariants
  with SetInvariants
  with StopWatch
  with Search{

  implicit val s = new Store(verbose, checker, noCycle, topologicalSort,propagateOnToString)
  implicit val c = new ConstraintSystem(s)

  def close()(implicit s:Store) {s.close()}

  def add(c:Constraint)(implicit cs:ConstraintSystem) {cs.post(c)}
  def post(c:Constraint)(implicit cs:ConstraintSystem) {cs.post(c)}

  def violation()(implicit cs:ConstraintSystem) = cs.violation
  def violations[V<:Variable](v:Array[V])(implicit cs:ConstraintSystem) = cs.violations(v)

  def solution()(implicit s:Store) = s.solution()

  def swapVal(a:CBLSIntVar, b:CBLSIntVar)(implicit o:Objective) = o.swapVal(a,b)
  def assignVal(a: CBLSIntVar, v: Int)(implicit o:Objective) = o.assignVal(a, v)

  def CBLSIntVar(value:Int = 0, d:Domain = FullRange, name:String = null)(implicit s:Store) = new CBLSIntVar(s,value, d,name)
}
