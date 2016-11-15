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
package oscar.cp.constraints

import oscar.algo.search.Outcome
import oscar.cp.core.variables.CPIntVar
import oscar.algo.search.Outcome._
import oscar.cp.core.Constraint
import oscar.cp.core.CPPropagStrength
import oscar.cp.multiobjective.Pareto

class ParetoConstraint[Sol](pareto: Pareto[Sol], isMax: Array[Boolean], objVars: Array[CPIntVar]) extends Constraint(objVars.head.store, "Gavanelli02 Dominance") {

  // Simplifies code understanding
  type Point = IndexedSeq[Int]
  
  override def propagate(): Outcome = {
    //println("propagate gananelli")
    // List of all solutions
    val allSols = pareto.objectiveSols
    // The DPobj of each objective
    val DPobjs = for(o <- pareto.Objs) yield computeDPobj(o)
    // The best dominant solutions according to each objective
    val bestDoms = getAllBestDominant(DPobjs, allSols)
    
    for (o <- pareto.Objs if bestDoms(o).isDefined) {
        
      val bound = bestDoms(o).get(o)
      
      // objective has to be maximized
      if (isMax(o)) { 
        if (objVars(o).updateMin(bound + 1) == Failure) return Failure
      }
      // objective has to be minimized
      else { 
        if (objVars(o).updateMax(bound - 1) == Failure) return Failure
      }
    }
    
    Suspend
  }  
  
  // Returns the array of solutions such that for each objective i, bestDominant(i) is the solution 
  // that dominates DPObjs(i) with the best value for the objective i.
  private def getAllBestDominant(DPobjs: IndexedSeq[Point], sols: List[Point]): Array[Option[Point]] = {
    
    val bestDom: Array[Option[Point]] = Array.fill(pareto.nObjs)(None)
    val bestObj: Array[Int] = Array.tabulate(pareto.nObjs)(o => if (isMax(o)) Int.MinValue else Int.MaxValue)
    
    for (s <- sols; o <- pareto.Objs) {
      if (pareto.dominate(s, DPobjs(o))) {
        if (pareto.isBetter(o)(s(o), bestObj(o))) {
          bestDom(o) = Some(s)
          bestObj(o) = s(o)
        }
      }
    }
    bestDom
  }
  
  // Compute the point which is the best for all objectives except for the objective obj
  private def computeDPobj(obj: Int): IndexedSeq[Int] = {
    for (o <- pareto.Objs) yield {
      if (o == obj) {
        if (isMax(o)) objVars(o).min
        else objVars(o).max
      }
      else {
        if (isMax(o)) objVars(o).max
        else objVars(o).min
      }
    }
  }

  override def setup(l: CPPropagStrength): Outcome = {
    idempotent = true
    if (propagate() == Failure) Failure
    else {
      for(o <- pareto.Objs if !objVars(o).isBound) {
    	if (isMax(o)) objVars(o).callPropagateWhenBoundsChange(this)
    	else objVars(o).callPropagateWhenBoundsChange(this)
      }
      Suspend
    }
  }
}

object ParetoConstraint {  
  def apply[Sol](pareto: Pareto[Sol], isMax: Array[Boolean], objs: Array[CPIntVar]): ParetoConstraint[Sol] = new ParetoConstraint(pareto, isMax, objs)
}
