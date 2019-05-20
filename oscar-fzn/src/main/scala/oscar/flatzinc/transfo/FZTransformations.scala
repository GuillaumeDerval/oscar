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
/**
 * @author Gustav Björdal
 * @author Jean-Noël Monette
 */
package oscar.flatzinc.transfo

import oscar.flatzinc.model._
import oscar.flatzinc.Log
import scala.collection.mutable.{ Map => MMap, Set => MSet }
import scala.collection.mutable.Queue
import oscar.flatzinc.UnsatException

object FZModelTransfo {
 
  def findInvariants(model: FZProblem, log:Log, searchVars: Iterable[Variable]):Unit = FZFindDefined.findInvariants(model,log, searchVars)
  def getSortedInvariants(invariants: List[Constraint])(implicit log: Log): (List[Constraint],List[Constraint]) = FZFindDefined.getSortedInvariants(invariants)(log)
  
  //def propagateDomainBounds(model: FZProblem)(implicit log: Log) = FZSimplify.propagateDomainBounds(model)(log)
  def simplify(model: FZProblem)(implicit log: Log) = FZSimplify.simplify(model)(log)
  def propagate(model: FZProblem)(implicit log: Log) = FZSimplify.propagate(model)(log)
}