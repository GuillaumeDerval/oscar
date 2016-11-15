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
package oscar.cp.test

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import oscar.cp.core.Constraint
import oscar.algo.reversible._
import oscar.algo.search.Outcome
import oscar.cp._
import oscar.cp.core.CPPropagStrength

/**
 * @author Pierre Schaus pschaus@gmail.com
 */
class TestIdempotency extends FunSuite with ShouldMatchers {

  test("test idempotency") {

    var nbCallToPropagate = 0

    class MyCons(val X: CPIntVar, idempot: Boolean) extends Constraint(X.store, "MyCons") {
      idempotent = idempot
      override def setup(l: CPPropagStrength): Outcome = {
        X.callPropagateWhenDomainChanges(this)
        Outcome.Suspend
      }
      override def propagate(): Outcome = {
        nbCallToPropagate += 1
        X.removeValue(0)
      }
    }

    val cp = CPSolver()
    val x = CPIntVar(0 to 3)(cp)
    cp.add(new MyCons(x, false))
    cp.add(x !== 3)
    nbCallToPropagate should equal(2)
    
    nbCallToPropagate = 0
    val y = CPIntVar(0 to 3)(cp)
    cp.add(new MyCons(y, true))
    cp.add(y !== 3)
    nbCallToPropagate should equal(1)
  }
}

