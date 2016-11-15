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

package oscar.cp

import oscar.algo.search.Outcome._
import oscar.algo.search.Outcome
import oscar.cp.core._
package object scheduling {

  /**
   * prune such that activity 1 << activity 2
   */
  def precedes(s1: CPIntVar, d1: CPIntVar, e1: CPIntVar, s2: CPIntVar, d2: CPIntVar, e2: CPIntVar): Outcome = {
      if (s2.updateMin(e1.min) == Failure) return Failure
      if (e1.updateMax(s2.max) == Failure) return Failure
      Suspend
  }

  /**
   * ensure s+d = e
   */
  def update(s: CPIntVar, d: CPIntVar, e: CPIntVar): Outcome = {
    // end <= start
    if (e.updateMin(s.min) == Outcome.Failure) {
      Outcome.Failure
    } else if (s.updateMax(e.max) == Outcome.Failure) {
      Outcome.Failure
    } // end = start + dur
    else if (e.updateMax(s.max + d.max) == Outcome.Failure) {
      Outcome.Failure
    } else if (e.updateMin(s.min + d.min) == Outcome.Failure) {
      Outcome.Failure
    } // start = end - dur
    else if (s.updateMax(e.max - d.min) == Outcome.Failure) {
      Outcome.Failure
    } else if (s.updateMin(e.min - d.max) == Outcome.Failure) {
      Outcome.Failure
    } // dur = end - start
    else if (d.updateMax(e.max - s.min) == Outcome.Failure) {
      Outcome.Failure
    } else if (d.updateMin(e.min - s.max) == Outcome.Failure) {
      Outcome.Failure
    } else Outcome.Suspend
  }
  

}