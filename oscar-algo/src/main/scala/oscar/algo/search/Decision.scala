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

package oscar.algo.search

import oscar.algo.reversible.ReversibleContext

/**
  * @author Sascha Van Cauwelaert
  * @author Pierre Schaus
  */
trait Decision extends Alternative {}

trait TrailDecision extends Decision {}

final class Push(val context: ReversibleContext) extends TrailDecision {
  override def apply(): Unit = context.pushState()
  override def toString: String = s"Push"
}

final class Pop(val context: ReversibleContext) extends TrailDecision {
  override def apply(): Unit = context.pop()
  override def toString: String = s"Pop"
}

class AlternativeDecision(alternative: Alternative) extends Decision {
  def apply() = alternative.apply()
}
