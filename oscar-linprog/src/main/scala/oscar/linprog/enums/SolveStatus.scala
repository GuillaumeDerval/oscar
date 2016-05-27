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

package oscar.linprog.enums

sealed abstract class SolveStatus(val name: String) {
  override def toString: String = name
}

case object NotSolved extends SolveStatus("NOT_SOLVED")
case object Solved extends SolveStatus("SOLVED")

object SolveStatus {
  val values = Seq(NotSolved, Solved)

  def fromString(str: String): SolveStatus = values.find(ss => ss.name == str.toUpperCase) match {
    case Some(ss) => ss
    case None     => throw new IllegalArgumentException(s"Unrecognized solve status: $str")
  }
}

case object NotSolvedYetException extends Exception("Problem is not solved yet.")