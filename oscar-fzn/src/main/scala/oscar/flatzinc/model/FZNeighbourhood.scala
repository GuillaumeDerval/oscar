/*
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

package oscar.flatzinc.model

/**
  * @author Gustav Bjordal
  */

class FZNeighbourhood(val name: String,
                      val subNeighbourhoods: List[FZSubNeighbourhood],
                      val initVariables: Array[Variable],
                      val initConstraints: List[Constraint]) {

  private val controlledVariables = subNeighbourhoods.foldLeft(Array.empty[Variable])(
    (acc, n) => acc ++ n.getControlledVariables).toSet.toArray

  def getControlledVariables: Array[Variable] = {
    controlledVariables
  }

  private val searchVariables = subNeighbourhoods.foldLeft(Array.empty[Variable])(
    (acc, n) => acc ++ n.getSearchVariables).toSet.toArray

  def getSearchVariables: Array[Variable] = {
    searchVariables
  }
}

class FZSubNeighbourhood(val name: String,
                         val itVariables: Array[Variable],
                         val moves: List[FZMove],
                         val whereVariables: Array[Variable],
                         val whereConstraints: List[Constraint],
                         val ensureVariables: Array[Variable],
                         val ensureConstraints: List[Constraint]) {

  private val controlledVariables = (moves.foldLeft(Array.empty[Variable])(
    (acc, m) => acc ++ m.getControlledVariables) ++ itVariables ++ whereVariables ++ ensureVariables).toSet.toArray

  def getControlledVariables: Array[Variable] = {
    controlledVariables
  }

  private val searchVariables = moves.foldLeft(Array.empty[Variable])(
    (acc, m) => acc ++ m.getControlledVariables).toSet.toArray

  def getSearchVariables: Array[Variable] = {
    searchVariables
  }
  def debugPrint = {
    println("------ Neighbourhood for " + name + " ------")
    println("Iterator variables: ")
    for (it <- itVariables) {
      println("\t" + it)
    }
    println("Where variables:")
    for (e <- whereVariables) {
      println("\t" + e)
    }
    println("Where constraints:")
    for (e <- whereConstraints) {
      println("\t" + e)
    }
    println("Moves:")
    for (m <- moves) {
      println("\t" + m)
    }
    println("Ensure variables:")
    for (e <- ensureVariables) {
      println("\t" + e)
    }
    println("Ensure constraints:")
    for (e <- ensureConstraints) {
      println("\t" + e)
    }
  }

}
