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
package oscar.dfo

import oscar.algebra._
import oscar.dfo.modeling.DFOSolver

package object modeling {
  
  val rand = new scala.util.Random(12)
  
  object DFOAlgo extends Enumeration {
    val NelderMead = Value("NelderMead")
    val DDS = Value("DDS")
    val MDS = Value("MDS")
  }
  
  def minimize(objective: Expression[AnyType,Double])(implicit dfo: DFOSolver) = dfo.minimize(objective)
  def onSolution(block: => Unit)(implicit dfo: DFOSolver) = dfo.onSolution(block) 

}
