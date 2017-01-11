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
/*******************************************************************************
  * Contributors:
  *     This code has been initially developed by CETIC www.cetic.be
  *         by Renaud De Landtsheer
  ******************************************************************************/


package oscar.cbls.invariants.core

/**
 * This package proposes a model of propagation.
 * A propagation is a wave that updates something in [[oscar.cbls.core.propagation.PropagationElement]]
 * Propagation elements have some dependencies that must be followed by the propagation wave.
 * Propagation elements are grouped in a structure called the [[oscar.cbls.core.propagation.PropagationStructure]]
 *
 * A propagation is like wave that sweeps the graph defined by the propagation elements,
 * calling the performPropagation method of each propagation element in such a way that
 - each propagate method is called only once
 - the propagate method of a propagation element is only called after all its preceding propagation elements have been propagated
 *
 * One might request targeted propagation, which specify that the propagation wave must only propagate
 * what is necessary to reach a defined element.
 * @author renaud.delandtsheer@cetic.be
 *
 */
package object propagation{
}
