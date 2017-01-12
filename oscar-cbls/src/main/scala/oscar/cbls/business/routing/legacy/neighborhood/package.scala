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
package oscar.cbls.business.routing

/*******************************************************************************
 * Contributors:
 *     This code has been initially developed by Ghilain Florent.
 ******************************************************************************/

//TODO: a segment exchange neighbor that exchange two segments of two route (or from the same route) heavily relying on the k-nearest and route invariant of course

/**
 * Contains all neighborhoods, like three-opt, two-opt, 1-0 exchange, etc..
 * all these neighborhood implement the [[oscar.cbls.search.core.Neighborhood]] so they can be used with [[oscar.cbls.search.combinators]]
 */
package object neighborhood {}
