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
 * @author Jean-Noël Monette
 */
package oscar.flatzinc

class NoSuchConstraintException(val cstr: String, val where: String) extends Exception("Constraint "+cstr+" is not implemented in "+where)

class NoSuchMoveException(val move: String, val where: String) extends Exception("FZMove "+move+" is not implemented in "+where)


class UnsatException(val txt: String) extends Exception("The problem is unsatisfiable: "+txt)

class ParsingException(val txt: String) extends RuntimeException("Parsing Exception: "+txt)