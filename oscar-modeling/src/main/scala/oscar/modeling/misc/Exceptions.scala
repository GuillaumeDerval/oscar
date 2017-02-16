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

package oscar.modeling.misc

/**
 * Exception launched when an invalid CPVar is used to implement a Var
 * @param msg: an explanation message
 */
class InvalidCPVarException(msg: String) extends IllegalArgumentException(msg)

class EmptyDomainException extends Exception("")

class CannotBecomeSparseException extends Exception("")

class VariableNotBoundException extends Exception("")

class InvalidValueException extends Exception("")