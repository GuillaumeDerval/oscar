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
package oscar.invariants.test

import org.scalatest.FunSuite
import org.scalatest.Matchers
import scala.collection.immutable._
import oscar.invariants._

/**
 * Test functionality of Invariants
 * @author Sebatien Mouthuy & Pierre Schaus
 */
class InvariantsTesting extends FunSuite with Matchers {


	test("test whener on VarInt") {
		val y = new VarInt(1)

		var nb = 0

		whenever(y.filter(_ == 5)) {
			nb += 1
		}

		y := 5;
		nb should equal(1)
		y := 6;
		nb should equal(1)
		y := 5;
		nb should equal(2)
	}

	test("test once on VarInt") {
		val y = new VarInt(1)

		var nb = 0

		once(y.filter(_ == 5)) {
			nb += 1
		}

		y := 5;
		nb should equal(1)
		y := 6;
		nb should equal(1)
		y := 5;
		nb should equal(1)
	}


}

