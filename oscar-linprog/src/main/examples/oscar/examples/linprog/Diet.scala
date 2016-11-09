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

package oscar.examples.linprog

import oscar.algebra._
import oscar.linprog.MPModel
import oscar.linprog.lp_solve.LPSolve

/**
 * The goal of the diet problem is to find the cheapest combination of foods
 * that will satisfy all the daily nutritional requirements of a person.
 * The problem is formulated as a linear program where the objective is to minimize cost and
 * meet constraints which require that nutritional needs be satisfied.
 * We include constraints that regulate the number of
 * calories and amounts of vitamins, minerals, fats, sodium and cholesterol in the diet.
 */
object Diet extends MPModel(LPSolve) with App {

  case class Nutriment(name: String) {
    val shortName = name
  }
  case class Food(x: Var[Double], price: Double, contents: Nutriment => Double)

  val nutriments = List("A", "C", "B1", "B2", "NA", "CAL").map { Nutriment }

  def nvar(name: String) = VarNumerical(name, 2, 10) //Each food is limited between 2 and 10

  val foods = List(
    (nvar("Beef"), 3.19, List(60, 20, 10, 15, 938, 295)),
    (nvar("Chicken"), 2.59, List(8, 0, 20, 20, 2180, 770)),
    (nvar("Fish"), 2.29, List(8, 10, 15, 10, 945, 440)),
    (nvar("Ham"), 2.89, List(40, 40, 35, 10, 278, 430)),
    (nvar("Macaroni"), 1.89, List(15, 35, 15, 15, 1182, 315)),
    (nvar("MeatLoaf"), 1.9, List(70, 30, 15, 15, 896, 400)),
    (nvar("Spaghetti"), 1.99, List(25, 50, 25, 15, 1329, 370)),
    (nvar("Turkey"), 2.49, List(60, 20, 15, 10, 1397, 450))
  ).map { case (n, p, nut) => Food(n, p, nutriments.zip(nut.map(_.toDouble)).toMap) }

  //for each nutriment, at least 700 must be present in the Diet
  var id = 0
  for (n <- nutriments) {
    add(s"C_$id" |: sum(foods) { f =>  f.x*f.contents(n) } >= 700.toDouble)
  }
  //minimize the total cost
  minimize(sum(foods) { f => f.x*f.price })

  // effectively solve the model
  interface.solve(this).onSolution { solution =>
      println("objective: " + solution(objective.expression))
      println("-----------------------------------")
      println(foods.map(f => s"${f.x.name} -> ${solution(f.x)}").mkString("\n"))
  }
}
