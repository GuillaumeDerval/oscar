package oscar.modeling.perf

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


import oscar.cp._
import oscar.modeling.algebra.integer.Max
import oscar.modeling.constraints.{BinPacking, GCC, Spread}
import oscar.modeling.solvers.cp.{Branchings, CPApp}
import oscar.modeling.vars.IntVar
import oscar.util._

import scala.io.Source


/**
 * Balanced Academic Curriculum Problem
 * The BACP is to design a balanced academic curriculum by assigning periods to courses in a way that 
 * the academic load of each period is balanced, i.e., as similar as possible . The curriculum must obey the following administrative and academic regulations: 
 * Academic curriculum: an academic curriculum is defined by a set of courses and a set of prerequisite relationships among them. 
 * Number of periods: courses must be assigned within a maximum number of academic periods. 
 * Academic load: each course has associated a number of credits or units that represent the academic effort required to successfully follow it. 
 * Prerequisites: some courses can have other courses as prerequisites. 
 * Minimum academic load: a minimum amount of academic credits per period is required to consider a student as full time. 
 * Maximum academic load: a maximum amount of academic credits per period is allowed in order to avoid overload. 
 * Minimum number of courses: a minimum number of courses per period is required to consider a student as full time. 
 * Maximum number of courses: a maximum number of courses per period is allowed in order to avoid overload. 
 * The goal is to assign a period to every course in a way that 
 * - the minimum and maximum academic load for each period, 
 * - the minimum and maximum number of courses for each period, 
 * - and the prerequisite relationships are satisfied. 
 * An optimal balanced curriculum balances academic load for all periods. 
 * @author Pierre Schaus pschaus@gmail.com
 */
object BACP extends CPApp[Unit] with App {
  val lines = Source.fromFile("data/bacp/instances12/inst4.txt").getLines.reduceLeft(_ + " " + _)
  val vals = lines.split("[ ,\t]").toList.filterNot(_ == "").map(_.toInt)
  var index = 0
  def next() = {
    index += 1
    vals(index - 1)
  }

  val nbCourses = next()
  val courses = 0 until nbCourses
  val nbPeriods = next()
  val periods = 0 until nbPeriods
  val mincredit = next()
  val maxcredit = next()
  val nbPre = next()
  val credits = Array.fill(nbCourses)(next())
  val prerequisites = Array.fill(nbPre)((next(), next()))

  var x = Array.fill(nbCourses)(IntVar(periods))
  val l = Array.fill(nbPeriods)(IntVar(0, credits.sum))
  val vari = IntVar(0, 10000000)

  add(Spread(l, credits.sum, vari))
  add(BinPacking(x, credits, l))
  for ((i, j) <- prerequisites) {
    add(x(i) < x(j)) // precedence constraint
  }
  add(GCC(x, periods, 5, 6))

  minimize(Max(l))
  setSearch {
    Branchings.binaryFirstFail(x, x => selectMin(periods)(x.hasValue)(l(_).min).get)
  }

  onSolution{}

  println(solve()._1)
}
