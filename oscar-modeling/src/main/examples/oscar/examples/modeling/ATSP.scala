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
package oscar.examples.modeling

import oscar.modeling.constraints.{MinCircuit, MinCircuitWeak}
import oscar.modeling.models.UninstantiatedModel
import oscar.modeling.models.cp.CPModel
import oscar.modeling.solvers.cp.decompositions.{AnotherModelDecomposition, CartProdRefinement}
import oscar.modeling.solvers.cp.{Branchings, CPApp}
import oscar.modeling.vars.IntVar
import oscar.util._

import scala.io.Source
import scala.spores._

/**
  * Example of ATSP, copied from the original one from OscaR-lib.
  * GNU GPL, OscaR Authors
  */
object ATSP extends CPApp[Int] {
  var lines = Source.fromFile("ftv70.atsp").getLines.toArray
  lines = lines.take(lines.size-1) // drop EOF
  val n = lines(3).split(":")(1).trim().toInt
  val dist = lines.drop(7).reduceLeft(_ + " " + _).split("[ ,\t]").toList.filterNot(_ == "").map(_.toInt)

  val distMatrixSucc = dist.sliding(n,n).toArray.map(_.toArray)

  distMatrixSucc.foreach(i => println(i.mkString("\t")))
  val succ = Array.fill(n)(IntVar(0, n-1))
  val obj = IntVar(0,1000000-1)

  minimize(obj)

  // Get another model with a weak min circuit for decomposition
  val modelWithWeakMinCircuit = this.getCurrentModel.apply({
    add(MinCircuitWeak(succ, distMatrixSucc,obj))
    this.getCurrentModel.asInstanceOf[UninstantiatedModel]
  })

  add(MinCircuit(succ, distMatrixSucc,obj))

  val branching = Branchings.fromAlternatives(spore {
    val _succ = succ
    val _n = n
    val _distMatrixSucc = distMatrixSucc
    (cp: CPModel) => {
      // Select the not yet bound city with the smallest number of possible successors
      selectMin(_succ.zipWithIndex)(x => !x._1.isBound)(x => x._1.size) match {
        case None => Branchings.noAlternative
        case Some(x) => {
          // Select the closest successors of the city x
          val v = selectMin(0 until n)(x._1.hasValue)(y => _distMatrixSucc(x._2)(y)).get
          Branchings.branch(cp.post(x._1 === v))(cp.post(x._1 !== v))
        }
      }
    }
  })

  setSearch(branching)
  setDecompositionStrategy(new AnotherModelDecomposition(modelWithWeakMinCircuit, new CartProdRefinement(succ, branching)))
  onSolutionF(spore {
    val x_ = obj
    () => {
      x_.max
    }
  })

  val stat = solve()
  println(stat)

}