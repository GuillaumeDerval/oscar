package examples

import constraints.{MinCircuit, MinCircuitWeak}
import models.{CPModel, UninstantiatedModel}
import oscar.util._
import solvers.cp.DistributedCPApp
import solvers.cp.branchings.Branching
import solvers.cp.decompositions.{AnotherModelDecomposition, CartProdRefinement}
import vars.IntVar

import scala.spores._
import scala.io.Source

/**
  * Example of ATSP, copied from the original one from OscaR-lib.
  * GNU GPL, OscaR Authors
  */
object ATSP extends DistributedCPApp[Int] with App {
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

  val branching = Branching.fromAlternatives(spore {
    val _succ = succ
    val _n = n
    val _distMatrixSucc = distMatrixSucc
    (cp: CPModel) => {
      // Select the not yet bound city with the smallest number of possible successors
      selectMin(_succ.zipWithIndex)(x => !x._1.isBound)(x => x._1.size) match {
        case None => Branching.noAlternative
        case Some(x) => {
          // Select the closest successors of the city x
          val v = selectMin(0 until n)(x._1.hasValue)(y => _distMatrixSucc(x._2)(y)).get
          Branching.branch(cp.post(x._1 === v))(cp.post(x._1 != v))
        }
      }
    }
  })

  setSearch(branching)
  setDecompositionStrategy(new AnotherModelDecomposition(modelWithWeakMinCircuit, new CartProdRefinement(succ, branching)))
  onSolution(spore {
    val x_ = obj
    () => {
      x_.max
    }
  })

  val stat = solve()
  println(stat)

}