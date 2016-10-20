package oscar.modeling.examples

import oscar.modeling.algebra.Sum
import oscar.modeling.constraints.AllDifferent
import oscar.modeling.models.CPModel
import oscar.modeling.solvers.cp.CPApp
import oscar.modeling.vars.IntVar
import oscar.util._
import oscar.modeling.solvers.cp.branchings.Branching
import oscar.modeling.solvers.cp.decompositions.CartProdRefinement

import scala.io.Source
import scala.spores._

/**
  * Example of QAP, copied from the original one from OscaR-lib.
  * GNU GPL, OscaR Authors
  */

object QuadraticAssignment extends CPApp[Int] with App {

  // Read the data
  var lines = Source.fromFile("qap.txt").getLines.toList.filter(_ != "")
  val n = lines.head.toInt
  val N = 0 until n
  lines = lines.drop(1)
  var w: Array[Array[Int]] = Array() //weight matrix
  var d: Array[Array[Int]] = Array() //distance matrix
  for (i <- N) {
    w = w :+ lines.head.split("[ ,\t]+").filter(_ != "").map(_ toInt).toArray
    lines = lines.drop(1)
  }
  for (i <- N) {
    d = d :+ lines.head.split("[ ,\t]+").filter(_ != "").map(_ toInt).toArray
    lines = lines.drop(1)
  }

  //onSolution { println("solution " + x.mkString(",")) }

  // for each facilities, the location chosen for it
  val x = N map (v => IntVar(0, n-1))

  add(AllDifferent(x.toArray))

  val obj = Sum(N, N)((i, j) => d(x(i))(x(j)) * w(i)(j)).reify()

  onSolution(spore {
    val x_ = obj
    () => {
      x_.max
    }
  })

  minimize(obj)

  val search = Branching.fromAlternatives(spore{
        val _x = x
        (cp: CPModel) => {
          val z = _x.filter(y => !y.isBound)
          if(z.isEmpty)
            Branching.noAlternative
          else {
            val vari = selectMinDeterministic(z)(y => y.size)
            val valu = vari.min
            Branching.branch(cp.post(vari === valu))(cp.post(vari !== valu))
          }
        }
    }
  )

  setSearch(search)
  setDecompositionStrategy(new CartProdRefinement(x,search))

  val stats = solve()
  println(stats)
}