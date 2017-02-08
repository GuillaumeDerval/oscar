package oscar.examples.modeling

import scala.io.Source

import oscar.modeling.algebra.integer.Sum
import oscar.modeling.constraints.{AllDifferent, StrongPropagation}
import oscar.modeling.solvers.cp.{Branchings, CPApp}
import oscar.modeling.vars.IntVar

/**
 * Quadratic Assignment Problem:
 * There are a set of n facilities and a set of n locations.
 * For each pair of locations, a distance is specified and
 * for each pair of facilities a weight or flow is specified
 * (e.g., the amount of supplies transported between the two facilities).
 * The problem is to assign all facilities to different locations
 * with the goal of minimizing the sum of the distances multiplied by the corresponding flows.
 *
 * @author Pierre Schaus pschaus@gmail.com
 * @author Guillaume Derval guillaume.derval@uclouvain.be
 */
object QuadraticAssignmentLNS extends CPApp[Unit] with App {

  // Read the data
  var lines = Source.fromFile("data/qap.txt").getLines.toList.filter(_ != "")
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


  // for each facilities, the location chosen for it
  val x = N.map(v => IntVar(0 until n)).toArray

  add(new AllDifferent(x) with StrongPropagation)

  minimize(Sum(N, N)((i, j) => d(x(i))(x(j)) * w(i)(j)))

  setSearch(Branchings.binaryFirstFail(x))

  var lastSol: Array[Int] = _
  onSolution {
    lastSol = x.map(_.min)
    println(lastSol.mkString(","))
  }

  // Search for an initial solution
  solve(nSols=1)

  val rand = new scala.util.Random(0)
  var limit = 1000 // set the timeout to 1 sec

  for (r <- 1 to 200) {
    // relax randomly 50% of the variables and run again

    val stat = solveSubjectTo(maxTime = limit) {
      add(N.filter(i => rand.nextInt(100) < 50).map(i => x(i) === lastSol(x(i))).map(_.toConstraint))
    }

    // This is equivalent to
    //val stat = fork {
    //  add(N.filter(i => rand.nextInt(100) < 50).map(i => x(i) === lastSol(x(i))).map(_.toConstraint))
    //  solve(maxTime=limit)
    //}

    // adapt the time limit for next run *2 is previous run reached the limit /2 otherwise
    limit = if (stat._1.completed) limit / 2 else limit * 2
    println("set limit to " + limit)
  }
  println(lastSol.mkString(","))
}
