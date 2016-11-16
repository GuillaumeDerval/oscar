package oscar.modeling.examples

import oscar.modeling.constraints.{BinPacking, Spread}

import scala.io.Source
import oscar.util._
import oscar.modeling.solvers.cp.{Branchings, CPApp, CPAppConfig}
import oscar.modeling.solvers.cp.decompositions.{CartProdRefinement, DecompositionAddCartProdInfo, DepthIterativeDeepening}
import oscar.modeling.vars.IntVar

import scala.spores._

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
  * @author Guillaume Derval guillaume.derval@student.uclouvain.be
  */
object BACP extends CPApp[Int] with App {
  override lazy val config = new CPAppConfig {
    val file = trailArg[String](descr = "Path to the instance")
  }

  val lines = Source.fromFile(config.file()).getLines.reduceLeft(_ + " " + _)
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

  val x = Array.fill(nbCourses)(IntVar(0, nbPeriods-1))
  val l = Array.fill(nbPeriods)(IntVar(0, credits.sum))
  val vari = IntVar(0, 10000000)

  add(Spread(l, credits.sum, vari))
  add(BinPacking(x, credits, l))
  for ((i, j) <- prerequisites) {
    add(x(i) < x(j)) // precedence constraint
  }

  // Search
  minimize(vari)

  val search = Branchings.binaryFirstFail(x, spore {
    val l_ = l.toIndexedSeq
    val periods_ = periods
    (z: IntVar) => {
      val ll = l_
      selectMinDeterministic(periods_.filter(z.hasValue(_)))(a => ll.apply(a).min)
    }
  })

  setSearch(search)
  setDecompositionStrategy(new CartProdRefinement(x, search))
  //setDecompositionStrategy(new DecompositionAddCartProdInfo(new DepthIterativeDeepening(Branching.naryStatic(x)), x))

  onSolution(spore {
    val v = vari
    () => vari.max
  })

  // Execution
  println(solve())
}

