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

package oscar.xcsp3.competition.solvers.parallel

import oscar.algo.Inconsistency
import oscar.cp.NoSolutionException
import oscar.cp.searches.lns.CPIntSol
import oscar.modeling.algebra.integer.IntExpression
import oscar.modeling.models.{Maximisation, Minimisation}
import oscar.modeling.solvers.cp.Branchings._
import oscar.modeling.solvers.cp.decompositions.CartProdRefinement
import oscar.modeling.solvers.cp.{Branchings, CPProgram}
import oscar.xcsp3.XCSP3Parser2
import oscar.xcsp3.competition.{CompetitionApp, CompetitionConf}

import scala.collection.mutable

object ParallelSolver extends CompetitionApp with App{

  override def runSolver(conf: CompetitionConf): Unit = {

    val program = new CPProgram[Unit]()

    //Parsing the instance
    printComment("Parsing instance...")
    val (vars, solutionGenerator) = try {
      XCSP3Parser2.parse(program.md, conf.benchname())
    } catch {
      case _: NotImplementedError =>
        status = "UNSUPPORTED"
        printStatus()
        (null, null)
      case _: NoSolutionException =>
        status = "UNSATISFIABLE"
        printStatus()
        (null, null)
      case _: Inconsistency =>
        status = "UNSATISFIABLE"
        printStatus()
        (null, null)
    }

    if (vars != null){
      val startTime = System.nanoTime()

      val obj: Option[(Boolean, IntExpression)] = program.md.getCurrentModel.optimisationMethod match {
        case Maximisation(o) => Some(true, o)
        case Minimisation(o) => Some(false, o)
        case _ => None
      }

      val sols = mutable.ListBuffer[(CPIntSol, String)]()

      program.onSolution({
        this.synchronized {
          val time = System.nanoTime() - startTime
          val sol = new CPIntSol(vars.map(_.min), if(obj.isDefined) obj.get._2.evaluate() else 0, time)
          val instantiation = solutionGenerator()
          if(sols.isEmpty || (obj.isDefined && ((obj.get._1 && sol.objective > sols.last._1.objective) || (!obj.get._1 && sol.objective < sols.last._1.objective)))){
            updateSol(instantiation, sol.objective, obj.isDefined)
            sols += ((sol, instantiation))
          }
        }
      })

      printComment("Parsing done, starting search...")

      val search = Branchings.conflictOrderingSearch(vars, i => vars(i).size, learnValueHeuristic(vars, if(obj.isDefined && obj.get._1) vars(_).min else vars(_).max))
      program.setDecompositionStrategy(new CartProdRefinement(vars, search))
      program.setSearch(search)

      val out = program.solveParallel(conf.nbcore(), 200, 0, (conf.timelimit() - 5) * 1000)

      if(sols.nonEmpty){
        if(obj.isDefined && out._1.completed) status = "OPTIMUM FOUND"
      }
      else if(out._1.completed) status = "UNSATISFIABLE"
      else printDiagnostic("NO_SOL_FOUND")
      printStatus()
    }
  }
}
