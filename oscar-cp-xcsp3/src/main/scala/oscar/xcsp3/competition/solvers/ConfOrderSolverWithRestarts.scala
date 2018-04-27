package oscar.xcsp3.competition.solvers

import oscar.algo.search.{DFSearch, SearchStatistics}
import oscar.cp._
import oscar.xcsp3.competition.solvers.resources.COSSolver

object ConfOrderSolverWithRestarts extends COSSolver {

  override def startSearch(solver: CPSolver, stopCondition: DFSearch => Boolean, vars: Array[CPIntVar], maximizeObjective: Option[Boolean]): SearchStatistics = {

    val initialTimeLimit : Long = 1000000000L
    var restartTimeLimit : Long = initialTimeLimit
    var globalStop = false
    var lastSearchStats : SearchStatistics = null
    var initialTime = System.nanoTime()
    val r = 1.3
    var completed = false

    val stopRestartCondition : DFSearch => Boolean = search => {
      val initialStop = stopCondition(search)
      if(initialStop)
       globalStop = true

      val stopRestart = restartTimeLimit < System.nanoTime() - initialTime
      initialStop || stopRestart || completed
    }

    val rand = new scala.util.Random(42)

    solver.search(
      conflictOrderingSearch(
        vars,
        i => vars(i).size,
//        learnValueHeuristic(vars, if(maximizeObjective.isDefined) if(maximizeObjective.get) vars(_).max else vars(_).min else vars(_).max)
        learnValueHeuristic(vars, if(maximizeObjective.isDefined) if(maximizeObjective.get) vars(_).randomValue(rand) else vars(_).randomValue(rand) else vars(_).randomValue(rand))
      )
    )

    //find first solution
    lastSearchStats = solver.start(timeLimit = 10, nSols = 1)
    if(lastSearchStats.completed) {
      completed = true
    }

    //restarts
    while(!globalStop) {
//      println(s"restartTimeLimit : $restartTimeLimit")
      initialTime = System.nanoTime()
      lastSearchStats = solver.startSubjectTo(stopRestartCondition, Int.MaxValue, null){}
      if(lastSearchStats.completed) {
        completed = true
      }
      if(lastSearchStats.nSols > 0) {
        restartTimeLimit = initialTimeLimit
      }
      restartTimeLimit = math.ceil(restartTimeLimit * r).toLong
    }

    lastSearchStats
  }
}
