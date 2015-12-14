package oscar.examples.cp

import oscar.cp._
import oscar.cp.nogoods.core.Nogood
import oscar.cp.nogoods.database.NogoodDB
import oscar.cp.nogoods.searches.{NogoodSearch, NogoodBranching, HeuristicNogoodBranching}
import oscar.util._

/**
 * The problem is to schedule an even number n of teams over n/2 periods and n - 1 weeks,
 * under the following constraints: <br>
 *  - Each team must play against every other team <br>
 *  - A team plays exactly one game per week  <br>
 *  - A team can play at most twice in the same period <br>
 *
 * @author Pierre Schaus pschaus@gmail.com
 */
object SportSchedulingNogood extends CPModel with App {

  val n = 14
  val nbPeriods = n / 2
  val Periods = 0 until nbPeriods
  val nbTeams = n
  val Teams = 0 until nbTeams
  val nbWeeks = n - 1
  val Weeks = 0 until nbWeeks
  val Homes = 0 to 1 // 0/1 for home/away game

  val team = Array.tabulate(nbPeriods, nbWeeks, 2)((p, w, h) => CPIntVar(0 until nbTeams))
  val game = Array.tabulate(nbPeriods, nbWeeks)((p, w) => CPIntVar(0 until (n * n - 1)))
  val tuples = for (i <- Teams; j <- i + 1 until nbTeams) yield (i, j, i * nbWeeks + j - 1)

  def printSol() {
    println("---------games---------")
    Periods.foreach {
      p => println(Weeks.map(w => game(p)(w).value).mkString("\t"))
    }
    println("---------teams---------")
    Periods.foreach {
      p => println(Weeks.map(w => (team(p)(w)(0).value, team(p)(w)(1).value)).mkString("\t"))
    }
    println("\n")
  }

  // make the link between the team and game variables
  for (p <- Periods; w <- Weeks) {
    add(table(team(p)(w)(0), team(p)(w)(1), game(p)(w), tuples))
  }
  // a team plays exactly one game per week
  for (w <- Weeks) {
    val teamw = for (p <- Periods; h <- Homes) yield team(p)(w)(h)
    add(allDifferent(teamw), Strong)
  }
  // every team plays against every other team
  add(allDifferent(game.flatten), Strong)
  // a team can play at most twice in the same period
  for (p <- Periods) {
    add(gcc(team(p).flatten, Teams, 1, 2), Strong)
  }

  val x: Array[CPIntVar] = game.flatten.toArray

  // Search that is able to extract no-goods (Recording and minimizing nogoods from restarts, Lecoutre et al, 2007)
  val ngBranching = new HeuristicNogoodBranching(x, i => x(i).size, i => x(i).randomValue)
  val ngDB = NogoodDB()
  val ngSearch = new NogoodSearch(solver,ngDB)


  var solFound = false
  ngSearch.onSolution {
    solFound = true
    println("sol found")
    printSol()
  }

  // use restarts to break heavy tails phenomena
  var restart = 0
  val t = time {
    do {
      ngSearch.start(ngBranching,ng => ng.nBacktracks > 5000 || ng.nSolutions > 1)
      // convert each no good computed on last restart and add them to the model
      solver.add(ngDB.allNogoods.map(ng => ng.toConstraint))
      println("#noGoods:"+ngDB.size)
      // remove all nogoods to avoid adding them twice
      ngDB.clear()
      restart += 1
    } while (!solFound)
  }

  println("time:" + t)
  println("#restart:" + restart)
}

