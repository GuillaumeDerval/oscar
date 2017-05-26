package oscar.xcsp3

import org.rogach.scallop.{ScallopConf, ScallopOption}

class CompetitionConf(arguments: Seq[String]) extends ScallopConf(arguments){
  val randomseed: ScallopOption[Int] = opt[Int]()
  val timelimit: ScallopOption[Int] = opt[Int]()
  val memlimit: ScallopOption[Int] = opt[Int]()
  val nbcore: ScallopOption[Int] = opt[Int]()
  val tmpdir: ScallopOption[Int] = opt[Int]()
  val dir: ScallopOption[Int] = opt[Int]()
  val benchname: ScallopOption[String] = trailArg[String]()
  verify()
}

object CompetitionApp extends App{
  val conf = new CompetitionConf(args)

  //TODO: Parse Instance and launch search

  /**
    * Use the following functions to print your outputs. Only the best solution should be printed.
    * You can print comments with the printComment method.
    */

  //Each time a new solution is found, it's objective should be printed:
  def printObjective(obj: Int): Unit = println("o " + obj)

  //Use this only for the last solution:
  def printSolution(sol: String, optimum:Boolean = false): Unit = {
    if(optimum) println("s OPTIMUM FOUND")
    else println("s SATISFIABLE")

    println("v " + sol.split("\\r?\\n").mkString("\nv "))
  }

  /**
    * Allowed status:
    * UNSATISFIABLE: the instance is unsatisfiable
    * UNSUPPORTED: unsupported constraint
    * UNKNOWN: other problem
    */
  def printStatus(status: String): Unit = println("s " + status)

  //For any comment:
  def printComment(com: String): Unit = println("c " + com.split("\\r?\\n").mkString("\nc "))
}
