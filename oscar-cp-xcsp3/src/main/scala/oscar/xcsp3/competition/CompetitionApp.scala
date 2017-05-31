package oscar.xcsp3.competition

import org.rogach.scallop.{ScallopConf, ScallopOption}

import scala.util.Random

class CompetitionConf(arguments: Seq[String]) extends ScallopConf(arguments){
  val randomseed: ScallopOption[Int] = opt[Int](default = Some(Random.nextInt(Int.MaxValue))) //A random seed
  val timelimit: ScallopOption[Int] = opt[Int](default = Some(240)) //The time available in seconds
  val memlimit: ScallopOption[Int] = opt[Int](default = Some(1000)) //The memory available in mb
  val nbcore: ScallopOption[Int] = opt[Int](default = Some(1)) //The number of cores available
  val tmpdir: ScallopOption[Int] = opt[Int]() //A temporary directory to write files
  val dir: ScallopOption[Int] = opt[Int]() //The directory containing the program
  val benchname: ScallopOption[String] = trailArg[String]() //The path to the instance file
  verify()
}

abstract class CompetitionApp extends App{
  val tStart = System.nanoTime()
  val conf = new CompetitionConf(args)

  printComment("seed: " + conf.randomseed())
  printComment("timeout: " + conf.timelimit())
  printComment("memlimit: " + conf.memlimit())

  Random.setSeed(conf.randomseed())

  try {
    runSolver(conf)
  }catch{
    case e: Exception =>
      printStatus("UNKNWOWN")
      printDiagnostic("EXCEPTION", e.getMessage)
      printComment(e.getStackTrace.mkString("\n"))
      Console.flush()
  }

  //TODO: Parse Instance, launch search and print results
  def runSolver(conf: CompetitionConf)

  /**
    * Use the following functions to print your outputs. Only the best solution should be printed.
    * You can print comments with the printComment method.
    */

  //Each time a new solution is found, it's objective should be printed:
  def printObjective(obj: Int): Unit = {
    println(tElapsed + " o " + obj)
    Console.flush()
  }

  //Use this only for the last solution:
  def printSolution(sol: String, optimum:Boolean = false): Unit = {
    if(optimum) printStatus("OPTIMUM FOUND")
    else printStatus("SATISFIABLE")

    println(tElapsed + " v " + sol.split("\\r?\\n").mkString("\n" + tElapsed + " v "))
  }

  /**
    * Allowed status:
    * OPTIMUM FOUND: the optimum has been found and proven (already printed in printSolution)
    * SATISFIABLE: a solution been found but cannot be proven optimal (already printed in printSolution)
    * UNSATISFIABLE: the instance is unsatisfiable (i.e. it is proven that no solution exists)
    * UNSUPPORTED: unsupported constraint
    * UNKNOWN: other (no solution has been found or there was a problem, use printDiagnostic to precise information)
    */
  def printStatus(status: String): Unit = println(tElapsed + " s " + status)

  //For any comment:
  def printComment(com: String): Unit = println(tElapsed + " c " + com.split("\\r?\\n").mkString("\n" + tElapsed + " c "))

  //For diagnostic information, name should be a keyword and value no more than one line.
  def printDiagnostic(name: String, value: String): Unit = println(tElapsed + " d " + name + " " + value)

  //For diagnostic information, name should be a keyword.
  def printDiagnostic(name: String): Unit = println(tElapsed + " d " + name)

  //Computes the time elapsed since the beginning of the search in ms
  def tElapsed: Long = (System.nanoTime() - tStart)/1000000
}
