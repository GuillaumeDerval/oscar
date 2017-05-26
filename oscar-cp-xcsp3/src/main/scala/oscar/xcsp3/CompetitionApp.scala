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
  println("args: ")
  println("Bench name: " + conf.benchname())
  println("Random seed: " + (if(conf.randomseed.isDefined) conf.randomseed() else "None"))
  println("Time limit: " + (if(conf.timelimit.isDefined) conf.timelimit() else "None"))
  println("Mem limit: " + (if(conf.memlimit.isDefined) conf.memlimit() else "None"))
  println("Nb cores: " + (if(conf.nbcore.isDefined) conf.nbcore() else "None"))

  //TODO: parse bench file

  //TODO: Launch search!
}
