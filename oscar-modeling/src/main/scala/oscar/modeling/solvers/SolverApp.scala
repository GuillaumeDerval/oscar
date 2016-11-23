package oscar.modeling.solvers

import org.rogach.scallop.{Scallop, ScallopConf, Subcommand}
import oscar.modeling.constraints.AllDifferent
import oscar.modeling.misc.ScallopConfProxy
import oscar.modeling.models.{ModelDeclaration, ModelDeclarationProxy}
import oscar.modeling.solvers.cp._
import oscar.modeling.solvers.cp.decompositions.CartProdRefinement
import oscar.modeling.vars.IntVar

import scala.spores._


/**
  * The base app of OscaR-Modeling. Can be linked to any type of solvers with modules, mixed-in using traits.
  */
class SolverApp[RetVal](modelDeclaration: ModelDeclaration = new ModelDeclaration()) extends SolveHolder[RetVal](modelDeclaration) with App with SolverAppModulable with ModelDeclarationProxy {
  val app = this
  implicit val md: ModelDeclaration = modelDeclaration

  // Custom configuration per app
  // Simply override config:
  // override lazy val config = new AppConfig { val myint = opt[Int]("myint","m") }
  class AppConfig extends ScallopConfProxy
  lazy val config = new AppConfig

  // Modules and config for them
  private val modules = getModules.map(x => x.subcommand -> x).toMap
  private val completeConfig = new ScallopConf(args) {
    modules.foreach(x => {
      config.applyTo(x._1)
      this.addSubcommand(x._1)
    })
  }
  completeConfig.verify()
  if(completeConfig.subcommand.isEmpty) {
    completeConfig.printHelp()
    System.exit(1)
  }

  // Module configuration
  private val moduleConfig = completeConfig.subcommand.get
  config.setProxyTo(moduleConfig)
  private val solverAppModule = modules(completeConfig.subcommand.get.asInstanceOf[Subcommand])

  solverAppModule.onSelect()

  def solve(): List[RetVal] = solverAppModule.solve()
}



