package oscar.modeling.solvers.cp

import org.rogach.scallop._
import oscar.modeling.misc.scallop.HostnameParser
import oscar.modeling.models.ModelDeclaration
import oscar.modeling.solvers.cp.Branchings.BranchingInstantiator
import oscar.modeling.solvers.cp.decompositions.DecompositionStrategy
import oscar.modeling.solvers.cp.distributed.SimpleRemoteSolverSystem
import oscar.modeling.solvers.{SolveHolder, SolverApp, SolverAppModulable, SolverAppModule}

import scala.spores.NullarySpore

/**
  * A trait for SolverApp that indicates that the model is solvable using a distributed CP solver
  */
trait DistributedCPSolving extends SolverAppModulable with CPSearchHolder with CPDecompositionHolder {
  override def getModules: List[SolverAppModule] = new DistributedCPAppModule(this.app, this.md) :: new DistributedCPAppClientModule() :: super.getModules
}

class DistributedCPAppClientModule() extends SolverAppModule {
  class DistributedCPClientSubcommand extends Subcommand("distributed-cp-client") {
    descr("Starts a remote client for distributed computation of this model")
    val hostname = opt[String](name="host", descr = "Hostname/IP on which this client should be binded. Default is 127.0.0.1.", default = Some("127.0.0.1"))
    val port = opt[Int](name="port", descr = "Port on which this client should be binded.Default is 0 (automatic port selection).", default = Some(0))
    val registerDir = opt[String](name="register-dir", descr="Path to a dir where the client will create a file named hostname:port. Useful if you set port to 0.", default=None)
  }
  override val subcommand = new DistributedCPClientSubcommand
  override def solve[RetVal](): List[RetVal] = throw new RuntimeException("This method should never be called with a distributed cp client!")

  override def onSelect(): Unit = {
    new SimpleRemoteSolverSystem(subcommand.hostname(), subcommand.port(), subcommand.registerDir.get)
    //Close app, since the client has done its work
    System.exit(0)
  }
}
/**
  * Module for SolverApp that solves models using a distributed CP solver (using EPS)
  * @param app the SolverApp
  * @param modelDeclaration the ModelDeclaration linked to the SolverApp
  */
class DistributedCPAppModule(app: SolverApp[_], modelDeclaration: ModelDeclaration) extends SolverAppModule {
  class DistributedCPSubcommand extends Subcommand("distributed-cp") {
    descr("Solves this model using a distributed CP solver. Needs one or multiple client, that can be started with the 'distributed-cp-client' subcommand.")
    val timeout = opt[Int](name="timeout", short='t', descr = "Timeout for the *solving*, in milliseconds. 0 (default) means no timeout", default = Some(0))
    val nSols = opt[Int](name="nsols", short='n',  descr = "Maximum number of solutions to find before stopping the solve. 0 (default) tells the solver to find all the solutions", default = Some(0))
    val sppw = opt[Int](name="sppw", short='s', descr = "Number of subproblems per thread. Default is 100.", default = Some(100))
    val host = opt[(String, Int)](name="host",
      descr = "hostname and ip, separated by a ':', on which this master should be binded. Only useful if you use --remote. Should be callable by clients. Default is 127.0.0.1:0. Port 0 means that it will be automatically selected by the OS.",
      default = Some(("127.0.0.1", 0)))(singleArgConverter(HostnameParser.parse))
    val remoteList = opt[List[(String, Int)]](name="remote", short='r',
      descr = "Remote client hostname and port. Hostname and port should be separated by a ':' (if only hostname is provided, port defaults to 2001)",
      default = None)(listArgConverter(HostnameParser.parse))
    val remoteFile = opt[List[(String, Int)]](name="remote-file", short='f',
      descr = "Path to a file listing hostname:port of remote clients, one per line",
      default = None)(singleArgConverter(HostnameParser.parseFromFile))
    mutuallyExclusive(remoteList, remoteFile)
  }
  override val subcommand = new DistributedCPSubcommand

  override def solve[RetVal](): List[RetVal] = {
    val pg = new CPProgram[RetVal](modelDeclaration)
    val onSolution: () => RetVal = app.asInstanceOf[SolveHolder[RetVal]].onSolution
    if(onSolution == null)
      throw new RuntimeException("No onSolution defined in the SolverApp or in the ModelDeclaration")
    if(!onSolution.isInstanceOf[NullarySpore[_]])
      println("Warning: onSolution is not a spore. It may fail when invocated by the clients.")
    val search: BranchingInstantiator = app.asInstanceOf[CPSearchHolder].getCPSearch
    if(search == null)
      throw new RuntimeException("No search defined in the SolverApp or in the ModelDeclaration")
    val decompose: DecompositionStrategy = app.asInstanceOf[CPDecompositionHolder].getCPDecompositionStrategy
    if(decompose == null)
      throw new RuntimeException("No decomposition defined in the SolverApp or in the ModelDeclaration")
    val nSols = subcommand.nSols()
    val time = subcommand.timeout()
    pg.onSolution{onSolution()}
    pg.setSearch(search)
    pg.setDecompositionStrategy(decompose)

    val remoteList = subcommand.remoteList.get.getOrElse(subcommand.remoteFile())
    val result = pg.solveDistributed(remoteList, subcommand.host(), subcommand.sppw(), nSols, time)
    result._2
  }
}
