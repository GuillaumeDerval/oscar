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

package oscar.modeling.solvers.cp

import oscar.modeling.misc.SearchStatistics
import oscar.modeling.misc.scallop.HostnameParser
import oscar.modeling.models.{ModelDeclaration, UninstantiatedModel}
import org.rogach.scallop._
import oscar.modeling.solvers.cp.distributed.{DecomposedCPSolve, SimpleRemoteSolverSystem}

/**
  * Minimal configuration for a DistributedCPApp model; models with arguments should subclass this by doing
  *
  * override lazy val config = new DistributedCPAppConfig {
  *   //new options
  * }
  *
  * in their DistributedCPApp object.
  */
class CPAppConfig extends Subcommand("master") {
  descr("Starts a master for distributed computation of this model. Can also work alone using threads")
  val threads = opt[Int](name="local", short='l', descr = "Number of local threads to start", default = None)
  val subproblemsPerWorker = opt[Int](name="sppw", short = 's', descr = "Number of subproblems per worker. Default is 100", default = Some(100))
  val host = opt[(String, Int)](name="host",
    descr = "hostname and ip, separated by a ':', on which this master should be binded. Only useful if you use --remote. Should be callable by clients. Default is 127.0.0.1:0. Port 0 means that it will be automatically selected by the OS.",
    default = Some(("127.0.0.1", 0)))(singleArgConverter(HostnameParser.parse))
  val remoteList = opt[List[(String, Int)]](name="remote",
    descr = "Remote client hostname and port. Hostname and port should be separated by a ':' (if only hostname is provided, port defaults to 2001)",
    default = None)(listArgConverter(HostnameParser.parse))
  val remoteFile = opt[List[(String, Int)]](name="remote-file",
    descr = "Path to a file listing hostname:port of remote clients, one per line",
    default = None)(singleArgConverter(HostnameParser.parseFromFile))
  val enableGUI = opt[Boolean]("gui", descr = "Enable the GUI. Disabled by default")
  val timeout = opt[Int](name="timeout", short='t', descr = "Timeout for the *solving*, in milliseconds. 0 (default) means no timeout", default = Some(0))
  val nSols = opt[Int](name="nsols", short='n',  descr = "Maximum number of solutions to find before stopping the solve. 0 (default) tells the solver to find all the solutions", default = Some(0))
  mutuallyExclusive(threads, remoteList, remoteFile)

  footer("\nTo start a client (for distributed solving), see <command> client --help")
}

/**
  * Minimal configuration for a DistributedCPApp. This contains the client and master configuration, but most users will
  * only want to override config for the master; see DistributedCPAppConfig.
  *
  * @param arguments args from App
  */
class CPAppCompleteConfig(arguments: Seq[String]) extends ScallopConf(arguments) {
  val client = new Subcommand("client") {
    descr("Starts a remote client for distributed computation of this model")
    val hostname = opt[String](name="host", descr = "Hostname/IP on which this client should be binded. Default is 127.0.0.1.", default = Some("127.0.0.1"))
    val port = opt[Int](name="port", descr = "Port on which this client should be binded.Default is 0 (automatic port selection).", default = Some(0))
    val registerDir = opt[String](name="register-dir", descr="Path to a dir where the client will create a file named hostname:port. Useful if you set port to 0.", default=None)
  }

  lazy val master = new CPAppConfig
  addSubcommand(master)
  addSubcommand(client)
}

/**
  * An app for DistributedCPProgram that provides a simple CLI for any model
  *
  * @param modeld
  * @tparam RetVal
  */
abstract class CPApp[RetVal](modeld: ModelDeclaration with DecomposedCPSolve[RetVal] = new ModelDeclaration() with DecomposedCPSolve[RetVal])
  extends CPProgram[RetVal](modeld) with App {

  lazy val config = new CPAppConfig()             //laziness ensures that if the variable is overridden...

  lazy val completeConfig = new CPAppCompleteConfig({
    if(args.isEmpty) Array("master")
    else if(args(0) != "master" && args(0) != "client") Array("master") ++ args
    else args
  }){
    override lazy val master = config                      //..., it is loaded at this point
  }
  completeConfig.verify()

  completeConfig.subcommand match {
    case Some(completeConfig.master) =>
      if(completeConfig.master.enableGUI())
        this.registerWatcher(SubproblemGraphicalProgressBar.getRegisterer._1, SubproblemGraphicalProgressBar.getRegisterer._2)
      //Nothing more to do here, the execution should continue in the subclass
    case Some(completeConfig.client) =>
      //Start client
      new SimpleRemoteSolverSystem(completeConfig.client.hostname(), completeConfig.client.port(), completeConfig.client.registerDir.get)
      //Close app, since the client has done its work
      System.exit(0)
    case None =>
      completeConfig.printHelp()
      System.exit(1)
  }


  def solve(nSols: Int = config.nSols.get.get, maxTime: Int = config.timeout.get.get): (SearchStatistics, List[RetVal]) =
    solve(md.getCurrentModel.asInstanceOf[UninstantiatedModel], nSols, maxTime)
  def solve(model: UninstantiatedModel): (SearchStatistics, List[RetVal])  = solve(model, config.nSols.get.get, config.timeout.get.get)
  def solve(model: UninstantiatedModel, nSols: Int, maxTime: Int): (SearchStatistics, List[RetVal]) = {
    if(completeConfig.master.remoteList.isDefined || completeConfig.master.remoteFile.isDefined) {
      super.solveDistributed(model,
        completeConfig.master.remoteList.get.getOrElse(completeConfig.master.remoteFile()),
        completeConfig.master.host(),
        completeConfig.master.subproblemsPerWorker(),
        nSols, maxTime
      )
    }
    else if(completeConfig.master.threads.isDefined && completeConfig.master.threads() > 1) {
      super.solveParallel(model,
        completeConfig.master.threads(),
        completeConfig.master.subproblemsPerWorker(),
        nSols, maxTime
      )
    }
    else {
      super.solveLocally(model, nSols, maxTime)
    }
  }

  def solveSubjectTo(nSols: Int = config.nSols.get.get, maxTime: Int = config.timeout.get.get)(func: => Unit): (SearchStatistics, List[RetVal]) = {
    fork {
      func
      solve(nSols, maxTime)
    }
  }

}