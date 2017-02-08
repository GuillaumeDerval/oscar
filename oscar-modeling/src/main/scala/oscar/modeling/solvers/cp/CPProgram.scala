package oscar.modeling.solvers.cp

import java.util.concurrent.LinkedBlockingQueue

import akka.actor._
import akka.pattern.ask
import akka.remote.RemoteScope
import akka.util.Timeout
import com.typesafe.config.Config
import oscar.algo.search.{Branching, DFSearch}
import oscar.modeling.constraints.Constraint
import oscar.modeling.misc.ComputeTimeTaken._
import oscar.modeling.misc.{ComputeTimeTaken, SearchStatistics}
import oscar.modeling.models._
import oscar.modeling.models.operators.CPInstantiate
import oscar.modeling.solvers.cp.Branchings.{Alternative, BranchingInstantiator}
import oscar.modeling.solvers.SolveHolder
import oscar.modeling.solvers.cp.decompositions.DecompositionStrategy
import oscar.modeling.solvers.cp.distributed._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.concurrent.duration._
import scala.concurrent.Await

/**
  * A CPProgram that can distribute works among a cluster
  *
  * @param modelDeclaration
  * @tparam RetVal
  */
class CPProgram[RetVal](modelDeclaration: ModelDeclaration = new ModelDeclaration())
  extends SolveHolder[RetVal] with CPSearchHolder with CPDecompositionHolder with ModelDeclarationProxy {
  implicit val program = this
  override implicit val md = modelDeclaration

  protected val registeredWatchers: scala.collection.mutable.ListBuffer[(
      List[SubProblem] => Watcher[RetVal],
      Watcher[RetVal] => Unit
    )] = ListBuffer()

  /**
    * Register a new watcher to be added when the solving begins
    *
    * @param creator creates a new Watcher, given the subproblem list
    * @param initiator initiate the Watcher. Called after the resolution has begun.
    */
  def registerWatcher(creator: (List[SubProblem]) => Watcher[RetVal],
                      initiator: (Watcher[RetVal]) => Unit): Unit = {
    registeredWatchers += ((creator, initiator))
  }

  def getSearch: BranchingInstantiator = getCPSearch
  def setSearch(b: Branching): Unit = setCPSearch(b)
  def setSearch(b: => Seq[Alternative]): Unit = setCPSearch(b)
  def setSearch(b: BranchingInstantiator): Unit = setCPSearch(b)
  def setDecompositionStrategy(d: DecompositionStrategy): Unit = setCPDecompositionStrategy(d)
  def getDecompositionStrategy: DecompositionStrategy = getCPDecompositionStrategy

  /**
    * Starts the CPProgram locally on threadCount threads, on the current model
    *
    * @param threadCount number of threads to use. By default, it is the number of available CPU
    * @return
    */
  def solveParallel(threadCount: Int = Runtime.getRuntime.availableProcessors(), sppw: Int = 100, nSols: Int = 0, maxTime: Int = 0): (SearchStatistics, List[RetVal]) = {
    solveParallel(md.getCurrentModel.asInstanceOf[UninstantiatedModel], threadCount, sppw, nSols, maxTime)
  }

  /**
    * Starts the CPProgram locally on threadCount threads, on the current model
    *
    * @param model the model to solve
    * @param threadCount number of threads to use
    * @return
    */
  def solveParallel(model: UninstantiatedModel, threadCount: Int, sppw: Int, nSols: Int, maxTime: Int): (SearchStatistics, List[RetVal]) = {
    solveDistributed(model, sppw*threadCount,
      AkkaConfigCreator.local(),
      (system, masterActor) => List.fill(threadCount)(system.actorOf(SolverActor.props[RetVal](md, onSolution, getSearch, masterActor))),
      nSols, maxTime
    )
  }

  /**
    * Starts the CPProgram in a distributed fashion
    *
    * @param remoteHosts list of tuples (hostname, port) on which remote Akka ActorSystems can be contacted
    * @param localhost tupe (hostname, port) on which the local ActorSystem will be contacted by remote Actors
    * @return
    */
  def solveDistributed(remoteHosts: List[(String, Int)], localhost: (String, Int), sppw: Int = 100, nSols: Int = 0, maxTime: Int = 0): (SearchStatistics, List[RetVal]) = {
    solveDistributed(md.getCurrentModel.asInstanceOf[UninstantiatedModel], remoteHosts, localhost, sppw, nSols, maxTime)
  }

  /**
    * Starts the CPProgram in a distributed fashion
    *
    * @param model model to solve
    * @param remoteHosts list of tuples (hostname, port) on which remote Akka ActorSystems can be contacted
    * @param localhost tupe (hostname, port) on which the local ActorSystem will be contacted by remote Actors
    * @return
    */
  def solveDistributed(model: UninstantiatedModel, remoteHosts: List[(String, Int)], localhost: (String, Int), sppw: Int, nSols: Int, maxTime: Int): (SearchStatistics, List[RetVal]) = {
    val (hostname, port) = localhost
    val config = AkkaConfigCreator.remote(hostname, port)

    solveDistributed(model, sppw*remoteHosts.length, config,
      (system, masterActor) => {
        remoteHosts.map(t => {
          val (hostnameL, portL) = t
          val address = Address("akka.tcp", "solving", hostnameL, portL)
          system.actorOf(SolverActor.props[RetVal](md, onSolution, getSearch, masterActor).withDeploy(Deploy(scope = RemoteScope(address))))
        })
      },
      nSols, maxTime
    )
  }

  /**
    * Starts the CPProgram locally, on a single thread
    * @param model the model to solve
    * @param nSols the maximum number of solutions (0 = all)
    * @param maxTime the maximum running time, in ms (0 = infinite)
    * @return
    */
  def solveLocally(model: UninstantiatedModel, nSols: Int, maxTime: Int): (SearchStatistics, List[RetVal]) = {
    //Create watchers
    val createdWatchers = registeredWatchers.map((tuple) => {
      val watcher = tuple._1(List(new SubProblem(List(), Map())))
      (watcher, () => tuple._2(watcher))
    })
    val watcherMultiplexer = new WatcherMultiplexer(createdWatchers.map(_._1))

    //Enable watchers
    for(tuple <- createdWatchers)
      tuple._2()

    //Build stop condition
    val maxTimestamp = maxTime + System.currentTimeMillis()
    val stopCondition = (s: DFSearch) => {
      var stop = false
      stop |= (nSols != 0 && s.nSolutions >= nSols)
      stop |= (maxTime != 0 && System.currentTimeMillis() >= maxTimestamp)
      stop
    }

    val cpModel = CPInstantiate(model)
    val solutions = mutable.ArrayBuffer[RetVal]()
    md.apply(cpModel) {
      cpModel.cpSolver.onSolution {solutions += onSolution()}
      cpModel.cpSolver.search(getSearch(cpModel))
      val result = cpModel.cpSolver.startSubjectTo(stopCondition, Int.MaxValue, null)()
      (new SearchStatistics(result), solutions.toList)
    }
  }

  /**
    * Starts the CPProgram using possibly remote solvers, created by createSolvers
    *
    * @param model model to solve
    * @param subproblemCount number of subproblems needed
    * @param systemConfig akka config to use
    * @param createSolvers function that creates SolverActor
    * @return
    */
  def solveDistributed(model: UninstantiatedModel, subproblemCount: Int, systemConfig: Config, createSolvers: (ActorSystem, ActorRef) => List[ActorRef], nSols: Int, maxTime: Int): (SearchStatistics, List[RetVal]) = {
    md.apply(model) {
      ComputeTimeTaken.reset()
      val subproblems: List[SubProblem] = computeTimeTaken("decomposition", "solving") {
        getDecompositionStrategy.decompose(model, subproblemCount)
      }
      println("Subproblems: " + subproblems.length.toString)

      val queue = new LinkedBlockingQueue[(Int, List[Constraint])]()
      val outputQueue = new LinkedBlockingQueue[SolvingMessage]()

      for (s <- subproblems.zipWithIndex)
        queue.add((s._2, s._1.constraints))

      //Create watchers
      val createdWatchers = registeredWatchers.map((tuple) => {
        val watcher = tuple._1(subproblems)
        (watcher, () => tuple._2(watcher))
      })

      val statWatcher = new StatisticsWatcher[RetVal]
      val watchers = createdWatchers.map(_._1).toArray ++ Array[Watcher[RetVal]](statWatcher)
      val watcher_thread = new Thread(new DistributedWatcherRunnable(watchers, outputQueue))
      watcher_thread.start()

      val (system, masterActor, subsolvers) = computeTimeTaken("actor creation", "network") {
        val system = ActorSystem("solving", systemConfig)
        val masterActor = system.actorOf(Props(new SolverMaster(md, queue, outputQueue, nSols, maxTime)), "master")

        //Ensures masterActor is aware that there are DeadLetters
        system.eventStream.subscribe(masterActor, classOf[DeadLetter])

        // Create solvers and wait for them to be started
        val subsolvers: List[ActorRef] = createSolvers(system, masterActor)
        implicit val timeout = Timeout(2 minutes)
        subsolvers.map((a) => a ? HelloMessage()).map((f) => Await.result(f, Duration.Inf))

        (system, masterActor, subsolvers)
      }

      for(tuple <- createdWatchers)
        tuple._2()

      // Start solving
      computeTimeTaken("solving", "solving") {
        statWatcher.start()
        // TODO this maybe should be in SolverMaster
        subsolvers.foreach((a) => a ! StartMessage())

        if(maxTime != 0) {
          import system.dispatcher
          system.scheduler.scheduleOnce(maxTime milliseconds, masterActor, SolveTimeout())
        }

        Await.result(system.whenTerminated, Duration.Inf)
      }
      watcher_thread.join()

      showSummary()
      statWatcher.get
    }
  }

  override protected val solveRedirectTo: Any = md
}

