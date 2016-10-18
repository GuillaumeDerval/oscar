package oscar.modeling.solvers.cp

import java.nio.charset.Charset
import java.nio.file.{Files, Paths}
import java.util.concurrent.LinkedBlockingQueue

import akka.actor._
import akka.event.Logging
import akka.pattern.ask
import akka.remote.RemoteScope
import akka.routing.{BroadcastRoutingLogic, Router}
import akka.util.Timeout
import com.typesafe.config.{Config, ConfigFactory}
import oscar.modeling.constraints.Constraint
import oscar.modeling.misc.ComputeTimeTaken._
import oscar.modeling.misc.{SPSearchStatistics, SearchStatistics}
import oscar.modeling.misc.TimeHelper._
import oscar.modeling.models._
import oscar.modeling.solvers.cp.branchings.Branching
import oscar.modeling.solvers.cp.decompositions.DecompositionStrategy
import oscar.modeling.vars.IntVar

import scala.collection.mutable.ListBuffer
import scala.concurrent.duration.{Duration, _}
import scala.concurrent.{Await, Future}

/**
  * A CPProgram that can distribute works among a cluster
  *
  * @param md
  * @tparam RetVal
  */
class DistributedCPProgram[RetVal](md: ModelDeclaration with DecomposedCPSolve[RetVal] = new ModelDeclaration() with DecomposedCPSolve[RetVal])
  extends ModelProxy[DecomposedCPSolve[RetVal], RetVal](md) {
  implicit val program = this

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

  def setDecompositionStrategy(d: DecompositionStrategy): Unit = md.setDecompositionStrategy(d)

  def getDecompositionStrategy: DecompositionStrategy = md.getDecompositionStrategy

  /**
    * Starts the CPProgram locally on threadCount threads, on the current model
    *
    * @param threadCount number of threads to use. By default, it is the number of available CPU
    * @return
    */
  def solveLocally(threadCount: Int = Runtime.getRuntime.availableProcessors(), sppw: Int = 100, nSols: Int = 0, maxTime: Int = 0): (SearchStatistics, List[RetVal]) = {
    solveLocally(modelDeclaration.getCurrentModel.asInstanceOf[UninstantiatedModel], threadCount, sppw, nSols, maxTime)
  }

  /**
    * Starts the CPProgram locally on threadCount threads, on the current model
    *
    * @param model the model to solve
    * @param threadCount number of threads to use
    * @return
    */
  def solveLocally(model: UninstantiatedModel, threadCount: Int, sppw: Int, nSols: Int, maxTime: Int): (SearchStatistics, List[RetVal]) = {
    solve(model, sppw*threadCount,
      AkkaConfigCreator.local(),
      (system, masterActor) => List.fill(threadCount)(system.actorOf(SolverActor.props[RetVal](md, masterActor))),
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
    solveDistributed(modelDeclaration.getCurrentModel.asInstanceOf[UninstantiatedModel], remoteHosts, localhost, sppw, nSols, maxTime)
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

    solve(model, sppw*remoteHosts.length, config,
      (system, masterActor) => {
        remoteHosts.map(t => {
          val (hostnameL, portL) = t
          val address = Address("akka.tcp", "solving", hostnameL, portL)
          system.actorOf(SolverActor.props[RetVal](md, masterActor).withDeploy(Deploy(scope = RemoteScope(address))))
        })
      },
      nSols, maxTime
    )
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
  def solve(model: UninstantiatedModel, subproblemCount: Int, systemConfig: Config, createSolvers: (ActorSystem, ActorRef) => List[ActorRef], nSols: Int, maxTime: Int): (SearchStatistics, List[RetVal]) = {
    modelDeclaration.apply(model) {
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
      val watcher_thread = new Thread(new WatcherRunnable(watchers, outputQueue))
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
}

class SimpleRemoteSolverSystem(hostname: String, port: Int, registerDir: Option[String]) {
  val system = ActorSystem("solving", AkkaConfigCreator.remote(hostname, port))
  if(registerDir.isDefined) {
    val addr = system.asInstanceOf[ExtendedActorSystem].provider.getDefaultAddress
    val filename = addr.host.get+":"+addr.port.get
    val filepath = Paths.get(registerDir.get, filename)
    Files.write(filepath, scala.collection.JavaConverters.asJavaIterableConverter(List[CharSequence](addr.toString)).asJava, Charset.forName("UTF-8"))
  }
  Await.result(system.whenTerminated, Duration.Inf)
}

/**
  * Stores the default Akka config
  */
private object AkkaConfigCreator {
  def remote(hostname: String, port: Int): Config = {
    ConfigFactory.parseString(s"""
       akka {
         #loglevel = "DEBUG"
         actor {
           provider = "akka.remote.RemoteActorRefProvider"
           serializers {
             kryo = "com.twitter.chill.akka.AkkaSerializer"
             sp = "solvers.cp.DoSubproblemSerializer"
           }
           serialization-bindings {
             "solvers.cp.SolvingMessage" = kryo
             "solvers.cp.HelloMessage" = kryo
             "solvers.cp.StartMessage" = kryo
             "solvers.cp.AwaitingSPMessage" = kryo
             "solvers.cp.DoneMessage" = kryo
             "solvers.cp.BoundUpdateMessage" = kryo
             "solvers.cp.AskForSolutionRecap" = kryo
             "solvers.cp.SolutionRecapMessage" = kryo
             "solvers.cp.AllDoneMessage" = kryo
             "solvers.cp.SolutionMessage" = kryo
             "models.Model" = kryo
             "models.ModelDeclaration" = kryo
             "solvers.cp.DoSubproblemMessage" = sp
           }
         }
         remote {
           #log-received-messages = on
           #log-frame-size-exceeding = 10b
           enabled-transports = ["akka.remote.netty.tcp"]
           netty.tcp {
             hostname = "$hostname"
             port = $port
           }
         }
       }
     """)
  }

  def local(): Config = {
    ConfigFactory.load()
    //ConfigFactory.parseString(s"""akka { loglevel = DEBUG }""")
  }
}

/**
  * An actor that manages a collection of SolverActor
  *
  * @param modelDeclaration that contains an UninstantiatedModel as current model (the one to be solved)
  * @param subproblemQueue
  * @param outputQueue
  * @tparam RetVal
  */
class SolverMaster[RetVal](modelDeclaration: ModelDeclaration with DecomposedCPSolve[RetVal],
                           subproblemQueue: LinkedBlockingQueue[(Int, List[Constraint])],
                           outputQueue: LinkedBlockingQueue[SolvingMessage],
                           var maxSols: Int, maxTime: Int) extends Actor {
  val log = Logging(context.system, this)
  var solutionRecapReceived = 0

  val uninstantiatedModel = modelDeclaration.getCurrentModel.asInstanceOf[UninstantiatedModel]

  val solverForcedToSendImmediately = maxSols != 0 || !uninstantiatedModel.optimisationMethod.isInstanceOf[NoOptimisation]

  @volatile private var boundary: Int = uninstantiatedModel.optimisationMethod match {
    case m: Minimisation => uninstantiatedModel.getRepresentative(m.objective).max
    case m: Maximisation => uninstantiatedModel.getRepresentative(m.objective).min
    case _               => 0
  }

  var broadcastRouter = Router(BroadcastRoutingLogic())
  var terminating = false
  var stillAcceptSolutions = true

  // Contains the number of problems still to be computed/computing.
  // spRemaining >= subproblemQueue.size() at any time
  var spRemaining = subproblemQueue.size()

  /**
    * Process messages from master
    */
  def receive = {
    case AwaitingSPMessage() =>
      context watch sender
      broadcastRouter = broadcastRouter.addRoutee(sender)
      context.sender() ! ConfigMessage(solverForcedToSendImmediately)
      context.sender() ! BoundUpdateMessage(boundary) //ensure the new member has the last bound
      sendNextJob(sender)
    case a: DoneMessage =>
      spRemaining -= 1
      outputQueue.add(a)
      sendNextJob(sender)
      if (spRemaining == 0) {
        assert(subproblemQueue.isEmpty)

        //check SolutionRecap in main function if needed
        if(solverForcedToSendImmediately)
        {
          broadcastRouter.route(AllDoneMessage(), self)
          outputQueue.add(AllDoneMessage())
          terminating = true
          context.system.terminate()
        }
        else
        {
          broadcastRouter.route(AskForSolutionRecap(), self)
        }
      }
    case SolveTimeout() =>
      // We are done here
      stillAcceptSolutions = false
      broadcastRouter.route(AllDoneMessage(), self)
      outputQueue.add(AllDoneMessage())
      terminating = true
      context.system.terminate()
    case SolutionMessage(solution, None) =>
      if(stillAcceptSolutions) {
        outputQueue.add(SolutionMessage(solution, None))
        if(maxSols != 0) {
          maxSols -= 1
          if(maxSols == 0) {
            // We are done here
            stillAcceptSolutions = false
            broadcastRouter.route(AllDoneMessage(), self)
            outputQueue.add(AllDoneMessage())
            terminating = true
            context.system.terminate()
          }
        }
      }
    case SolutionMessage(solution, Some(b)) =>
      val (updateBound, newSolution) =  uninstantiatedModel.optimisationMethod match {
        case m: Minimisation    => (boundary > b, boundary > b)
        case m: Maximisation    => (boundary < b, boundary < b)
        case m: NoOptimisation  => (false, true)
      }
      if(updateBound) {
        boundary = b
        broadcastRouter.route(BoundUpdateMessage(b), self)
      }
      if(newSolution && stillAcceptSolutions) {
        outputQueue.add(SolutionMessage(solution, Some(b)))
        if(maxSols != 0) {
          maxSols -= 1
          if(maxSols == 0) {
            // We are done here
            stillAcceptSolutions = false
            broadcastRouter.route(AllDoneMessage(), self)
            outputQueue.add(AllDoneMessage())
            terminating = true
            context.system.terminate()
          }
        }
      }

    case m: SolutionRecapMessage[RetVal] =>
      if(stillAcceptSolutions)
        outputQueue.add(m)
      solutionRecapReceived += 1
      if(solutionRecapReceived == broadcastRouter.routees.length) {//all done
        outputQueue.add(AllDoneMessage())
        terminating = true
        context.system.terminate()
      }
    case a: WatcherMessage => outputQueue.add(a)
    case DeadLetter(message: Any, sender: ActorRef, recipient: ActorRef) =>
      if(!terminating) {
        println("Dead letter! Forcing shutdown...")
        System.exit(1)
      }
    case _ => log.info("received unknown message")
  }

  /**
    * Send a new sp to a given actor. If there is no subproblem, do nothing
    *
    * @param to
    */
  def sendNextJob(to: ActorRef): Unit = {
    if (!subproblemQueue.isEmpty) {
      val next = subproblemQueue.poll()
      to ! DoSubproblemMessage(next._1, next._2)
    }
  }
}

/**
  * A solver actor, that solves one subproblem at once
  *
  * @param modelDeclaration that contains an UninstantiatedModel as current model (this is the one to be solved)
  * @tparam RetVal
  */
class SolverActor[RetVal](modelDeclaration: ModelDeclaration with DecomposedCPSolve[RetVal],
                          master: ActorRef) extends Actor with IntBoundaryManager {
  val log = Logging(context.system, this)

  // Config
  var forceImmediateSend: Boolean = false
  val forceClose = new SynchronizedForceClose

  DoSubproblemSerializer.add(modelDeclaration)

  import context.dispatcher

  val uninstantiatedModel = modelDeclaration.getCurrentModel.asInstanceOf[UninstantiatedModel]

  val cpmodel = new CPModel(uninstantiatedModel)
  cpmodel.cpSolver.silent = true

  @volatile private var boundary = 0

  val objv: IntVar = cpmodel.optimisationMethod match {
    case m: Minimisation =>
      boundary = cpmodel.getRepresentative(m.objective).max
      m.objective
    case m: Maximisation =>
      boundary = cpmodel.getRepresentative(m.objective).min
      m.objective
    case _ => null
  }

  val on_solution: () => RetVal = modelDeclaration.onSolution
  val foundSolutions: ListBuffer[RetVal] = ListBuffer()
  var solutionCount: Int = 0

  //var lastSolution: SolutionMessage[RetVal] = null

  val solution: Model => Unit = cpmodel.optimisationMethod match {
    case m: Minimisation =>
      (a) => {
        val v = cpmodel.getRepresentative(objv)
        this.updateBoundary(v.max)
        master ! SolutionMessage(on_solution(), Some(v.max))
        solutionCount += 1
        //lastSolution = SolutionMessage(on_solution(), Some(v.max))
      }
    case m: Maximisation =>
      (a) => {
        val v = cpmodel.getRepresentative(objv)
        this.updateBoundary(v.max)
        master ! SolutionMessage(on_solution(), Some(v.max))
        solutionCount += 1
        //lastSolution = SolutionMessage(on_solution(), Some(v.max))
      }
    case _ => (a) =>
      if(forceImmediateSend)
        master ! SolutionMessage(on_solution(), None)
      else {
        val v = on_solution()
        if(!v.isInstanceOf[Unit])
          foundSolutions.append(on_solution())
        solutionCount += 1
      }
  }

  val search: Branching = objv match {
    case null => new ForceCloseSearchWrapper(modelDeclaration.getSearch(cpmodel), forceClose)
    case _ => new IntBoundaryUpdateSearchWrapper(modelDeclaration.getSearch(cpmodel), this, forceClose, cpmodel.cpObjective)
  }

  /**
    * Process messages from master
    */
  def receive = {
    case m: HelloMessage => sender() ! m
    case ConfigMessage(newForceImmediateSend) => forceImmediateSend = newForceImmediateSend
    case StartMessage() => master ! AwaitingSPMessage()
    case DoSubproblemMessage(spid: Int, sp: List[Constraint]) =>
      Future {
        println("start")
        try{
          solve_subproblem(spid, sp)
        }
        catch {
          case a: Throwable => log.info("WTF ")
            a.printStackTrace()
        }
      }
    case BoundUpdateMessage(newBound: Int) =>
      this.updateBoundary(newBound)
    case AskForSolutionRecap() =>
      //should only be sent if we are on a satisfaction problem, and forceImmediateSend is off
      assert(null == objv && !forceImmediateSend)
      master ! SolutionRecapMessage(solutionCount, foundSolutions.toList)
    case AllDoneMessage() =>
      forceClose.close()
      DoSubproblemSerializer.remove(modelDeclaration) //allow to run GC on the modelDeclaration
      context.stop(self)
    case _ => log.info("received unknown message")
  }

  /**
    * Solve the current subproblem; should be called from a Future.
    */
  def solve_subproblem(spid: Int, sp: List[Constraint]): Unit = {
    try{
      val t0 = getThreadCpuTime
      val info = modelDeclaration.apply(cpmodel) {
        cpmodel.cpSolver.startSubjectTo() {
          for(constraint <- sp)
            cpmodel.post(constraint)

          /*
           * Note: this has to be made after the call to the selection function, because it may overwrite the search
           * and the solution handling function AND may want to use the original status at decomposition
           */
          cpmodel.cpSolver.searchEngine.clearOnSolution()
          cpmodel.cpSolver.onSolution {
            solution(cpmodel)
          }
          cpmodel.cpSolver.search(search)

          /*
           * Set the current bound at start
           */
          if (null != objv) {
            cpmodel.cpObjective.updateWorstBound(getBoundary())
            cpmodel.cpObjective.best = getBoundary()
          }
        }
      }
      cpmodel.cpSolver.searchEngine.clearOnSolution()
      val t1 = getThreadCpuTime
      master ! DoneMessage(spid, t1 - t0, new SPSearchStatistics(info))
    }
    catch {
      case e: ClosedByForce => //ignore
    }
  }

  def getBoundary(): Int = boundary

  def updateBoundary(newval: Int) = boundary = newval
}

object SolverActor {
  def props[RetVal](modelDeclaration: ModelDeclaration with DecomposedCPSolve[RetVal], master: ActorRef): Props =
    Props(classOf[SolverActor[RetVal]], modelDeclaration, master)
}