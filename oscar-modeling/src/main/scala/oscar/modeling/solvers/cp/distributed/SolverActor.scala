package oscar.modeling.solvers.cp.distributed

import akka.actor.{Actor, ActorRef, Props}
import akka.event.Logging
import oscar.algo.search.Branching
import oscar.cp.CPIntVar
import oscar.modeling.constraints.Constraint
import oscar.modeling.misc.SPSearchStatistics
import oscar.modeling.misc.TimeHelper._
import oscar.modeling.models._
import oscar.modeling.models.cp.CPModel
import oscar.modeling.solvers.cp.Branchings.BranchingInstantiator

import scala.collection.mutable.ListBuffer
import scala.concurrent.Future

/**
  * A solver actor, that solves one subproblem at once
  */
class SolverActor[RetVal](parameters: ActorParameters[RetVal]) extends Actor with IntBoundaryManager {
  val modelDeclaration = parameters.modelDeclaration
  val onSolution = parameters.onSolution
  val getSearch = parameters.getSearch
  val master = parameters.master

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

  val objv: CPIntVar = cpmodel.optimisationMethod match {
    case m: Minimisation =>
      boundary = cpmodel.cpObjective.objVar.max
      cpmodel.cpObjective.objVar
    case m: Maximisation =>
      boundary = cpmodel.cpObjective.objVar.min
      cpmodel.cpObjective.objVar
    case _ => null
  }

  val foundSolutions: ListBuffer[RetVal] = ListBuffer()
  var solutionCount: Int = 0

  //var lastSolution: SolutionMessage[RetVal] = null

  val solution: Model => Unit = cpmodel.optimisationMethod match {
    case m: Minimisation =>
      (a) => {
        this.updateBoundary(objv.max)
        master ! SolutionMessage(onSolution(), Some(objv.max))
        solutionCount += 1
        //lastSolution = SolutionMessage(on_solution(), Some(v.max))
      }
    case m: Maximisation =>
      (a) => {
        this.updateBoundary(objv.max)
        master ! SolutionMessage(onSolution(), Some(objv.max))
        solutionCount += 1
        //lastSolution = SolutionMessage(on_solution(), Some(v.max))
      }
    case _ => (a) =>
      if(forceImmediateSend)
        master ! SolutionMessage(onSolution(), None)
      else {
        val v = onSolution()
        if(!v.isInstanceOf[Unit])
          foundSolutions.append(onSolution())
        solutionCount += 1
      }
  }

  val search: Branching = modelDeclaration.apply(cpmodel) {
    objv match {
      case null => new ForceCloseSearchWrapper(getSearch(cpmodel), forceClose)
      case _ => new IntBoundaryUpdateSearchWrapper(getSearch(cpmodel), this, forceClose, cpmodel.cpObjective)
    }
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
    case AllDoneMessage(completed) =>
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
  def props[RetVal](modelDeclaration: ModelDeclaration, onSolution: () => RetVal,
                    getSearch: BranchingInstantiator, master: ActorRef): Props =
    Props(classOf[SolverActor[RetVal]], ActorParameters(modelDeclaration, onSolution, getSearch, master))
}