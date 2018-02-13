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

package oscar.modeling.solvers.cp.distributed

import java.util.concurrent.LinkedBlockingQueue

import akka.actor.{Actor, ActorRef, DeadLetter}
import akka.event.Logging
import akka.routing.{BroadcastRoutingLogic, Router}
import oscar.modeling.constraints.Constraint
import oscar.modeling.models._
import oscar.modeling.solvers.cp._

/**
  * An actor that manages a collection of SolverActor
  *
  * @param modelDeclaration that contains an UninstantiatedModel as current model (the one to be solved)
  * @param subproblemQueue
  * @param outputQueue
  * @tparam RetVal
  */
class SolverMaster[RetVal](modelDeclaration: ModelDeclaration,
                           subproblemQueue: LinkedBlockingQueue[(Int, List[Constraint])],
                           outputQueue: LinkedBlockingQueue[SolvingMessage],
                           var maxSols: Int, maxTime: Int) extends Actor {
  val log = Logging(context.system, this)
  var solutionRecapReceived = 0

  val uninstantiatedModel = modelDeclaration.getCurrentModel.asInstanceOf[UninstantiatedModel]

  val solverForcedToSendImmediately = maxSols != 0 || maxTime != 0 || !uninstantiatedModel.optimisationMethod.isInstanceOf[NoOptimisation]

  @volatile private var boundary: Int = uninstantiatedModel.optimisationMethod match {
    case m: Minimisation => if(m.objective.max != Int.MaxValue) m.objective.max + 1 else Int.MaxValue
    case m: Maximisation => if(m.objective.min != Int.MinValue) m.objective.min - 1 else Int.MinValue
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
          broadcastRouter.route(AllDoneMessage(true), self)
          outputQueue.add(AllDoneMessage(true))
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
      broadcastRouter.route(AllDoneMessage(false), self)
      outputQueue.add(AllDoneMessage(false))
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
            broadcastRouter.route(AllDoneMessage(true), self)
            outputQueue.add(AllDoneMessage(true))
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
            broadcastRouter.route(AllDoneMessage(true), self)
            outputQueue.add(AllDoneMessage(true))
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
        outputQueue.add(AllDoneMessage(true))
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