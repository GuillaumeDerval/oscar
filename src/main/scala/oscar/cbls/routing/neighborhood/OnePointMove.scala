package oscar.cbls.routing.neighborhood

/*******************************************************************************
 * This file is part of OscaR (Scala in OR).
 *
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/gpl-3.0.html
 ******************************************************************************/

/*******************************************************************************
 * Contributors:
 *     This code has been initially developed by CETIC www.cetic.be
 *         by Renaud De Landtsheer
 ******************************************************************************/

import oscar.cbls.search.SearchEngine
import oscar.cbls.algebra.Algebra._
import oscar.cbls.routing._


/**
 * Moves a point of a route to another place in the same or in an other route.
 * size if O(n²)
 */

object OnePointMove extends SearchEngine{
  def getBestMove(vrp:VRP with ObjectiveFunction with ClosestNeighborPoints with PositionInRouteAndRouteNr with Constraints,
    k:Int):OnePointMove = findMove(false, vrp,k)
  def getFirstImprovingMove(vrp:VRP with ObjectiveFunction with ClosestNeighborPoints with PositionInRouteAndRouteNr with Constraints,
    k:Int , startFrom:Neighbor = null):OnePointMove = findMove(true,vrp,k,startFrom)


  /**Search for the proper One point move
   *
   * @param FirstImprove if true, returns the first improving move, otherwise, searches for the best one
   * @param vrp the model of the problem
   */
  private def findMove(FirstImprove:Boolean,vrp:VRP with ObjectiveFunction with ClosestNeighborPoints
    with PositionInRouteAndRouteNr with Constraints, k:Int,startFrom:Neighbor = null):OnePointMove = {
    var BestObj:Int = vrp.ObjectiveVar.value
    var move:((Int, Int)) = null
    val hotRestart = if (startFrom == null) 0 else startFrom.startNodeForNextExploration

    for (beforeMovedPoint <- 0 until vrp.N startBy hotRestart if vrp.isRouted(beforeMovedPoint)){
      val movedPoint = vrp.Next(beforeMovedPoint).value
      for(insertionPoint <- vrp.getKNearestNeighbors(k,beforeMovedPoint) if (vrp.isRouted(insertionPoint)
        && beforeMovedPoint != insertionPoint && movedPoint != insertionPoint)){
          if(!vrp.isADepot(movedPoint) || (vrp.isADepot(movedPoint) && vrp.onTheSameRoute(movedPoint,insertionPoint))){
            if(!isStrongConstraintsViolated(beforeMovedPoint,insertionPoint, vrp)){
              val newObj = getObjAfterMove(beforeMovedPoint,insertionPoint, vrp)
              if (newObj < BestObj){
                if (FirstImprove) return OnePointMove(beforeMovedPoint,insertionPoint, newObj, vrp)
                BestObj = newObj
                move = ((beforeMovedPoint, insertionPoint))
              }
             }
          }
        }
    }
    if (move == null) null
    else OnePointMove(move._1,move._2, BestObj, vrp)
  }

  def doMove(predOfMovedPoint:Int, PutAfter:Int, vrp:VRP){
     val toUpdate = vrp.moveTo(predOfMovedPoint,vrp.Next(predOfMovedPoint).value,PutAfter)
     toUpdate.foreach(t => t._1 := t._2)
   }

  def isStrongConstraintsViolated(predOfMovedPoint:Int, PutAfter:Int, vrp:VRP with Constraints):Boolean = {
    val toUpdate = vrp.moveTo(predOfMovedPoint,vrp.Next(predOfMovedPoint).value,PutAfter)
    vrp.isViolatedStrongConstraints(toUpdate)
  }

  /*
    Evaluate the objective after a temporary one-point-move action thanks to ObjectiveFunction's features.
   */
  def getObjAfterMove(predOfMovedPoint:Int, PutAfter:Int, vrp:VRP with ObjectiveFunction):Int = {
    val toUpdate = vrp.moveTo(predOfMovedPoint,vrp.Next(predOfMovedPoint).value,PutAfter)
    vrp.getAssignVal(toUpdate)
  }
}

case class OnePointMove(val predOfMovedPoint:Int, val PutAfter:Int, objAfter:Int, vrp:VRP) extends Neighbor{
  def comit {OnePointMove.doMove(predOfMovedPoint, PutAfter, vrp)}
  def getObjAfter = objAfter
  override def toString():String = "(point = " + vrp.Next(predOfMovedPoint).value + ", insertion = " + PutAfter+" )"

  def startNodeForNextExploration: Int = predOfMovedPoint
}

