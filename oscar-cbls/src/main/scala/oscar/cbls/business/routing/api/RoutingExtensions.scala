package oscar.cbls.business.routing.api

import oscar.cbls.business.routing._
import oscar.cbls.business.routing.model.extensions._

import scala.collection.immutable.List

/**
  * Created by fg on 9/10/17.
  */
trait RoutingExtensions {

  /**
    * This class is used to represent the concept of chains.
    * A chain is a list of nodes within which each node is the predecessor of the next node in the list.
    * This class offers a list of methods and values usefull if you want to implement custom methods for your routing problem.
    *
    * @param vrp The basic vehicle routing problem
    * @param chains The list of chains => List of List of nodes
    * @return A Chains object
    */
  def chains(vrp: VRP, chains: List[List[Int]]) =
    new Chains(vrp, chains)
  type Chains = oscar.cbls.business.routing.model.extensions.Chains

  /**
    * This class is used to display your routing problem on a map.
    * Your routing problem can be displayed on a real map or a simple map
    * (blank frame with dots at there respective position)
    *
    * To use it after. Add this at the end of your search procedure : afterMove(myDisplayObject.drawRoutes())
    *
    * @param vrp The basic vehicle routing problem
    * @param nodePositions A list of node's position. nodePosition(0) represent the position of the first node
    * @param displayOnRealMap true if you want to display on a real map
    * @param selectRouteToDisplay true if you want a way to select the route to display (deprecated)
    * @param sizeOfMap The size of your map
    * @param refreshRate The refresh rate (be carefull if the refresh rate is to high you may have greate performance issues
    * @return A display object
    */
  def display(vrp: VRP,
              nodePositions: List[(Double,Double)],
              displayOnRealMap: Boolean = false,
              selectRouteToDisplay: Boolean = false,
              sizeOfMap: Option[Int] = None,
              refreshRate: Int = 100,
              title:String = "VRP with OscaR.cbls"
             ) =
    new Display(vrp,nodePositions,displayOnRealMap,selectRouteToDisplay,sizeOfMap,refreshRate, title)
  type Display = oscar.cbls.business.routing.model.extensions.Display

  /**
    * This class is only used to simplify the constraints creation.
    * The invariant used to set the time constraint is not a time dedicated invariant
    * so we need some extra information like earlylines, deadlines...
    *
    * This class serve as a data package
    *
    * @param earlylines For each node the time after which we can start our tasks
    * @param deadlines For each node the time before which the task has to be finished
    * @param taskDurations For each node the task's duration
    * @param maxWaitingDurations For each node the maximum among of time we can wait before starting the task.
    *                            e.g.: You can stay at a parking for a limited among of time.
    */
  def timeWindow(earlylines: Array[Int],
                 deadlines: Array[Int],
                 taskDurations: Array[Int],
                 maxWaitingDurations: Array[Int]) =
    new TimeWindow(earlylines,deadlines,taskDurations,maxWaitingDurations)
  type TimeWindow = oscar.cbls.business.routing.model.extensions.TimeWindow

}
