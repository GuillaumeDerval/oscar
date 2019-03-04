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

package oscar.cbls.lib.invariant.graph

import oscar.cbls.SetValue
import oscar.cbls.algo.graph._
import oscar.cbls.core._
import oscar.cbls.core.computation.{Domain, SetNotificationTarget}

import scala.collection.immutable.SortedSet

class DistanceInConditionalGraph(graph:ConditionalGraph,
                                 from:Int,
                                 to:Int,
                                 openConditions:SetValue,
                                 distanceIfNotConnected:Long)
                                (underApproximatingDistance:(Int,Int) => Long
                                 = {val underApproxDistanceMatrix = FloydWarshall.buildDistanceMatrix(graph,_ => true);
                                  (a:Int,b:Int) =>{
                                    underApproxDistanceMatrix(a)(b)
                                  }})
  extends IntInvariant(initialDomain = Domain(underApproximatingDistance(from,to) match{ case Long.MaxValue => distanceIfNotConnected case x => x} ,distanceIfNotConnected))
    with VaryingDependencies
    with SetNotificationTarget {

  registerStaticDependency(openConditions)
  private var key:ValueWiseKey = registerDynamicValueWiseDependency(openConditions)

  finishInitialization()

  val aStar = new RevisableAStar(graph, underApproximatingDistance)

  var listenedValues:SortedSet[Int] = SortedSet.empty
  def setListenedValueOnValueWiseKey(newListenedValues:SortedSet[Int]): Unit ={
    val toRemoveValues = listenedValues -- newListenedValues

    toRemoveValues.foreach(k => key.removeFromKey(k))

    val toAddValues = newListenedValues -- listenedValues
    toAddValues.foreach(k => key.addToKey(k))

    listenedValues = newListenedValues
  }

  //initialize the stuff
  scheduleForPropagation()

  def getPath:RevisableDistance =  aStar.search(
    graph.nodes(from),
    graph.nodes(to),
    {val o = openConditions.value; condition => o contains condition},
    true)

  def computeAffectAndAdjustValueWiseKey(){
    //println("computeAffectAndAdjustValueWiseKey")
    if(key==null) return //in this case,it will never be connected, and this was already checked.

    aStar.search(
      graph.nodes(from),
      graph.nodes(to),
      {val o = openConditions.value; condition => o contains condition},false)

    match{
      case d@Distance(from, to,distance:Long, requiredConditions, unlockingConditions,_) =>
        //println("computeAffectAndAdjustValueWiseKey" + d)
        setListenedValueOnValueWiseKey(requiredConditions ++ unlockingConditions)

        this := distance

      case n@NeverConnected(from,to) =>
        //println("computeAffectAndAdjustValueWiseKey" + n)
        //will only happen once at startup

        key.performRemove()
        key = null

        this := distanceIfNotConnected

      case n@NotConnected(from, to, unlockingConditions) =>
        //println("computeAffectAndAdjustValueWiseKey" + n)
        setListenedValueOnValueWiseKey(unlockingConditions)

        this := distanceIfNotConnected
    }
  }

  override def notifySetChanges(v: ChangingSetValue,
                                d: Int,
                                addedValues: Iterable[Long],
                                removedValues: Iterable[Long],
                                oldValue: SortedSet[Long],
                                newValue: SortedSet[Long]): Unit = {

    //this looks a bit drastic,
    // however, we are in a value-wise context;
    // so this method is only called
    // when something happened to the graph
    // that requires the path to be re-computed
    scheduleForPropagation()
  }

  override def performInvariantPropagation(): Unit = {
    //note: this will be called even if not needed simply because we have an output that requires propagation.
    computeAffectAndAdjustValueWiseKey()
  }

  /** To override whenever possible to spot errors in invariants.
    * this will be called for each invariant after propagation is performed.
    * It requires that the Model is instantiated with the variable debug set to true.
    */
  override def checkInternals(c: Checker): Unit = {

    //We rely on the existing Astar, but call it twice.

    val fwd = aStar.search(
      graph.nodes(from),
      graph.nodes(to),
      {val o = openConditions.value; condition => o contains condition},false)

    val bwt = aStar.search(
      graph.nodes(to),
      graph.nodes(from),
      {val o = openConditions.value; condition => o contains condition},false)

    (fwd,bwt) match{
      case (Distance(a, b,distance1, _, _,_),Distance(c, d,distance2, _, _,_)) =>
        require(this.value == distance1)
        require(distance1 == distance2)

      case (NeverConnected(a,b),NeverConnected(c,d)) =>

        require(this.value == distanceIfNotConnected)

      case (NotConnected(a, b, _),NotConnected(c, d, _))=>
        //println("computeAffectAndAdjustValueWiseKey" + n)
        require(this.value == distanceIfNotConnected)
      case _ => throw new Error("disagreeing aStar")
    }
  }
}
