package oscar.cbls.lib.invariant.graph

import oscar.cbls.SetValue
import oscar.cbls.algo.graph._
import oscar.cbls.core._
import oscar.cbls.core.computation.SetNotificationTarget

import scala.collection.immutable.SortedSet

class DistanceInConditionalGraph(graph:ConditionalGraph,
                                 from:Int,
                                 to:Int,
                                 openConditions:SetValue,
                                 distanceIfNotConnected:Int)
                                (underApproximatingDistance:(Int,Int) => Long
                                 = {val underApproxDistanceMatrix = FloydWarshall.buildDistanceMatrix(graph,_ => true);
                                  (a:Int,b:Int) =>{
                                    underApproxDistanceMatrix(a)(b)
                                  }})
  extends IntInvariant() with VaryingDependencies with SetNotificationTarget {

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
}
