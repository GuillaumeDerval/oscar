package oscar.cbls.lib.invariant.graph

import oscar.cbls.algo.graph._
import oscar.cbls.core.computation.{IntNotificationTarget, SetNotificationTarget}
import oscar.cbls.core.{ChangingIntValue, ChangingSetValue, IntInvariant, ValueWiseKey}

import scala.collection.immutable.SortedSet

class DistanceInConditionalGraph(graph:ConditionalGraph,
                                 from:ChangingIntValue,
                                 to:ChangingIntValue,
                                 openConditions:ChangingSetValue,
                                 distanceIfNotConnected:Int)
                                (underApproximatingDistance:(Int,Int) => Option[Int]
                                 = {val m = FloydWarshall.buildDistanceMatrix(graph,_ => true); (a,b) => m(a)(b)})
  extends IntInvariant() with SetNotificationTarget with IntNotificationTarget{

  registerStaticAndDynamicDependency(from)
  registerStaticAndDynamicDependency(to)
  registerStaticDependency(openConditions)
  private var key:ValueWiseKey = registerDynamicValueWiseDependency(openConditions)

  val aStar = new RevisableAStar(graph, underApproximatingDistance)

  var listenedValues:SortedSet[Int] = SortedSet.empty
  def setListenedValueOnValueWiseKey(newListenedValues:SortedSet[Int]): Unit ={
    val toRemoveValues = newListenedValues -- listenedValues
    toRemoveValues.foreach(key.removeFromKey)

    val toAddValues = listenedValues -- newListenedValues
    toAddValues.foreach(key.addToKey)

    listenedValues = newListenedValues
  }

  //initialize the stuff
  computeAffectAndAdjustValueWiseKey()

  def computeAffectAndAdjustValueWiseKey(){
    aStar.search(
      graph.nodes(from.value),
      graph.nodes(to.value),
      {val o = openConditions.value; condition => o contains condition})
    match{
      case Distance(from:Node, to:Node,distance:Int, requiredConditions:SortedSet[Int], unlockingConditions:SortedSet[Int]) =>
        setListenedValueOnValueWiseKey(requiredConditions ++ unlockingConditions)

        this := distance

      case NeverConnected(from:Node,to:Node) =>
      //will only happen once at startup

        key.performRemove()
        key = null

        this := distanceIfNotConnected

      case NotConnected(from:Node, to:Node, unlockingConditions:SortedSet[Int]) =>
        setListenedValueOnValueWiseKey(unlockingConditions)

        this := distanceIfNotConnected
    }
  }

  override def notifySetChanges(v: ChangingSetValue,
                                d: Int,
                                addedValues: Iterable[Int],
                                removedValues: Iterable[Int],
                                oldValue: SortedSet[Int],
                                newValue: SortedSet[Int]): Unit = {
    scheduleForPropagation()
  }

  override def notifyIntChanged(v: ChangingIntValue, id: Int, OldVal: Int, NewVal: Int): Unit = {
    scheduleForPropagation()
  }

  override def performInvariantPropagation(): Unit = {
    computeAffectAndAdjustValueWiseKey()
  }
}
