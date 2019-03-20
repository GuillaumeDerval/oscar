package oscar.cbls.business.routing.invariants

import oscar.cbls._
import oscar.cbls.algo.dll.{DLLStorageElement, DoublyLinkedList}
import oscar.cbls.algo.graph._
import oscar.cbls.algo.quick.QList
import oscar.cbls.algo.seq.IntSequence
import oscar.cbls.business.routing.model.RoutingConventionMethods
import oscar.cbls.core._

import scala.collection.immutable.SortedSet

class RouteLengthOnConditionalGraph(routes:ChangingSeqValue,
                                    n:Int,
                                    v:Int,
                                    openConditions:ChangingSetValue,
                                    graph:ConditionalGraph,
                                    underApproximatingDistance:(Int,Int) => Long,
                                    distanceIfNotConnected:Int,
                                    distancePerVehicle:Array[CBLSIntVar])
  extends Invariant() with SeqNotificationTarget with SetNotificationTarget {

  require(v == distancePerVehicle.length)

  warning(
    openConditions.min == 0 && openConditions.max == graph.nbConditions-1,
    "RouteLengthOnConditionalGraph: openConditions should range on the conditions of the conditional graph")

  val nbConditions = graph.nbConditions

  registerStaticAndDynamicDependency(openConditions)
  registerStaticAndDynamicDependency(routes)
  finishInitialization()
  for(i <- distancePerVehicle) i.setDefiningInvariant(this)

  // //////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  val isConditionalEdgeOpen:Array[Boolean] = Array.fill(openConditions.max.toInt+1)(false)

  val aStarEngine = new RevisableAStar(graph:ConditionalGraph, underApproximatingDistance)

  //les Astar sont stockés par identifiant de Astar (utilisés comme id dans les messages de notification de set)
  //AStarValue = (minNode,maxNode,AStarResult,astarId,key)

  //pour gérer les changements dans le graphe
  //on a un array:
  //astarId => nodeFrom //on ne peut pas dire que le AStarID est le nodeFrom parce-que on veut gérer la symétrie en O(1)
  //quand on a une notification de setChange, on a forcément un ID de astar.
  //on va rechercher les deux noeuds, from et to, et on note les deux comme étant à réévaluer.

  //un array
  //nodeFrom => AStarValue
  //pour permettre de re-parcourir la séquence efficacement.

  //on a un invariant en une passes:
  //mise à jour immédiate à chaque notification
  //pas de checkpoint.
  //on utilise un vehicle searcher en log(v)

  //max two of them.
  val minNodeToAStarInfos:Array[QList[AStarInfo]] = Array.fill(n)(null)


  def getAStarInfo(node1:Long,node2:Long):AStarInfo = {
    val (minNode,maxNode) = if(node1 < node2)(node1,node2) else (node2,node1)

    val allInfoOnMinNode = minNodeToAStarInfos(minNode) // at most two

    if(allInfoOnMinNode.head.maxNode == maxNode){
      allInfoOnMinNode.head
    }else{
      //there are two of them, so we are the second one (of course)
      require(allInfoOnMinNode.tail.head.maxNode == maxNode)
      require(allInfoOnMinNode.tail.tail == null)
      allInfoOnMinNode.tail.head
    }
  }

  val conditionToAStarInfo:Array[DoublyLinkedList[AStarInfo]] = Array.tabulate(nbConditions)(_ => new DoublyLinkedList[AStarInfo]())


  val allAStarInfo:DoublyLinkedList[AStarInfo] = new DoublyLinkedList[AStarInfo]()

  class AStarInfo(val minNode:Int,
                  val maxNode:Int,
                  val result:RevisableDistance){

    private var myElements:QList[DLLStorageElement[AStarInfo]] = QList(allAStarInfo.addElem(this))

    for( c <- result.conditionsForRevisions){
      myElements = QList(conditionToAStarInfo(c).addElem(this),myElements)
    }

    minNodeToAStarInfos(minNode) = QList(this,minNodeToAStarInfos(minNode))

    val distance:Long = result match{
      case d:Distance =>
        d.distance
      case _ =>
        distanceIfNotConnected
    }

    /**
      * @return true if this info was valid and invalidated, false if it was already invalidated
      */
    def invalidate(): Unit ={
      while(myElements != null){
        myElements.head.delete()
        myElements = myElements.tail
      }

      if(minNodeToAStarInfos(minNode).head == this){
        minNodeToAStarInfos(minNode) = minNodeToAStarInfos(minNode).tail
      }else{
        //there are two of them, so we are the second one (of course)
        require(minNodeToAStarInfos(minNode).tail.head == this)
        require(minNodeToAStarInfos(minNode).tail.tail == null)
        minNodeToAStarInfos(minNode) = QList(minNodeToAStarInfos(minNode).head)
      }
    }
  }

  val fromNodeToAStarInfo:Array[AStarInfo] = Array.fill(n)(null)

  def computeDistanceAndSaveItAll(fromNode:Int,toNode:Int): AStarInfo = {
    val (minNode,maxNode) = if(fromNode < toNode)(fromNode,toNode) else (toNode,fromNode)
    val result = aStarEngine.search(
      graph.nodes(fromNode),
      graph.nodes(toNode),
      isConditionalEdgeOpen,
      includePath = false)

    //it performs the registration automatically.
    new AStarInfo(minNode, maxNode, result)
  }

  def dropAllAStarInfo(): Unit ={
    //we are forced to do this way because DLL are mutable things
    while(allAStarInfo.nonEmpty){
      allAStarInfo.head.invalidate()
    }
  }

  override def notifySetChanges(v: ChangingSetValue,
                                id: Int,
                                addedValues: Iterable[Long],
                                removedValues: Iterable[Long],
                                oldValue: SortedSet[Long],
                                newValue: SortedSet[Long]): Unit = {

    require(v == openConditions)

    var aStarToRecompute: List[AStarInfo] = Nil

    def recordTouchedCondition(closedCondition: Int){
      val l = conditionToAStarInfo(closedCondition).toList
      for (a <- l) a.invalidate()
      require(conditionToAStarInfo(closedCondition).isEmpty)
      aStarToRecompute = l ::: aStarToRecompute
    }

    for(closedCondition <- removedValues){
      recordTouchedCondition(closedCondition)
      isConditionalEdgeOpen(closedCondition) = false
    }
    for(openCondition <- addedValues){
      recordTouchedCondition(openCondition)
      isConditionalEdgeOpen(openCondition) = true
    }

    //now we have to recompute the hops, and update the corresponding vehicles
    for(aStar:AStarInfo <- aStarToRecompute){

      //find the vehicle
      val vehicle = vehicleSearcher(routes.value,aStar.minNode)

      //recompute the hop, and save the AstarInfo for later notifications
      val newAStarInfo = computeDistanceAndSaveItAll(aStar.minNode,aStar.maxNode)

      //update the route length of the vehicle
      val oldDistance = aStar.distance
      val newDistance = newAStarInfo.distance
      distancePerVehicle(vehicle) :+= (newDistance - oldDistance)
    }
  }


  // //////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  //there is no checkpoint at all.
  //a cached vehicle searcher is used here, and updated at each checkpoint.
  protected var vehicleSearcher:((IntSequence,Long)=>Long) = if(v == 1L) ((_,_) => 0L) else
    RoutingConventionMethods.cachedVehicleReachingPosition(routes.value, v)


  /**
    *
    * @param s a sequence of integers representing routes
    * @return the distance per vehicle or the total distance in a singleton array, according to the global "perVehicle" flag
    */
  private def computeAndAffectValueFromScratch(routes:IntSequence){
    require(allAStarInfo.isEmpty)
    var currentPosition = routes.explorerAtAnyOccurrence(0).get
    var currentVehicle:Int = 0
    var currentLength:Long = 0

    while(currentPosition.next match{
      case None => //at the end of the current vehicle, which is the last one
        //compute the last hop
        val lastHopToComeBack = computeDistanceAndSaveItAll(currentPosition.value,v-1).distance
        currentLength += lastHopToComeBack
        distancePerVehicle(v-1) := currentLength
        false
      case Some(nextPosition) if nextPosition.value < v =>
        //at the end of the current vehicle; starting a new one
        val lastHopToComeBack = computeDistanceAndSaveItAll(currentPosition.value,currentVehicle).distance
        currentLength += lastHopToComeBack
        distancePerVehicle(currentVehicle) := currentLength

        currentPosition = nextPosition
        currentVehicle += 1
        currentLength = 0
        require(currentVehicle == nextPosition.value)
        true
      case Some(nextPosition) if nextPosition.value >= v =>
        //carry on the current vehicle
        val newHop = computeDistanceAndSaveItAll(currentPosition.value,nextPosition.value).distance
        currentLength += newHop
        currentPosition = nextPosition
        true
    }){}

  }

  // //////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  override def notifySeqChanges(v: ChangingSeqValue, d: Int, changes: SeqUpdate): Unit ={
    digestUpdates(changes)
  }

  private def digestUpdates(changes:SeqUpdate):Unit = {
    changes match {
      case SeqUpdateDefineCheckpoint(prev,isStarMode,checkpointLevel) =>
        //we do not manage checkpoints at all
        digestUpdates(prev)

        //we update teh vehicle searcher, since many queries might be done on it.
        vehicleSearcher =
          if(v == 1L) (_,_) => 0L
          else RoutingConventionMethods.cachedVehicleReachingPosition(changes.newValue, v)

      case r@SeqUpdateRollBackToCheckpoint(checkpoint:IntSequence,checkpointLevel:Int) =>
        //we do not manage checkpoints at all
        digestUpdates(r.howToRollBack)

        //we update teh vehicle searcher, since many queries might be done on it.
        vehicleSearcher =
          if(v == 1L) (_,_) => 0L
          else RoutingConventionMethods.cachedVehicleReachingPosition(changes.newValue, v)

      case x@SeqUpdateMove(fromIncluded : Int, toIncluded : Int, after : Int, flip : Boolean, prev : SeqUpdate) =>
        //on which vehicle did we move?
        //also from --> to cannot include a vehicle start.
        digestUpdates(prev)
        if(x.isNop) return
        else if(x.isSimpleFlip){
          //this is a simple flip

          val oldPrevFromValue = prev.newValue.valueAtPosition(fromIncluded - 1L).get
          val oldSuccToValue = RoutingConventionMethods.routingSuccPos2Val(toIncluded,prev.newValue,v)

          val fromValue = x.fromValue
          val toValue = x.toValue

          //we do not care about order:-)
          val oldAStarBeforeMovedSegment = getAStarInfo(oldPrevFromValue,fromValue)
          val oldAStarAfterMovedSegment = getAStarInfo(toValue,oldSuccToValue)

          oldAStarBeforeMovedSegment.invalidate()
          oldAStarAfterMovedSegment.invalidate()

          val newAStarBeforeMovedSegment = computeDistanceAndSaveItAll(oldPrevFromValue,toValue)
          val newAStarAfterMovedSegment = computeDistanceAndSaveItAll(fromValue,oldSuccToValue)

          //for simple flip, there is no node cost to consider

          val vehicleOfMovedSegment = vehicleSearcher(prev.newValue, fromIncluded)

          distancePerVehicle(vehicleOfMovedSegment) :+= (
            newAStarBeforeMovedSegment.distance + newAStarAfterMovedSegment.distance
            - (oldAStarBeforeMovedSegment.distance + oldAStarAfterMovedSegment.distance))

        }else {
          //actually moving, not simple flip
          val oldPrevFromValue = prev.newValue.valueAtPosition(fromIncluded - 1L).get
          val oldSuccToIfNoLoopOpt = prev.newValue.valueAtPosition(toIncluded + 1L)
          val oldSuccToValue = oldSuccToIfNoLoopOpt match {
            case None => v - 1L
            case Some(value) => if (value < v) value - 1L else value
          }

          val fromValue = x.fromValue
          val toValue = x.toValue
          val afterValue = x.afterValue

          val oldSuccAfterValue = RoutingConventionMethods.routingSuccPos2Val(after, prev.newValue, v)

          val oldAStarBeforeMovedSegment = getAStarInfo(oldPrevFromValue,fromValue)
          val oldAStarAfterMovedSegment = getAStarInfo(toValue,oldSuccToValue)
          val oldAStarAfterAfter = getAStarInfo(afterValue,oldSuccAfterValue)

          oldAStarBeforeMovedSegment.invalidate()
          oldAStarAfterMovedSegment.invalidate()
          oldAStarAfterAfter.invalidate()

          val newAStarBeforeMovedSegment = computeDistanceAndSaveItAll(afterValue,if(flip) toValue else fromValue)
          val newAStarAfterMovedSegment = computeDistanceAndSaveItAll(if(flip) fromValue else toValue,oldSuccAfterValue)
          val newAStarReplacingSegment = computeDistanceAndSaveItAll(oldPrevFromValue,oldSuccToValue)


          //per vehicle, there might be some node cost to consider
          val vehicleOfMovedSegment = vehicleSearcher(prev.newValue, fromIncluded)
          val targetVehicleOfMove = vehicleSearcher(prev.newValue, after)
          assert(vehicleOfMovedSegment == vehicleSearcher(prev.newValue,toIncluded))

          if (vehicleOfMovedSegment == targetVehicleOfMove) {
            //the segment is moved to the same vehicle, so we do not consider node cost here

            distancePerVehicle(vehicleOfMovedSegment) :+= (
              newAStarReplacingSegment.distance + newAStarBeforeMovedSegment.distance + newAStarAfterMovedSegment.distance
                - (oldAStarBeforeMovedSegment.distance + oldAStarAfterMovedSegment.distance + oldAStarAfterAfter.distance))

          } else {
            //moving a segment to another vehicle, and per vehicle required.

            //summing the moved segment (this is slow, but it is requested to compute the cost per vehicle)
            val segmentLength = computeValueBetween(prev.newValue,
              vehicleOfMovedSegment,
              fromIncluded, fromValue,
              toIncluded,toValue)

            distancePerVehicle(vehicleOfMovedSegment) :+= (
              newAStarReplacingSegment.distance - (oldAStarBeforeMovedSegment.distance + oldAStarAfterMovedSegment.distance + segmentLength))

            distancePerVehicle(targetVehicleOfMove) :+= (
              newAStarBeforeMovedSegment.distance + segmentLength + newAStarAfterMovedSegment.distance - oldAStarAfterAfter.distance)
          }
        }

      case SeqUpdateInsert(value : Long, pos : Int, prev : SeqUpdate) =>
        digestUpdates(prev)

        val newSeq = changes.newValue

        val oldPrev = prev.newValue.valueAtPosition(pos-1L).get

        val oldSucc =prev.newValue.valueAtPosition(pos) match{
          case None => v-1L //at the end
          case Some(oldSuccIfNoLoop) =>  if(oldSuccIfNoLoop < v) oldSuccIfNoLoop-1L else oldSuccIfNoLoop
        }

        val oldAStar = getAStarInfo(oldPrev,oldSucc)
        oldAStar.invalidate()

        val newDistanceBefore = computeDistanceAndSaveItAll(oldPrev,value).distance
        val newDistanceAfter = computeDistanceAndSaveItAll(oldSucc,value).distance

        val vehicle = vehicleSearcher(newSeq, pos)
        distancePerVehicle(vehicle) :+= (newDistanceBefore + newDistanceAfter - oldAStar.distance)

      case x@SeqUpdateRemove(positionOfDelete : Int, prev : SeqUpdate) =>
        digestUpdates(prev)
        val removedValue = x.removedValue

        val oldPrevValue = prev.newValue.valueAtPosition(positionOfDelete-1).get
        val oldSuccValue = RoutingConventionMethods.routingSuccPos2Val(positionOfDelete, prev.newValue,v)

        //we do not care about order:-)
        val oldAStarBefore = getAStarInfo(oldPrevValue,removedValue)
        val oldAStarAfter = getAStarInfo(oldSuccValue,removedValue)

        oldAStarBefore.invalidate()
        oldAStarAfter.invalidate()

        val newDistance = computeDistanceAndSaveItAll(oldPrevValue,oldSuccValue).distance

        val vehicle = vehicleSearcher(prev.newValue,positionOfDelete)

        distancePerVehicle(vehicle) :+= (newDistance - (oldAStarBefore.distance + oldAStarAfter.distance))


      case SeqUpdateLastNotified(value:IntSequence) =>
        require(value quickEquals routes.value)
      //we are starting from the previous value
      case SeqUpdateAssign(value : IntSequence) =>
        //impossible to go incremental
        dropAllAStarInfo()
        computeAndAffectValueFromScratch(value)
    }
  }


  protected def computeValueBetween(s:IntSequence, vehicle:Long, fromPosIncluded:Long, fromValueIncluded:Long, toPosIncluded:Long, toValueIncluded:Long):Long = {
    if(fromPosIncluded == toPosIncluded) 0
    else if(fromPosIncluded < toPosIncluded) {
      var e = s.explorerAtPosition(fromPosIncluded).get
      var toReturn = 0

      while (e.position < toPosIncluded) {
        val nextPos = e.next.get
        toReturn += getAStarInfo(e.value,nextPos.value).distance
        e = nextPos
      }
      toReturn
    }else{
      //this is symmetric
      computeValueBetween(s:IntSequence, vehicle:Long, toPosIncluded:Long, toValueIncluded:Long, fromPosIncluded:Long, fromValueIncluded:Long)
    }
  }



  override def checkInternals(c : Checker) : Unit = {
    check(c, routes.value)
  }

  def check(c : Checker,s:IntSequence) {
    c.check(!distanceIsSymmetric || RouteLength.isDistanceSymmetric(distanceMatrix, n), Some("distance matrix should be symmetric if invariant told so"))

    val values = computeValueFromScratch(s)
    for (vehicle <- 0L until v) {
      c.check(distancePerVehicle(vehicle).newValue == values(vehicle), Some("distance(" + vehicle + ").value=" + distancePerVehicle(vehicle).newValue + " should == computeValueFromScratch(routes.value)(0L)" + values(vehicle)))
    }

    if (checkpoint != null) {
      val values = computeValueFromScratch(checkpoint)
      for (vehicle <- 0L until v) {
        if (isVehicleChangedSinceCheckpoint(vehicle))
          c.check(savedValues(vehicle) == values(vehicle))
      }
    }
  }
}

