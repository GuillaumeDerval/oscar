package oscar.cbls.business.routing.invariants

import oscar.cbls._
import oscar.cbls.algo.dll.{DLLStorageElement, DoublyLinkedList}
import oscar.cbls.core._
import oscar.cbls.algo.graph._
import oscar.cbls.algo.quick.QList
import oscar.cbls.algo.seq.IntSequence
import oscar.cbls.business.routing.model.RoutingConventionMethods

import scala.collection.immutable.SortedSet

class RouteLengthOnConditionalGraph(routes:ChangingSeqValue,
                                    n:Int,
                                    v:Int,
                                    openConditions:ChangingSetValue,
                                    graph:ConditionalGraph,
                                    underApproximatingDistance:(Int,Int) => Long,
                                    distanceIfNotConnected:Int,
                                    distance:Array[CBLSIntVar])
  extends Invariant() with SeqNotificationTarget with SetNotificationTarget {

  val nbConditions = graph.nbConditions

  registerStaticDependency(openConditions) //no dynamic since we will use value-wise keys on each hop.
  registerStaticAndDynamicDependency(routes)
  finishInitialization()
  for(i <- distance) i.setDefiningInvariant(this)

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

  //on a un invariant en deux passes:
  //notification puis propagation
  //les notifications enregistrent
  // pour l'array de next: les nodeFrom ou il y a du changement
  //pour les Astart à refaire: les nodeFrom ou il y a du changement.
  // En passant, il faut noter le Astar comme étant à re-calculer pour ce faire, on stocke -1 dans le tableau des AstarValue to ID


  val aStarIDToFromNode:Array[Int] = Array.fill(n)(-1)
  var availableAstarIDs:QList[Int] = (0 until n).foldLeft(null:QList[Int])((acc:QList[Int],i:Int) => new QList(i,acc))

  def allocAstarID:Int = {
    val toReturn = availableAstarIDs.head
    availableAstarIDs = availableAstarIDs.tail
    toReturn
  }

  def freeAstarId(id:Int):Unit = {
    availableAstarIDs = QList(id,availableAstarIDs)
  }

  class AStarInfo(minNode:Int,
                  maxNode:Int,
                  result:RevisableDistance,
                  aStarID:Int){

    val distance:Long = result match{
      case d:Distance =>
        d.distance
      case _ =>
        distanceIfNotConnected
    }

    private var myElements:QList[DLLStorageElement[AStarInfo]] = null
    def recordPDLL(element: DLLStorageElement[AStarInfo]): Unit ={
      myElements = QList(element,myElements)
    }

    def invalidateAStart(): Unit ={
      while(myElements != null){
        myElements.head.delete()
        myElements = myElements.tail
      }
    }
  }

  val fromNodeToAStarInfo:Array[AStarInfo] = Array.fill(n)(null)

  def computeDistance(fromNode:Int,toNode:Int): AStarInfo ={
    val (minNode,maxNode) = if(fromNode < toNode)(fromNode,toNode) else (toNode,fromNode)
    val result = aStarEngine.search(
      graph.nodes(fromNode),
      graph.nodes(toNode),
      isConditionalEdgeOpen,
      includePath = false)

    val toReturn = new AStarInfo(minNode, maxNode, result, allocAstarID)

    for(value <- result.conditionsForRevisions) {
      toReturn.recordPDLL(valueToAStarInfos(value).addElem(toReturn))
    }
    toReturn
  }


  //this is an internal value-wise mechanism.
  val valueToAStarInfos:Array[DoublyLinkedList[AStarInfo]] = Array.fill(nbConditions)(new DoublyLinkedList[AStarInfo])


  override def notifySetChanges(v: ChangingSetValue,
                                id: Int,
                                addedValues: Iterable[Long],
                                removedValues: Iterable[Long],
                                oldValue: SortedSet[Long],
                                newValue: SortedSet[Long]): Unit = {

    //we can invalidate all the AStarInfo based on the values


  }


  // //////////////////////////////////////////////////////////////////////////////////////////////////////////////////


  //TODO: handle inactive checkpoints
  private val savedValues:Array[Long] = computeValueFromScratch(routes.value)
  protected var checkpoint = routes.value

  protected[this] val isVehicleChangedSinceCheckpoint:Array[Boolean] = Array.fill(v)(false)
  protected var changedVehiclesSinceCheckpoint:QList[Long] = null

  //only one level of stack for checkpoint here.

  protected var vehicleSearcher:((IntSequence,Long)=>Long) = if(v == 1L) ((_,_) => 0L) else
    RoutingConventionMethods.cachedVehicleReachingPosition(routes.value, v)

  affect(savedValues)

  override def notifySeqChanges(v: ChangingSeqValue, d: Int, changes: SeqUpdate): Unit ={
    if(!digestUpdates(changes)) {
      for(v <- 0L until this.v) recordTouchedVehicle(v)
      affect(computeValueFromScratch(changes.newValue))
    }
  }

  private def digestUpdates(changes:SeqUpdate):Boolean = {
    changes match {
      case SeqUpdateDefineCheckpoint(prev,isStarMode,checkpointLevel) =>
        //we only consider level 0L; other are not managed.
        if(checkpointLevel == 0L) {

          if (!digestUpdates(prev)) {
            affect(computeValueFromScratch(changes.newValue))
          }
          saveCurrentCheckpoint(changes.newValue)
          true
        }else{
          //ignore it altogether
          digestUpdates(prev)
        }

      case r@SeqUpdateRollBackToCheckpoint(checkpoint:IntSequence,checkpointLevel:Int) =>
        if(checkpointLevel == 0L) {
          require(checkpoint quickEquals this.checkpoint)
          restoreCheckpoint()
          true
        }else{
          digestUpdates(r.howToRollBack)
        }

      case SeqUpdateInsert(value : Long, pos : Int, prev : SeqUpdate) =>
        if(!digestUpdates(prev)) return false

        val newSeq = changes.newValue

        val oldPrev = prev.newValue.valueAtPosition(pos-1L).get

        val oldSucc =prev.newValue.valueAtPosition(pos) match{
          case None => v-1L //at the end
          case Some(oldSuccIfNoLoop) =>  if(oldSuccIfNoLoop < v) oldSuccIfNoLoop-1L else oldSuccIfNoLoop
        }

        val oldDistance = distanceMatrixOnNode(oldPrev)(oldSucc)
        val newDistance = distanceMatrixOnNode(oldPrev)(value) + distanceMatrixOnNode(value)(oldSucc)
        val nodeCost = distanceMatrixOnNode(value)(value)


        val vehicle = vehicleSearcher(newSeq, pos)
        recordTouchedVehicle(vehicle)
        distance(vehicle) :+= (newDistance + nodeCost - oldDistance)

        true

      case x@SeqUpdateMove(fromIncluded : Int, toIncluded : Int, after : Int, flip : Boolean, prev : SeqUpdate) =>
        //on which vehicle did we move?
        //also from --> to cannot include a vehicle start.
        if(!digestUpdates(prev)) false
        else if(x.isNop) true
        else if(x.isSimpleFlip){
          //this is a simple flip

          val oldPrevFromValue = prev.newValue.valueAtPosition(fromIncluded - 1L).get
          val oldSuccToValue = RoutingConventionMethods.routingSuccPos2Val(toIncluded,prev.newValue,v)

          val fromValue = x.fromValue
          val toValue = x.toValue

          val oldHopBeforeMovedSegment = distanceMatrixOnNode(oldPrevFromValue)(fromValue)
          val oldHopAfterMovedSegment = distanceMatrixOnNode(toValue)(oldSuccToValue)
          val newHopBeforeMovedSegment = distanceMatrixOnNode(oldPrevFromValue)(toValue)
          val newHopAfterMovedSegment = distanceMatrixOnNode(fromValue)(oldSuccToValue)

          //for simple flip, there is no node cost to consider

          val vehicleOfMovedSegment = vehicleSearcher(prev.newValue, fromIncluded)
          val deltaDistance = if(distanceIsSymmetric) 0L else {
            //there is a flip and distance is asymmetric
            computeValueBetween(prev.newValue,
              vehicleOfMovedSegment,
              toIncluded, toValue,
              fromIncluded, fromValue) -
              computeValueBetween(prev.newValue,
                vehicleOfMovedSegment,
                fromIncluded, fromValue,
                toIncluded, toValue)
          }
          recordTouchedVehicle(vehicleOfMovedSegment)
          distance(vehicleOfMovedSegment) :+= (newHopBeforeMovedSegment + newHopAfterMovedSegment
            - (oldHopBeforeMovedSegment + oldHopAfterMovedSegment) + deltaDistance)

          true
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

          val oldHopBeforeMovedSegment = distanceMatrixOnNode(oldPrevFromValue)(fromValue)
          val oldHopAfterMovedSegment = distanceMatrixOnNode(toValue)(oldSuccToValue)
          val oldHopAfterAfter = distanceMatrixOnNode(afterValue)(oldSuccAfterValue)

          val newHopBeforeMovedSegment = distanceMatrixOnNode(afterValue)(if(flip) toValue else fromValue)
          val newHopAfterMovedSegment = distanceMatrixOnNode(if(flip) fromValue else toValue)(oldSuccAfterValue)
          val newHopReplacingSegment = distanceMatrixOnNode(oldPrevFromValue)(oldSuccToValue)

          //per vehicle, there might be some node cost to consider
          val vehicleOfMovedSegment = vehicleSearcher(prev.newValue, fromIncluded)
          val targetVehicleOfMove = vehicleSearcher(prev.newValue, after)
          assert(vehicleOfMovedSegment == vehicleSearcher(prev.newValue,toIncluded))

          if (vehicleOfMovedSegment == targetVehicleOfMove) {
            //the segment is moved to the same vehicle, so we do not consider node cost here

            val (deltaDistance) = if(distanceIsSymmetric || !flip) 0L else {
              //there is a flip and distance is asymmetric
              computeValueBetween(prev.newValue,
                vehicleOfMovedSegment,
                toIncluded, toValue,
                fromIncluded, fromValue) -
                computeValueBetween(prev.newValue,
                  vehicleOfMovedSegment,
                  fromIncluded, fromValue,
                  toIncluded, toValue)
            }

            recordTouchedVehicle(vehicleOfMovedSegment)
            distance(vehicleOfMovedSegment) :+= (
              newHopReplacingSegment + newHopBeforeMovedSegment + newHopAfterMovedSegment
                - (oldHopBeforeMovedSegment + oldHopAfterMovedSegment + oldHopAfterAfter) + deltaDistance)

          } else {
            //moving a segment to another vehicle, and per vehicle required.

            //summing the moved segment (this is slow, but it is requested to compute the cost per vehicle)
            val oldCostInSegment = computeValueBetween(prev.newValue,
              vehicleOfMovedSegment,
              fromIncluded, fromValue,
              toIncluded,toValue)
            val newCostInSegment = if(distanceIsSymmetric || !flip) oldCostInSegment else{
              //there is a flip and distance is asymmetric
              computeValueBetween(prev.newValue,
                vehicleOfMovedSegment,
                toIncluded,toValue,
                fromIncluded, fromValue)
            }

            recordTouchedVehicle(vehicleOfMovedSegment)
            distance(vehicleOfMovedSegment) :+= (
              newHopReplacingSegment - (oldHopBeforeMovedSegment + oldHopAfterMovedSegment + oldCostInSegment))

            recordTouchedVehicle(targetVehicleOfMove)
            distance(targetVehicleOfMove) :+= (
              newHopBeforeMovedSegment + newCostInSegment + newHopAfterMovedSegment - oldHopAfterAfter)
          }

          true
        }

      case x@SeqUpdateRemove(position : Int, prev : SeqUpdate) =>
        //on which vehicle did we remove?
        //on which vehicle did we insert?
        val removedValue = x.removedValue
        //node cost to be considered
        if(!digestUpdates(prev)) return false

        val positionOfDelete = x.position

        val oldPrevValue = prev.newValue.valueAtPosition(positionOfDelete-1L).get //vehicles are never deleted
      val oldSuccValue = RoutingConventionMethods.routingSuccPos2Val(positionOfDelete, prev.newValue,v)
        val newDistance = distanceMatrixOnNode(oldPrevValue)(oldSuccValue)
        val oldDistanceBefore = distanceMatrixOnNode(oldPrevValue)(removedValue)
        val oldDistanceAfter = distanceMatrixOnNode(removedValue)(oldSuccValue)
        val nodeCost = distanceMatrixOnNode(removedValue)(removedValue)


        val vehicle = vehicleSearcher(prev.newValue,positionOfDelete)
        recordTouchedVehicle(vehicle)
        distance(vehicle) :+= (newDistance - (oldDistanceBefore + oldDistanceAfter + nodeCost))

        true

      case SeqUpdateLastNotified(value:IntSequence) =>
        require(value quickEquals routes.value)
        true //we are starting from the previous value
      case SeqUpdateAssign(value : IntSequence) =>
        false //impossible to go incremental
    }
  }

  /**
    * engages the saving of output values at this checkpoint.
    * you also must call  recordTouchedVehicle(v:Int) for this saving to be effective.
    * @param s
    */
  protected def saveCurrentCheckpoint(s:IntSequence){
    checkpoint = s

    while (changedVehiclesSinceCheckpoint != null) {
      isVehicleChangedSinceCheckpoint(changedVehiclesSinceCheckpoint.head) = false
      changedVehiclesSinceCheckpoint = changedVehiclesSinceCheckpoint.tail
    }

    //TODO: find stronger condition
    if(v > 1L) vehicleSearcher = RoutingConventionMethods.cachedVehicleReachingPosition(checkpoint,v)
  }

  private def restoreCheckpoint(){

    while (changedVehiclesSinceCheckpoint != null) {
      val v = changedVehiclesSinceCheckpoint.head
      distance(v) := savedValues(v)
      isVehicleChangedSinceCheckpoint(v) = false
      changedVehiclesSinceCheckpoint = changedVehiclesSinceCheckpoint.tail
    }

  }

  private def recordTouchedVehicle(v:Int){
    if(checkpoint!= null && !isVehicleChangedSinceCheckpoint(v)){
      savedValues(v) = distance(v).newValue
      isVehicleChangedSinceCheckpoint(v) = true
      changedVehiclesSinceCheckpoint = QList(v,changedVehiclesSinceCheckpoint)
    }

  }

  private def affect(value:Array[Long]){
    var currentV = distance.length
    while(currentV >0L){
      currentV -= 1L
      distance(currentV) := value(currentV)
    }
  }

  // labeled forward and backward nodes with their cumulated distance
  // use invalidation per vehicle in case more than one move is performed
  // just one thing: backtrack is only performed through checkpoint; star mode will lead to recomputation of the vehicles from scratch
  //datastruct for checkpoint: forward et bw labeling per vehicle. labeling: node -> (forward,backward) in a redBlack
  protected def computeValueBetween(s:IntSequence, vehicle:Long, fromPosIncluded:Long, fromValueIncluded:Long, toPosIncluded:Long, toValueIncluded:Long):Long = {
    if(fromPosIncluded <= toPosIncluded) {
      var e = s.explorerAtPosition(fromPosIncluded).get
      var toReturn = distanceMatrixOnNode(e.value)(e.value)

      while (e.position < toPosIncluded) {
        val nextPos = e.next.get
        toReturn += distanceMatrixOnNode(e.value)(nextPos.value) + distanceMatrixOnNode(nextPos.value)(nextPos.value)
        e = nextPos
      }
      toReturn
    }else{
      var e = s.explorerAtPosition(fromPosIncluded).get
      var toReturn = distanceMatrixOnNode(e.value)(e.value)

      while (e.position > toPosIncluded) {
        val prevPos = e.prev.get
        toReturn += distanceMatrixOnNode(e.value)(prevPos.value) + distanceMatrixOnNode(prevPos.value)(prevPos.value)
        e = prevPos
      }
      toReturn
    }
  }

  /**
    *
    * @param s a sequence of integers representing routes
    * @return the distance per vehicle or the total distance in a singleton array, according to the global "perVehicle" flag
    */
  private def computeValueFromScratch(s:IntSequence):Array[Long] = {
    val toReturn = Array.tabulate(v)(v => distanceMatrixOnNode(v)(v))
    val it = s.iterator

    var prevNode:Long = it.next()
    var currentVehicle:Long = prevNode
    require(currentVehicle == 0L)

    while(it.hasNext){
      val node = it.next()
      if(node < v){
        //reaching a new vehicle start
        //finishing the circle (cost of vehicle node already added)
        toReturn(currentVehicle) = toReturn(currentVehicle) + distanceMatrixOnNode(prevNode)(currentVehicle)
        currentVehicle = node
      }else{
        //continuing on the same vehicle
        toReturn(currentVehicle) = toReturn(currentVehicle) + distanceMatrixOnNode(prevNode)(node) +  distanceMatrixOnNode(node)(node)
      }
      prevNode = node
    }
    //for the last vehicle, the finishing operation in the loop will not be executed, so we have to add one here
    toReturn(currentVehicle) = toReturn(currentVehicle) + distanceMatrixOnNode(prevNode)(currentVehicle)

    toReturn
  }

  override def checkInternals(c : Checker) : Unit = {
    check(c, routes.value)
  }

  def check(c : Checker,s:IntSequence) {
    c.check(!distanceIsSymmetric || RouteLength.isDistanceSymmetric(distanceMatrix, n), Some("distance matrix should be symmetric if invariant told so"))

    val values = computeValueFromScratch(s)
    for (vehicle <- 0L until v) {
      c.check(distance(vehicle).newValue == values(vehicle), Some("distance(" + vehicle + ").value=" + distance(vehicle).newValue + " should == computeValueFromScratch(routes.value)(0L)" + values(vehicle)))
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

