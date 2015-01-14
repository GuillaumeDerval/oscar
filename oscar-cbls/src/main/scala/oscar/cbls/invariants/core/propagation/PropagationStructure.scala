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
/******************************************************************************
  * Contributors:
  *     This code has been initially developed by CETIC www.cetic.be
  *         by Renaud De Landtsheer
  ******************************************************************************/

package oscar.cbls.invariants.core.propagation

import oscar.cbls.invariants.core.algo.dag._
import oscar.cbls.invariants.core.algo.tarjan._
import oscar.cbls.invariants.core.algo.dll._
import collection.immutable.SortedMap
import collection.mutable.Queue
import oscar.cbls.invariants.core.algo.heap.{AggregatedBinomialHeap, AbstractHeap, BinomialHeap}
import oscar.cbls.invariants.core.computation.StorageUtilityManager
;

/**
 * a schedulingHandler handles the scheduling for a set of PE.
 *
 */
trait SchedulingHandler{
  /**
   * when a PE needs propagation, it schedules itself to his SH through this method
   * notice that a PE canno schedule itself for propagation if it has already been, and has not been propagated since
   * PE should ensure this by themselves, EG through an internal boolean variable
   * @param e
   */
  def scheduleForPropagation(e:PropagationElement)

  /**
    * @return the propagation structure that contains this, itself if it is a PS
    */
  def propagationStructure:PropagationStructure
}

/**
 * This class manages propagation among propagation elements.
 *
 * This class is intended to be extended, and the overriding class must implement
 * the method getPropagationElements that returns the propagation elements to be considered
 * Each propagation element has a UniqueID. Those should be assigned continuously starting from 0.
 *
 * It is to be used as follows: once the set of propagation elements is stabilized,
 * one must call setupPropagationStructure, which will built the necessary internal data structure
 * propagation are triggered by calling the propagate method.
 * additionally, before calling setupPropagationStructure, the method registerForPartialPropagation can be
 * called to specify propagation elements that might require lazy propagation.
 *
 *  Two debug mechanisms are provided: trace printing and debug mode.
 *
 *  A trace printing is provided; the propagation structure prints a trace of what it is propagating.
 *  This is activated by the Verbose parameter. All prints are preceded by ''PropagationStruture''
 *  This an be useful when checking the behavior of partial propagation.
 *
 *  A self-check method is called by the propagation structure after propagation is performed.
 *  This is activated by the Checker parameter.
 *  You should ensure that Asteroid is compiled with assert activated if you are using the debug mode.
 *  It will considerably slow down Asteroid, as other checks are implemented in the base modules.
 *
 *  Also, although this propagation structure is intended to support acyclic graph
 *  for the static dependency graph, you can deactivate the associated mechanism
 *  by setting the IsAcyclic graph to true.
 *  If unsure, set to false (or do not set; it is false by default),
 *  the engine will discover it by itself. See also method isAcyclic to query a propagation structure.
 *
 * @param verbose requires that the propagation structure prints a trace of what it is doing.
 * @param checker: set a Some[Checker] top check all internal properties of invariants after propagation, set to None for regular execution
 * @param noCycle is to be set to true only if the static dependency graph is acyclic.
 * @param topologicalSort if true, use topological sort, false, use distance to input, and associated faster heap data structure
 * @param sortScc true if SCC should be sorted, false otherwise. Set to true, unless you know what your are doing. Setting to false might provide a speedup, but propagation will not be single pass on SCC anymore
 * @author renaud.delandtsheer@cetic.be
 */
abstract class PropagationStructure(val verbose: Boolean, val checker:Option[Checker] = None, val noCycle: Boolean, val topologicalSort:Boolean, val sortScc:Boolean = true)
  extends SchedulingHandler{

  //priority queue is ordered, first on propagation planning list, second on DAG.

  /**This method is to be overridden and is expected to return the propagation elements
    * on which the propagation structure will reason.
    * The method is expected to return consistent result once the setupPropagationStructure method is called
    */
  def getPropagationElements: Iterable[PropagationElement]

  /**
   * @return the propagation structure that contains this, itself if it is a PS
   */
  override def propagationStructure: PropagationStructure = this


  /**This method is to be overridden and is expected to return the maximal value of the UniqueID
    * of the propagation elements
    * The method is expected to return consistent result once the setupPropagationStructure method is called
    */
  private var MaxID: Int = -1

  def getMaxID = MaxID

  def GetNextID(): Int = {
    MaxID += 1
    MaxID
  }

  private var acyclic: Boolean = false

  /**@return true if the propagation structure consider that his graph is acyclic, false otherwise.
    * call this after the call to setupPropagationStructure
    * If the propagation structure has been created with NoCycle set to true, this will return true
    */
  def isAcyclic: Boolean = acyclic

  private var StronglyConnexComponentsList: List[StronglyConnectedComponent] = List.empty

  /**To call when one has defined all the propagation elements on which propagation will ever be triggered.
    * It must be called before any propagation is triggered,
    * as it allows the propagation structure to build the necessary internal structures
    * @param DropStaticGraph if true, the propagation structure drops the static graph after setup.
    */
  protected def setupPropagationStructure(DropStaticGraph: Boolean) {

    if (verbose) {
      println("PropagationStructure: closing propagation structure. Propagations structure includes: ")
      getPropagationElements.foreach(p => println("+ " + p))
      println("PropagationStructure: end propagations structure includes; size=" + getPropagationElements.size)
    }

    val StrognlyConnectedComponents: List[List[PropagationElement]] =
      if (noCycle) {
        if (verbose) {
          println("PropagationStructure: IsAcyclic flag; assuming acyclic static dependency graph ")
        }
        var toreturn: List[List[PropagationElement]] = List.empty
        for (n <- getPropagationElements) toreturn = List(n) :: toreturn
        toreturn
      } else {
        //identification des composantes connexes
        val storageForTarjan = this.getNodeStorage[TarjanNodeData]
        storageForTarjan.initialize(() => new TarjanNodeData)
        TarjanWithExternalStorage.getStronlyConnexComponents[PropagationElement](
          getPropagationElements,
          p => p.getStaticallyListeningElements,
          storageForTarjan.get)
      }

    assert(getPropagationElements.size == StrognlyConnectedComponents.foldLeft(0)
      ((acc, component) => acc + component.size))

    //tri topologique sur les composantes fortement connexes
    acyclic = true
    StronglyConnexComponentsList = List.empty
    val ClusteredPropagationComponents: List[PropagationElement] = StrognlyConnectedComponents.map(a => {
      if (a.tail.isEmpty) {
        a.head
      } else {
        acyclic = false

        val c = if (sortScc) new StronglyConnectedComponentTopologicalSort(a, this, GetNextID())
        else new StronglyConnectedComponentNoSort(a,this,GetNextID())
        StronglyConnexComponentsList = c :: StronglyConnexComponentsList
        c
      }
    })

    buildFastPropagationTracks()

    //this performs the sort on Propagation Elements that do not belong to a strongly connected component,
    // plus the strongly connected components, considered as a single node. */
    var LayerCount = 0
    if (topologicalSort){
      computePositionsThroughTopologialSort(ClusteredPropagationComponents)
    }else{
      LayerCount = computePositionsThroughDistanceToInput(ClusteredPropagationComponents)+1
    }

    if (topologicalSort){
      ExecutionQueue = new BinomialHeap[PropagationElement](p => p.position, ClusteredPropagationComponents.size)
    }else{
      ExecutionQueue = new AggregatedBinomialHeap[PropagationElement](p => p.position, LayerCount)
    }

    Propagating = false
    PreviousPropagationTarget = null

    if (DropStaticGraph) dropStaticGraph()

    //variables are already able to propagate immediately before model close and if not monitored yet.

    ScheduledElements = List.empty
    for (e <- getPropagationElements) {
      e.rescheduleIfNeeded()
    }
    for (scc <- StronglyConnexComponentsList){
      scc.rescheduleIfNeeded()
    }
    //propagate() we do not propagate anymore here since the first query might require a partial propagation only
  }

  /**This computes the position of the clustered PE, that is: the SCC and the PE not belonging to an SCC*/
  private def computePositionsThroughTopologialSort(ClusteredPropagationComponents:List[PropagationElement]){
    if (verbose) println("PropagationStructure: Positioning through topological sort")
    var front: List[PropagationElement] = ClusteredPropagationComponents.filter(n => {n.setCounterToPrecedingCount(); n.position == 0})
    var position = 0 //la position du prochain noeud place.
    while (!front.isEmpty) {
      val n = front.head
      front = front.tail
      n.position = position
      position += 1
      front = n.decrementSucceedingAndAccumulateFront(front)
    }
    if (position != ClusteredPropagationComponents.size) {
      if (noCycle){
        throw new Exception("cycle detected in propagation graph, please set NoCycle flag to false when declaring your model")
      }else{
        throw new Exception("internal bug")
      }
    }
  }

  /**This computes the position of the clustered PE based on distance to input,
    * that is: the SCC and the PE not belonging to an SCC
    * @return the max Position, knowing that the first is zero*/
  private def computePositionsThroughDistanceToInput(ClusteredPropagationComponents:List[PropagationElement]):Int = {
    if (verbose) println("PropagationStructure: Positioning through layered sort")
    val front:Queue[PropagationElement] =  new Queue[PropagationElement]()
    for (pe <- ClusteredPropagationComponents){
      pe.setCounterToPrecedingCount()
      if(pe.position == 0) front.enqueue(pe)
    }
    front.enqueue(null) //null marker denotes when Position counter must be incremented
    var position = 0 //la position du prochain noeud place.
    var count = 0 //the number of PE
    var countInLayer = 0

    while (true) {
      val n = front.dequeue()
      if (n == null){
        if (front.isEmpty){
          if (verbose) println("PropagationStructure: Layer " + position + " #Elements:" + countInLayer)
          if (count != ClusteredPropagationComponents.size) {
            if (noCycle){
              throw new Exception("cycle detected in propagation graph although NoCycle was set to true")
            }else{
              throw new Exception("internal bug")
            }
          }
          return position+1
        }else{
          if (verbose) println("PropagationStructure: Layer " + position + " #Elements:" + countInLayer)
          countInLayer=0
          position +=1
          front.enqueue(null) //null marker denotes when Position counter must be incremented
        }
      }else{
        n.position = position
        count +=1
        countInLayer+=0
        for (pe <- n.decrementSucceedingAndAccumulateFront(List.empty)) front.enqueue(pe)
      }
    }
    0 //never reached
  }

  def dropStaticGraph() {
    for (p <- getPropagationElements) p.dropStaticGraph()
  }

  private var ScheduledElements: List[PropagationElement] = List.empty
  private var ExecutionQueue: AbstractHeap[PropagationElement] = null

  //I'v been thinking about using a BitArray here, but although this would slightly decrease memory
  // (think, relative to all the rest of the stored data), it would increase runtime
  private var FastPropagationTracks: SortedMap[PropagationElement, Array[Boolean]] =
    SortedMap.empty[PropagationElement, Array[Boolean]]

  /**to call before setupPropagationStructure to specify PropagationElements
    * on which one need partial propagation
    */
  def registerForPartialPropagation(p: PropagationElement) {
    FastPropagationTracks += ((p, null))
  }

  private var PreviousPropagationTarget: PropagationElement = null

  def isPropagating:Boolean = Propagating

  /**triggers the propagation in the graph.
    * this method will do nothing if called before setupPropagationStructure
    * if UpTo set to a PropagationElement,
    * and provided it has been registered through the registerForPartialPropagation method,
    * the propagation will be partial, targeting this element.
    * @param UpTo: the optional target of partial propagation
    */
  final def propagate(UpTo: PropagationElement = null) {
    if (!Propagating) {
      if (UpTo != null) {
        val Track = FastPropagationTracks.getOrElse(UpTo, null)
        val SameAsBefore = Track != null && PreviousPropagationTarget == UpTo
        propagateOnTrack(Track, SameAsBefore)
      } else {
        propagateOnTrack(null, false)
      }
      PreviousPropagationTarget = UpTo
    }
  }

  /**Builds and stores the partial propagation tracks*/
  private def buildFastPropagationTracks() {
    if (!FastPropagationTracks.isEmpty) {
      //calculer la reacheability sur le graphe statique par algo de Floyd Warshall
      //on prend les listening elements parce-que certains peuvent ne pas etre enregistres dans le modele
      // si ils sont en entree du graphe.
      val keys = FastPropagationTracks.keys
      for (n <- keys) {
        FastPropagationTracks += ((n, BuildFastPropagationtrack(n)))
      }
    }
  }

  /**Builds the partial propagation track for the specified target
    * @param target the propagation element for which we build the partial propagation track
    * @return an array of boolean: UniqueID => should the element with UniqueID be propagated for this target?
    */
  private def BuildFastPropagationtrack(target: PropagationElement): Array[Boolean] = {
    val Track: Array[Boolean] = new Array[Boolean](getMaxID + 1)
    for (i <- 0 to getMaxID) Track(i) = false

    var ToExplore: List[PropagationElement] = List(target)
    Track(target.uniqueID) = true

    while (!ToExplore.isEmpty) {
      val n = ToExplore.head
      ToExplore = ToExplore.tail
      for (nn <- n.getStaticallyListenedElements)
        if (nn.uniqueID != -1 && !Track(nn.uniqueID)) {
          ToExplore = nn :: ToExplore
          Track(nn.uniqueID) = true
        }
    }

    for (scc <- StronglyConnexComponentsList) {
      Track(scc.uniqueID) = Track(scc.propagationElements.head.uniqueID)
    }
    Track
  }

  private var PostponedComponents: List[PropagationElement] = List.empty

  /**
   * performs a propagation on a propagation track
   * if propagation track is omitte, total propagation is performed
   * @param Track the propagation track, an array indices_of_propagation_element -> should it be propagated now
   * @param SameAsBefore the previous propagation was on the same track, so that the postponed element are still postponed
   */
  private def propagateOnTrack(Track: Array[Boolean], SameAsBefore: Boolean) {
    if (Propagating) return
    Propagating = true

    if (!SameAsBefore) {
      var NewPostponed: List[PropagationElement] = List.empty
      for (e: PropagationElement <- PostponedComponents) {
        if (Track == null || Track(e.uniqueID)) {
          ScheduledElements = e :: ScheduledElements
        } else {
          NewPostponed = e :: NewPostponed
        }
      }
      PostponedComponents = NewPostponed
    } //if it is SameAsBefore, we do not check whether the elements are in the track,
    // as they are postponed, they are not in it anyway
    //notice that for partial propagation, connex components cannot be partially propagated
    // because they are strognly connected over the static propagation graph.

    if (verbose) {
      if (Track == null) println("PropagationStruture: start total propagation")
      else println("PropagationStruture: start partial propagation")
    }

    for (e: PropagationElement <- ScheduledElements) {
      if (Track == null || Track(e.uniqueID)) {
        ExecutionQueue.insert(e)
      } else {
        PostponedComponents = e :: PostponedComponents
      }
    }
    ScheduledElements = List.empty

    while (!ExecutionQueue.isEmpty) {
      ExecutionQueue.popFirst().propagate()
      for (e <- ScheduledElements) {
        if (Track == null || Track(e.uniqueID)) {
          ExecutionQueue.insert(e)
        } else {
          PostponedComponents = e :: PostponedComponents
        }
      }
      ScheduledElements = List.empty
    }

    if (verbose) println("PropagationStruture: end propagation")

    if (Track == null) {
      checker match {
        case Some(c) =>
          for (p <- getPropagationElements) {
            p.checkInternals(c)
          }
        case None =>
      }
    }
    Propagating = false
  }

  /**this method is used by propagationComponents to schedule themself for propagation. */
  def scheduleForPropagation(p: PropagationElement) {
    ScheduledElements = p :: ScheduledElements
  }

  /**this variable controls propagation.
    * initially true to avoid spurious propagation during the construction of the data structure;
    * set to false by setupPropagationStructure
    */
  var Propagating: Boolean = true

  /**this variable is set by the propagation element to notify that they are propagating.
    * it is used to ensure that no propagation element perform illegal operation
    * such as writing a variable they do not control, etc)*/
  var PropagatingElement: PropagationElement = null

  /**returns the propagation element that is currently propagating.
    * it allows one to ensure that the propagating element behaves as declared in its dependencies
    */
  def getPropagatingElement: PropagationElement = PropagatingElement

  /*This dumps the propagation graphs in a dot format, for documentation purposes
    * Static graph should only be set if the static graph has not been dropped
    * @param StaticGraph adds the static graph as red arrows
    * @param DynamicGraph adds the dynamic graph as blue arrows
    * @return a string that contains the dot format
    **/
  /*
  def dumpToDot(StaticGraph: Boolean, DynamicGraph: Boolean, Target:PropagationElement = null): String = {
    var ToReturn = "digraph PropagationStructure {\n"
    ToReturn += "   rankdir=LR;\n"
    def nodeName(p: PropagationElement) = "node" + p.uniqueID

    if(!StaticGraph && !DynamicGraph)
      throw new Exception("you want to dump to dot, but none of the static and dynamic graphs")

    for (e <- getPropagationElements if e.schedulingHandler == this) {
      if (! (!StaticGraph && e.isInstanceOf[BulkPropagator]))
        ToReturn += "   " + nodeName(e) + e.getDotNode + "\n"
    }

    for (scc <- StronglyConnexComponentsList){
      ToReturn += "   subgraph " + "cluster_"+nodeName(scc) + "{" + "\n"
      for (f <- scc.Elements) {
        ToReturn += "      " + nodeName(f) + f.getDotNode + "\n"
      }
      ToReturn += "   }" + "\n"
    }

    if (StaticGraph && DynamicGraph){
      for (e <- getPropagationElements) {
        for (f <- e.getStaticallyListenedElements if f.uniqueID != -1) {
          if (e.getDeterminingElement == f){
            //determining element, blue arrow
            ToReturn += "   " + nodeName(f) + " -> " + nodeName(e) + "[color = blue]" + "\n"
          }else if (e.getDynamicallyListenedElements.exists(p => p==f)){
            //in static and dynamic graph
            ToReturn += "   " + nodeName(f) + " -> " + nodeName(e) + "[color = red]" + "\n"
          }else{
            //only in static graph
            if(this.isAcyclic){
              ToReturn += "   " + nodeName(f) + " -> " + nodeName(e) + "[color = black style=dotted]" + "\n"
            }else{
              ToReturn += "   " + nodeName(f) + " -> " + nodeName(e) + "[color = black style=dotted constraint=false]" + "\n"
            }
          }
        }
        for (f <- e.getDynamicallyListenedElements if f.uniqueID != -1) {
          if (!e.getStaticallyListenedElements.exists(p => p==f)){
            //in dynamic graph and not in static one because of bulking
            ToReturn += "   " + nodeName(f) + " -> " + nodeName(e) + "[color = red]" + "\n"
          }
        }
      }
    }else if (StaticGraph) {
      for (e <- getPropagationElements) {
        for (f <- e.getStaticallyListenedElements if f.uniqueID != -1) {
          ToReturn += "   " + nodeName(f) + " -> " + nodeName(e) + "[color = black style=dotted]" + "\n"
        }
      }
    }else if (DynamicGraph) {
      for (e <- getPropagationElements) {
        for (f <- e.getDynamicallyListenedElements if f.uniqueID != -1) {
          if (e.getDeterminingElement == f){
            //determining element, blue arrow
            ToReturn += "   " + nodeName(f) + " -> " + nodeName(e) + "[color = blue]" + "\n"
          }else{
            //in dynamic graph
            ToReturn += "   " + nodeName(f) + " -> " + nodeName(e) + "[color = red]" + "\n"
          }
        }
      }
    }
    ToReturn + "}\n"
  }
*/

  /**Builds a dictionary to store data related to the PE.
    * the dictionary is O(1), based on an array.
    * It only works on PE that are registered to this structure.
    * The storage is not initialized, call the initialize to set it to some conventional value.
    * @tparam T the type stored in the data structure
    * @return a dictionary over the PE that are registered in the propagation structure.
    */
  def getNodeStorage[T](implicit X:Manifest[T]):NodeDictionary[T] = new NodeDictionary[T](this.MaxID)

  /** returns some info on the Propgation structure
    * call this after closing
    * @return
    */
  def stats:String = {
    "PropagationStructure(" + "\n" +
      "  declaredAcyclic: " + noCycle + "\n" +
      "  topologicalSort:" + topologicalSort + "\n" +
      "  sortScc:" + sortScc + "\n" +
      "  actuallyAcyclic:" + acyclic + "\n" +
      "  propagationElementCount:" + getPropagationElements.size + "\n" +
      "  StronglyConnectedComponentsCount:" + StronglyConnexComponentsList.size + "\n" +
      ")"
  }
}

/**This is a O(1) dictionary for propagation elements.
  * It is based on an array, and the keys it support is only the PE that have been reistered
  * to the propagation structure by the time this is instantiated.
  * WARNING: this is not efficient if you do not actually use many of the keys
  * because the instantiated array will be very large compared to your benefits.
  * This might kill cache and RAM for nothing
  *
  * @param MaxNodeID the maximal ID of a node to be stored in the dictionary (since it is O(1) it is an array, and we allocate the full necessary size
  * @tparam T the type stored in this structure
  * @author renaud.delandtsheer@cetic.be
  */
class NodeDictionary[T](val MaxNodeID:Int)(implicit val X:Manifest[T]){
  private val storage:Array[T] = new Array[T](MaxNodeID+1)

  def update(elem:PropagationElement,value:T){
    storage(elem.uniqueID)=value
  }

  def get(elem:PropagationElement):T = storage(elem.uniqueID)

  def initialize(value:() => T){for (i <- storage.indices) storage(i) = value()}
}

abstract class StronglyConnectedComponent(val propagationElements: Iterable[PropagationElement],
                                                val core: PropagationStructure, val _UniqueID: Int) extends PropagationElement with SchedulingHandler{
  schedulingHandler = core
  uniqueID = _UniqueID

  for (e <- propagationElements) e.schedulingHandler = this

  def size: Int = propagationElements.size

  override def propagationStructure: PropagationStructure = core

  /** This returns the dot node to display on the DOT output for the node. Only the argument of the nodes
    * example: "[label= \"toto\" shape=diamond color=red]"
    * */
  def getDotNode: String = {
    throw new Exception("StrognlyConnectedComponent are handled as subgraph in dot files")
    ""
  }

  var ScheduledElements: List[PropagationElement] = List.empty

  def scheduleForPropagation(element: PropagationElement) {
    ScheduledElements = element :: ScheduledElements
    super.scheduleForPropagation()
  }

  override def decrementSucceedingAndAccumulateFront(acc: List[PropagationElement]): List[PropagationElement] = {
    var toreturn = acc
    for (element <- propagationElements){
      toreturn = element.decrementSucceedingAndAccumulateFront(toreturn)
    }
    toreturn
  }

  override def setCounterToPrecedingCount(): Boolean = {
    position = propagationElements.count(p => p.setCounterToPrecedingCount())
    (position != 0)
  }

  override private[core] def rescheduleIfNeeded() {}
  //we do nothing, since it is the propagation elements that trigger the registration if needed of SCC

  override def checkInternals(c: Checker) {
    for (e <- propagationElements) { e.checkInternals(c) }
  }
}

class StronglyConnectedComponentNoSort(Elements: Iterable[PropagationElement],
                                       core: PropagationStructure, _UniqueID: Int) extends StronglyConnectedComponent(Elements,core,_UniqueID) {

  override def performPropagation() {
    //TODO: this is performing a stack-based prioritization: maybe a queue would perform better? actually.
    while (!ScheduledElements.isEmpty) {
      val x = ScheduledElements.head
      ScheduledElements = ScheduledElements.tail
      x.propagate()
    }
  }
}

class StronglyConnectedComponentTopologicalSort(
                                                 override val propagationElements: Iterable[PropagationElement],
                                 override val core: PropagationStructure, _UniqueID: Int) extends StronglyConnectedComponent(propagationElements,core,_UniqueID) with DAG {

  for (e <- propagationElements) {
    e.setInSortingSCC()
  }
  for (e <- propagationElements) {
    e.initiateDynamicGraphFromSameComponent(this)
  }

  //for the DAG
  override def nodes = propagationElements.asInstanceOf[Iterable[DAGNode]]

  var newDependenciesToInject:List[WaitingDependency] = List.empty

  case class WaitingDependency(from:PropagationElement,
                               to:PropagationElement,
                               var inject1:(()=>Unit) = null,
                               var inject2:(()=>Unit) = null,
                               var isStillValid:(()=>Boolean) = null){
    /** injects the waiting dependency
     * @return true if the dependency was injected, false otherwise
     */
    def injectIfStillValid():Boolean = {
      if(isStillValid()) {
        inject1()
        inject2()
        true
      }else false
    }

    def inject(){
        inject1()
        inject2()
    }
  }

  def injectWaitingNewDependencies(autoSort:Boolean){
    for(d:WaitingDependency <- newDependenciesToInject){
      if(d.injectIfStillValid() && autoSort) notifyAddEdge(d.from,d.to)
    }
    newDependenciesToInject = List.empty
  }

  def registerListenedWaitingDependency(injector:(()=>Unit), isStillValid:(()=>Boolean)){
    if(autoSort){
      val waiting = newDependenciesToInject.head
      waiting.inject1 = injector
      waiting.isStillValid = isStillValid
    }else{
      injector()
    }
  }

  def registerListeningWaitingDependency(injector:(()=>Unit)){
    if(autoSort){
      val waiting = newDependenciesToInject.head
      waiting.inject2 = injector
    }else{
      injector()
    }
  }

  def addDependency(from:PropagationElement, to:PropagationElement){
    if(autoSort){
      newDependenciesToInject = WaitingDependency(from,to) :: newDependenciesToInject
    }
  }

  /** this is called when the dependency has been added and all its field are filled.
    * We take the opportunity to check if the dependency is by any chance already implemented
    * in the sort.
    * if yes, we inject it right away, since it does not trigger any computation, actually.
    */
  def dependencyAdded(){
    if(autoSort){
      val waiting = newDependenciesToInject.head
      if(waiting.from.position < waiting.to.position){
        waiting.inject()
        notifyAddEdge(waiting.from,waiting.to)
        newDependenciesToInject = newDependenciesToInject.tail
      }
    }
  }

  val h: BinomialHeap[PropagationElement] = new BinomialHeap[PropagationElement](p => p.position, size)

  override def performPropagation() {
    //setting autosort to true will not perform any operation unless it was set to false. This happens in two cases:
    //at the initial propagation, and when a stall just occurred. In these case, a non-incremental sort takes place

    injectWaitingNewDependencies(autoSort)
    autoSort = true

    for (e <- ScheduledElements) {
      h.insert(e)
    }
    ScheduledElements = List.empty

    var maxposition:Int = -1

    while (!h.isEmpty) {
      val x = h.popFirst()
      x.propagate()
      assert(x.position >= maxposition,"non monotonic propagation detected in SCC")
      assert({maxposition = x.position; true})

      for (e <- ScheduledElements) {
        h.insert(e)
      }
      ScheduledElements = List.empty
    }
  }
}

object PropagationElement {
  implicit val Ord: Ordering[PropagationElement] = new Ordering[PropagationElement] {
    def compare(o1: PropagationElement, o2: PropagationElement) = o1.compareTo(o2) //the one of dagnode
  }
}

/**This class is used in as a handle to register and unregister dynamically to variables
  * @author renaud.delandtsheer@cetic.be
  * */
class KeyForElementRemoval(val keyForListenedElement: DPFDLLStorageElement[(PropagationElement, Any)]
                                , val keyForListeningElement: DPFDLLStorageElement[PropagationElement]){
  def performRemove(): Unit ={
    keyForListeningElement.delete()
    keyForListenedElement.delete()
  }
}

case object DummyKeyForElementRemoval extends KeyForElementRemoval(null,null){
  override def performRemove() = {}
}

/** this is a basic PE that actually does not integrate into the propagation network
  * it is used by constants
  */
trait BasicPropagationElement{

  protected[propagation] def registerStaticallyListeningElement(listening:PropagationElement){}

  /**
   * only if the listening is not varying its dependencies
   *
   * there is not scc because if someone call this, he is not dynamic PE, hence is not a boundary
   * it also has no dynamicallyListened stuff to update (only static stuff)
   * can only be called before model closing
   * @param listening the dynamically listening element
   * @param i: the payload that will be given for the notification, according to what the PE is supposed to do
   */
  protected[propagation] def registerDynamicallyListeningElementNoKey(listening:PropagationElement,i:Any){}

  /**
   * @param listening the listening element
   * @param sccOfListening the SCC in case listening is on he boundary, null otherwise
   * @param dynamicallyListenedElementDLLOfListening the PFDLL
   * @return a key for dependency removal
   */
  protected[propagation] def registerDynamicallyListeningElement(listening:PropagationElement,
                                                                 i: Any,
                                                                 sccOfListening:StronglyConnectedComponentTopologicalSort,
                                                                 dynamicallyListenedElementDLLOfListening:DelayedPermaFilteredDoublyLinkedList[PropagationElement,PropagationElement]):
  KeyForElementRemoval = DummyKeyForElementRemoval

  def schedulingHandler:SchedulingHandler = null
}

/**
 * it does not changes it listened elements
 * however, its listening elements might change, and a proper list must therefore be kept.
 */
trait PropagationElement extends BasicPropagationElement with DAGNode{

  def dropStaticGraph() {
    staticallyListenedElements = null
  }

  var dynamicallyListenedElementsFromSameComponent: Iterable[PropagationElement] = null
  var dynamicallyListeningElementsFromSameComponent: Iterable[PropagationElement] = null

  //dynamicallyListenedElementsFromSameComponent
  final def getDAGPrecedingNodes: Iterable[DAGNode] = dynamicallyListenedElementsFromSameComponent

  //dynamicallyListeningElementsFromSameComponent
  final def getDAGSucceedingNodes: Iterable[DAGNode] = dynamicallyListeningElementsFromSameComponent

  def initiateDynamicGraphFromSameComponent(stronglyConnectedComponentTopologicalSort: StronglyConnectedComponentTopologicalSort){
    initiateDynamicGraphFromSameComponentListening(stronglyConnectedComponentTopologicalSort)
    initiateDynamicGraphFromSameComponentListened(stronglyConnectedComponentTopologicalSort)
  }

  protected def initiateDynamicGraphFromSameComponentListening(stronglyConnectedComponentTopologicalSort: StronglyConnectedComponentTopologicalSort){
    def filterForListening(listeningAndPayload:(PropagationElement,Any),injector:(()=>Unit), isStillValid:(()=> Boolean)){
      if(stronglyConnectedComponentTopologicalSort == listeningAndPayload._1.schedulingHandler)
        stronglyConnectedComponentTopologicalSort.registerListeningWaitingDependency(injector)
    }

    dynamicallyListeningElementsFromSameComponent
      = dynamicallyListeningElements.delayedPermaFilter(filterForListening, (e) => e._1)
  }

  protected def initiateDynamicGraphFromSameComponentListened(stronglyConnectedComponentTopologicalSort: StronglyConnectedComponentTopologicalSort) {
    assert(stronglyConnectedComponentTopologicalSort == schedulingHandler)
    //filters the list of staticallyListenedElements

    dynamicallyListenedElementsFromSameComponent
      = staticallyListenedElements.filter(_.schedulingHandler == stronglyConnectedComponentTopologicalSort)
  }

  /** the thing to which we schedult ourselves for propagation
    * can be a SCC or a PS
    */
  override def schedulingHandler:SchedulingHandler = mySchedulingHandler
  def schedulingHandler_=(s:SchedulingHandler){mySchedulingHandler = s}
  private var mySchedulingHandler:SchedulingHandler = null

  def propagationStructure:PropagationStructure = schedulingHandler.propagationStructure

  /**set to true if the PropagationElement is scheduled for propagation, false otherwise.
    * this is managed by the PropagationElement
    */
  protected var isScheduled: Boolean = false

  private[propagation] var staticallyListenedElements: List[PropagationElement] = List.empty
  private[propagation] var staticallyListeningElements: List[PropagationElement] = List.empty


  private final val dynamicallyListeningElements: DelayedPermaFilteredDoublyLinkedList[(PropagationElement, Any), PropagationElement]
  = new DelayedPermaFilteredDoublyLinkedList[(PropagationElement, Any), PropagationElement]

  /**through this method, the PropagationElement must declare which PropagationElement it is listening to
    * in the static dependency graph. The result must be stable after the call to setupPropagationStructure.
    * to override*/
  private[core] final def getStaticallyListenedElements: Iterable[PropagationElement] = staticallyListenedElements

  /**through this method, the PropagationElement must declare which PropagationElement listen to it
    * in the static dependency graph. The result must be stable after the call to setupPropagationStructure.
    * to override*/
  private[core] final def getStaticallyListeningElements: Iterable[PropagationElement] = staticallyListeningElements

  private[core] final def getDynamicallyListeningElements: Iterable[(PropagationElement, Any)] = dynamicallyListeningElements

  protected[core] def getDynamicallyListenedElements: Iterable[PropagationElement] = staticallyListenedElements

  protected def registerStaticallyListenedElement(b: BasicPropagationElement){
    b.registerStaticallyListeningElement(this)
  }

  override protected[propagation] def registerStaticallyListeningElement(listening: PropagationElement){
    listening.staticallyListenedElements = listening :: listening.staticallyListenedElements
    staticallyListeningElements = this :: staticallyListeningElements
  }

  /**this will not return a key because we do not have varying dependencies*/
  protected def registerDynamicallyListenedElement(b:BasicPropagationElement, i: Any):KeyForElementRemoval = {
    b.registerDynamicallyListeningElementNoKey(this,i)
    null
  }

  /**
   * only if the listening is not varying its dependencies
   *
   * there is not scc because if someone call this, he is not dynamic PE, hence is not a boundary
   * it also has no dynamicallyListened stuff to update (only static stuff)
   * can only be called before model closing
   * @param listening the dynamically listening element
   */
  override protected[propagation] def registerDynamicallyListeningElementNoKey(listening: PropagationElement, i: Any){
    dynamicallyListeningElements.addElem(listening,i)
  }

  /**
   * @param listening the listening element
   * @param sccOfListening the SCC in case listening is on he boundary, null otherwise
   * @param dynamicallyListenedElementDLLOfListening the PFDLL
   * @return a key for dependency removal
   */
  override protected[propagation]
  def registerDynamicallyListeningElement(listening: PropagationElement, i: Any,
                                          sccOfListening: StronglyConnectedComponentTopologicalSort,
                                          dynamicallyListenedElementDLLOfListening: DelayedPermaFilteredDoublyLinkedList[PropagationElement,PropagationElement])
  : KeyForElementRemoval = {
    if(sccOfListening != null && sccOfListening == this.schedulingHandler){
      //this is only called once the component is established, so no worries.
      //we must call this before performing hte injection to create the waitingDependency in the SCC
      sccOfListening.addDependency(this,listening)
      val keyForListenedElement = dynamicallyListeningElements.addElem((listening, i))
      val keyForListeningElement = dynamicallyListenedElementDLLOfListening.addElem(this)
      sccOfListening.dependencyAdded()
      new KeyForElementRemoval(keyForListenedElement,keyForListeningElement)
    }else{
      val keyForListenedElement = dynamicallyListeningElements.addElem((listening, i))
      val keyForListeningElement = dynamicallyListenedElementDLLOfListening.addElem(this)
      new KeyForElementRemoval(keyForListenedElement, keyForListeningElement)
    }
  }

  def setInSortingSCC(){}


  def compare(that: DAGNode): Int = {
    assert(this.uniqueID != -1, "cannot compare non-registered PropagationElements this: [" + this + "] that: [" + that + "]")
    assert(that.uniqueID != -1, "cannot compare non-registered PropagationElements this: [" + this + "] that: [" + that + "]")
    this.uniqueID - that.uniqueID
  }

  def decrementSucceedingAndAccumulateFront(acc: List[PropagationElement]): List[PropagationElement] = {
    var toreturn = acc
    for (succeeding <- getStaticallyListeningElements){
      if (succeeding.schedulingHandler == schedulingHandler.propagationStructure || succeeding.schedulingHandler != this.schedulingHandler) {
        //not in the same SCC as us
        toreturn = succeeding.decrementAndAccumulateFront(toreturn)
      }
    }
    toreturn
  }

  final def decrementAndAccumulateFront(acc: List[PropagationElement]): List[PropagationElement] = {
    position -= 1
    if (position == 0) {
      //faut pusher qqchose
      schedulingHandler match{
        case scc:StronglyConnectedComponent =>
          scc.decrementAndAccumulateFront(acc)
        case s:PropagationStructure => this :: acc
      }
    } else {
      acc
    }
  }

  /**Sets the Position oto the number of element that need to be decremented, not belonging to same connex component
    * for connex component, set it to the number of element that are referenced from othercomponents
    * @return true if there is a dependency, false otherwise
    */
  def setCounterToPrecedingCount(): Boolean = {
    //le compteur est mis au nombre de noeud precedent qui ne sont pas dans la meme composante connexe
    schedulingHandler match{
      case scc:StronglyConnectedComponent =>
        position = this.getStaticallyListenedElements.count(p => p.schedulingHandler != scc && p.schedulingHandler != null)
      case ps:PropagationStructure =>
        position = this.getStaticallyListenedElements.count(p => p.schedulingHandler != null)
    }
    position != 0
  }

  /**to invoque to force inclusion of the propagation element in the current or next propagation wave. */
  final def scheduleForPropagation() {
    assert(schedulingHandler != null, "cannot schedule or propagate element out of propagation structure")
    if (!isScheduled) {
      isScheduled = true
      schedulingHandler.scheduleForPropagation(this)
    }
  }

  private[core] def rescheduleIfNeeded() {
    if (isScheduled) {
      if (this.propagationStructure.verbose) println("PropagationStruture: re-scheduled [" + this.getClass + "]")
      schedulingHandler.scheduleForPropagation(this)
    }
  }

  /**Performs the propagation, and some bookkeeping around it.
    */
  final def propagate() {
    assert(isScheduled) //could not be scheduled actually, if was propagated, but not purged from postponed (in case select propagation for input is implemented)
    assert(propagationStructure != null, "cannot schedule or propagate element out of propagation structure")
    assert({propagationStructure.PropagatingElement = this; true})
    if (propagationStructure.verbose) println("PropagationStruture: propagating [" + this + "]")
    performPropagation()
    isScheduled = false //to avoid registering SCC to the propagation structure every time...
    assert({propagationStructure.PropagatingElement = null; true})
  }

  /**this is the propagation method that should be overridden by propagation elements.
    * notice that it is only called in a propagation wave if:
    * 1: it has been registered for propagation since the last time it was propagated
    * 2: it is included in the propagation wave: partial propagation wave do not propagate all propagation elements;
    *    it only propagates the ones that come in the predecessors of the targeted propagation element
    *  overriding this method is optional, so an empty body is provided by default*/
  def performPropagation() {
    ;
  }

  /**This is the debug procedure through which propagation element can redundantly check
    * that the incremental computation they perform through the performPropagation method is correct
    * overriding this method is optional, so an empty body is provided by default
    */
  def checkInternals(c: Checker) {
    ;
  }

  /**This returns the dot node to display on the DOT output for the node. Only the argument of the nodes
    * example: "[label= \"toto\" shape=diamond color=red]"
    * */
//  def getDotNode: String
}

trait VaryingDependencies extends PropagationElement{
  //for cycle managing
  /**set to true if the PropagationElement is one that can break
    * or make dependency cycles in the dynamic dependency graph
    * managed by the PropagationComponent
    * basically, set to true if the determiningElement is not in the same component
    * and if this PropagationElement belongs to a cycle in the static dependency graph*/
  private var inSortingSCC: Boolean = false

  /**this sets the value of IsBoundary according to the definition of this variable
    * @return the value of IsBoundary*/
  override def setInSortingSCC() {
    assert(schedulingHandler.isInstanceOf[StronglyConnectedComponentTopologicalSort])
    require(determiningElement != null)
    require (determiningElement.schedulingHandler == null || determiningElement.schedulingHandler != this.schedulingHandler)
    inSortingSCC = true
  }

  private var determiningElement: BasicPropagationElement = null
  def getDeterminingElement = determiningElement

  /**must belong to the statically listened elements.
    * cannot be added to the dynamically listened ones
    * (it is added through this method, and you cannot remove it, so you do not get the key for removing it)
    * @param p the element that determines the dynamic dependencies of the propagation element
    * @param i an additional value that is stored in this element together with the reference to this,
    * can be use for notification purposes
    */
  protected final def registerDeterminingElement(p: BasicPropagationElement, i: Any) {
    p match {
      case pe: PropagationElement =>
        assert(this.getStaticallyListenedElements.exists(e => e == pe),
          "dependency to determining element " + p + " must be registered in static propagation graph")
        assert(determiningElement == null, "only one determining element is authorized")
        registerDynamicallyListenedElement(pe, i)
        determiningElement = pe
    }
  }

  private[propagation] final val dynamicallyListenedElements: DelayedPermaFilteredDoublyLinkedList[PropagationElement, PropagationElement]
  = new DelayedPermaFilteredDoublyLinkedList[PropagationElement, PropagationElement]

  override protected[core] def getDynamicallyListenedElements: Iterable[PropagationElement] = dynamicallyListenedElements

  override protected def registerDynamicallyListenedElement(b:BasicPropagationElement,i:Any):KeyForElementRemoval =
    b.registerDynamicallyListeningElement(
      this,
      i,
      if(inSortingSCC)schedulingHandler.asInstanceOf[StronglyConnectedComponentTopologicalSort] else null,
      dynamicallyListenedElements)

  override protected def initiateDynamicGraphFromSameComponentListened(stronglyConnectedComponentTopologicalSort: StronglyConnectedComponentTopologicalSort) {
    assert(stronglyConnectedComponentTopologicalSort == schedulingHandler)
    def filterForListened(listened: PropagationElement, injector: (() => Unit), isStillValid: (() => Boolean)): Unit = {
      if (stronglyConnectedComponentTopologicalSort == listened.schedulingHandler)
        stronglyConnectedComponentTopologicalSort.registerListenedWaitingDependency(injector, isStillValid)
    }
    dynamicallyListenedElementsFromSameComponent
      = dynamicallyListenedElements.delayedPermaFilter(filterForListened)
  }

  override def dropStaticGraph() {
    staticallyListenedElements = null
    staticallyListeningElements = null
  }
}


/**This is the node type to be used for bulking
  * @author renaud.delandtsheer@cetic.be
  * **/
trait BulkPropagator extends PropagationElement

/**
 * @author renaud.delandtsheer@cetic.be
 */
trait Checker {
  def check(verity: Boolean, traceOption: Option[String] = None)
}

/**
 * a checker that trows an error as soon as there is an error
 * @author renaud.delandtsheer@cetic.be
 */
case class ErrorChecker() extends Checker {
  def check(verity: Boolean, traceOption: Option[String]) = {
    if(! verity) throw new Error("Error in checker, debug: " + traceOption)
  }
}
