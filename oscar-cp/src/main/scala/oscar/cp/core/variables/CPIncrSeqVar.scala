package oscar.cp.core.variables

import oscar.algo.Inconsistency

import oscar.algo.reversible.{ReversibleInt, ReversibleSet}
import oscar.cp.core.CPStore

class CPIncrSeqVar(
                  final val store: CPStore,
                  final val starts: Array[CPIntVar], //Starts of activities
                  final val ends: Array[CPIntVar], //Ends of activities
                  final val durations: Array[CPIntVar], //Durations of activities
                  final val transitions: Array[Array[Int]], //Transition times
                  final val dependencies: Array[Seq[Int]] = Array(), //Dependencies between activities (a -> b -> c -> ...), if one is not possible the whole seq is not
                  final val minTime: Int = 0,
                  final val maxTime: Int = Integer.MAX_VALUE,
                  final val name: String = "SEQUENCE_VAR"
                ) extends CPVar with Traversable[Int]{

  assert(starts.length == ends.length)
  private final val nActivities = starts.length

  //Domain
  private final val activities = Array.tabulate(nActivities)(i => i) //Activities
  private final val positions = Array.tabulate(nActivities)(i => i) //Activity indices
  private final val mandatory = new ReversibleSet(store) //Set of mandatory activities

  private final val visitedEnd: ReversibleInt = new ReversibleInt(store, 0) //end index of visited activities (excl.)
  private final val removedStart: ReversibleInt = new ReversibleInt(store, nActivities) //start index of removed activities (incl.)
  private final val currentTime: ReversibleInt = new ReversibleInt(store, minTime) //current completion time for visited seq

  //utility values
  val depCouples: Array[(Int, Int)] = dependencies.flatMap(seq => seq.zip(seq.tail))
  private final val forDependencies: Map[Int, Int] = depCouples.toMap
  private final val prevDependencies: Map[Int, Int] = depCouples.map(_.swap).toMap
  private final val allDependencies: Map[Int, Set[Int]] = dependencies
    .flatMap(seq => seq.map(a => (a, seq.toSet - a)))
    .toMap

  //Initial propagation:
  mandatoryCheck()
  removeUnfeasible()
  branchNext()

  /**
    * @return true if the domain of the variable has exactly one value, false if the domain has more than one value
    */
  def isBound: Boolean = visitedEnd.value >= removedStart.value

  //Feasability checks:

  /**
    * Checks that a feasible path exists between mandatory activities
    */
  def mandatoryCheck(): Unit = {
    if(!pathExists(lastVisited, currentTime, allMandatory.toSet)) throw Inconsistency
  }

  private def pathExists(l: Option[Int], t: Int, acts: Set[Int]): Boolean ={
    //if set empty: A path exists
    if(acts.isEmpty) return true

    //Checking that each activity can be reached
    for(a <- acts.filter(a => !prevDependencies.contains(a) || !acts.contains(prevDependencies(a)))){
      val arrival = math.max(t + transitions(l.getOrElse(a))(a), starts(a).min)
      val departure = arrival + durations(a).min
      if(arrival > starts(a).max || departure > ends(a).max || departure > maxTime) return false
    }

    //Branching on activities
    for(a <- acts.filter(a => !prevDependencies.contains(a) || !acts.contains(prevDependencies(a)))){
      //TODO: memoisation
      val departure = math.max(t + transitions(l.getOrElse(a))(a), starts(a).min) + durations(a).min
      if(pathExists(Some(a), departure, acts - a)) return true
    }

    false
  }

  //Propagation rules:

  /**
    * Removes unfeasible activities
    */
  def removeUnfeasible(): Unit = {
    for(a <- allPossible){
      val arrival = math.max(currentTime.value + transitions (lastVisited.getOrElse(a))(a), starts(a).min)
      val departure = arrival + durations (a).min
      if (arrival > starts(a).max || departure > ends(a).max || departure > maxTime){
        remove(a)
      }
    }
  }

  /**
    * Checks if need to visit mandatory activity next
    */
  def branchNext(): Unit = {
    val m = allMandatory.filter(canVisit).find(m => !detourPossible(m))
    if(m.nonEmpty) visit(m.get)
  }

  private def detourPossible(m: Int): Boolean = {
    for(a <- allVisitable.filter(_ != m)){
      val arra = math.max(currentTime.value + transitions (lastVisited.getOrElse(a))(a), starts(a).min)
      val depa = arra + durations (a).min
      val arrm = math.max(depa + transitions(a)(m), starts(m).min)
      val depm = arrm + durations (m).min
      if (arra <= starts(a).max && depa <= ends(a).max && arrm <= starts(m).max && depm <= ends(m).max && depm <= maxTime){
        return true
      }
    }
    false
  }

  /**
    * removes activity actId from the domain of possible activities
    */
  def remove(actId: Int): Unit = {
    val pos = positions(actId)
    if(pos >= removedStart.value) return //Already removed: Nothing to do
    if(isMandatory(actId) || isVisited(actId)){ //Trying to remove visited or mandatory activity: Inconsistency
      removedStart.decr()
      throw Inconsistency
    }

    removedStart.decr()
    val actId2 = activities(removedStart.value)
    activities(pos) = actId2
    activities(removedStart.value) = actId
    positions(actId) = removedStart.value
    positions(actId2) = pos

    //Removing dependent activities
    if(forDependencies.contains(actId)) remove(forDependencies(actId))
    if(prevDependencies.contains(actId)) remove(prevDependencies(actId))
  }

  def nVisited: Int = visitedEnd.value

  /**
    * sets next activity visited to actId
    */
  def visit(actId: Int): Unit = {
    if(isVisited(actId)) return //Activity already visited: nothing to do
    if(!canVisit(actId)){ //trying to visit not visitable activity: Inconsistency
      visitedEnd.incr()
      throw Inconsistency
    }

    //Updating activity and current time
    val arrival = math.max(currentTime.value + transitions(lastVisited.getOrElse(actId))(actId), starts(actId).min)
    val departure = arrival + durations(actId).min
    store.add(starts(actId) === arrival)
    store.add(durations(actId) === durations(actId).min)
    store.add(ends(actId) === departure)
    currentTime.setValue(departure)

    //Setting activity as visited
    val pos = positions(actId)
    val actId2 = activities(visitedEnd.value)
    activities(pos) = actId2
    activities(visitedEnd.value) = actId
    positions(actId) = visitedEnd.value
    positions(actId2) = pos
    visitedEnd.incr()

    //Adding dependent activities:
    allDependencies.getOrElse(actId, Set())
      .filter(a => !isVisited(a) && !isMandatory(a))
      .foreach(setActMandatory)

    //checking feasibility:
    mandatoryCheck()
    //propagation:
    removeUnfeasible()
    branchNext()
  }

  /**
    * @return true if the activity is visited
    */
  def isVisited(actId: Int): Boolean = positions(actId) < visitedEnd.value

  /**
    * @return the sequence of visited activities
    */
  def visited: Array[Int] = activities.take(visitedEnd.value)

  /**
    * @return the last activity visited
    */
  def lastVisited: Option[Int] = if(nVisited > 0) Some(activities(visitedEnd.value - 1)) else None

  /**
    * @return current end time of the sequence
    */
  def currentEndTime: Int = currentTime

  /**
    * @return true if the activity is possible
    */
  def isPossible(actId: Int): Boolean = {
    val pos = positions(actId)
    pos >= visitedEnd.value && pos < removedStart.value
  }

  /**
    * @return all the activities that can eventually be visited in the sequence
    */
  def allPossible: Array[Int] = activities.slice(visitedEnd.value, removedStart.value)

  /**
    * @return true if the activity is mandatory
    */
  def isMandatory(actId: Int): Boolean = mandatory.contains(actId)

  /**
    * @return all the mandatory activities that must eventually be visited
    */
  def allMandatory: Array[Int] = mandatory.toArray

  /**
    * Sets activity actId to mandatory
    */
  def setMandatory(actId: Int): Unit = {
    setActMandatory(actId)

    //Adding dependent activities:
    allDependencies.getOrElse(actId, Set())
      .filter(a => !isVisited(a) && !isMandatory(a))
      .foreach(setActMandatory)

    //checking feasability:
    mandatoryCheck()
  }

  //Not to be used outside setMandatory!!!
  private def setActMandatory(actId: Int): Unit = {
    if(isVisited(actId) || isMandatory(actId)) return //Activity already visited or mandatory: nothing to do
    if(!isPossible(actId)){ //trying to set removed activity as mandatory: Inconsistency
      mandatory.add(actId)
      throw Inconsistency
    }
    mandatory.add(actId)
  }

  /**
    * @return true if the activity is possible and can be directly visited
    */
  def canVisit(actId: Int): Boolean = {
    isPossible(actId) && (!prevDependencies.contains(actId) || isVisited(prevDependencies(actId)))
  }

  /**
    * @return all the activities that can be visited next
    */
  def allVisitable: Array[Int] = allPossible.filter(canVisit)

  override def foreach[U](f: Int => U): Unit = ???

  override def toString(): String = ???
}
