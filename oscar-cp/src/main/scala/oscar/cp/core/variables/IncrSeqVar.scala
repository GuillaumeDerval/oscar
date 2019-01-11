package oscar.cp.core.variables
import oscar.algo.reversible.{ReversibleInt, ReversibleSet}
import oscar.cp.core.CPStore

class IncrSeqVar(
                  final val store: CPStore,
                  final val starts: Array[CPIntVar], //Starts of activities
                  final val ends: Array[CPIntVar], //Ends of activities
                  final val transitions: Array[Array[Int]], //Transition times
                  final val dependencies: Array[(Int, Int)], //Dependencies between activities
                  final val minTime: Int = 0,
                  final val maxTime: Int = Integer.MAX_VALUE,
                  final val name: String = "SEQUENCE_VAR"
                ) extends CPVar with Traversable[Int]{

  assert(starts.length == ends.length)
  private final val nActivities = starts.length

  //Domain
  private final val activities = Array.tabulate(nActivities) //Activities
  private final val next = new ReversibleSet(store) //Set of next possible activities
  private final val mandatory = new ReversibleSet(store) //Set of mandatory activities

  private final val visitedEnd: ReversibleInt = new ReversibleInt(store, nActivities) //activities before visitedEnd are visited
  private final val removedStart: ReversibleInt = new ReversibleInt(store, nActivities) //activities after removedStart are removed
  private final val currentTime: ReversibleInt = new ReversibleInt(store, nActivities) //current completion time for visited seq

  //utility values
  private final val actDependencies = dependencies.toMap

  //TODO: fill next with starting activities

  /**
    * @return true if the domain of the variable has exactly one value, false if the domain has more than one value
    */
  def isBound: Boolean = ???

  /**
    * removes activity actId from the domain of possible activities
    */
  def remove(actId: Int): Unit = ???

  /**
    * sets next activity visited to actId
    */
  def visit(actId: Int): Unit = ???

  /**
    * @return true if the activity is visited
    */
  def isVisited(actId: Int): Boolean = ???

  /**
    * @return the sequence of visited activities
    */
  def getVisited: Array[Int] = ???

  /**
    * @return the last activity visited
    */
  def getLastVisited: Int = ???

  /**
    * @return current end time of the sequence
    */
  def currentEndTime: Int = ???

  /**
    * @return true if the activity is possible
    */
  def isPossible(actId: Int): Boolean = ???

  /**
    * @return true if the activity is mandatory
    */
  def isMandatory(actId: Int): Boolean = ???

  /**
    * Sets activity actId to mandatory
    */
  def setMandatory(actId: Int): Unit = ???

  /**
    * @return all the activities that can eventually be visited in the sequence
    */
  def getPossible: Array[Int] = ???

  /**
    * @return all the activities that can be visited next
    */
  def getPossibleNext: Array[Int] = ???

  /**
    * @return all the mandatory activities that must eventually be visited
    */
  def getMandatory: Array[Int] = ???

  override def foreach[U](f: Int => U): Unit = ???

  override def toString(): String = ???
}
