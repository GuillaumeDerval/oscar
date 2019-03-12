package oscar.cbls.business.scheduling.model

import scala.collection.mutable

abstract class Activity(index: Int, name: String) {
  def duration: Long
}

case class NormalActivity(index: Int, name: String, duration: Long) extends  Activity(index, name)

// Precedences

/**
  * This class is a container for the precedence constraints between activity indices
  *
  * @param numActivities the number of activities
  */
class Precedences(numActivities: Int, beforeAfterPairs: List [(Int, Int)]) {
  // Precedents array
  val precArray: Array[List[Int]] = Array.tabulate(numActivities)(_ => Nil)
  // Successors array
  val succArray: Array[List[Int]] = Array.tabulate(numActivities)(_ => Nil)

  for {(indA, indB) <- beforeAfterPairs} {
    // Check preconditions
    require(0 <= indA && indA < numActivities)
    require(0 <= indB && indB < numActivities)
    require(!descendants(indB).contains(indA))
    /////
    precArray(indB) ::= indA
    succArray(indA) ::= indB
  }


  /**
    * Gets a priority list according to the precedences constraints
    *
    * @return a list containing a permutation of [0..numActivity) corresponding
    *         to a consistent priority list (if A->B, index of A is before index of B in the list)
    */
  def getPriorityList: List[Long] = {
    def insertList(i: Int, succI: List[Int], accList: List[Long]): List[Long] = {
      accList match {
        case Nil => List(i)
        case x::xs =>
          if (succI.contains(x))
            i::accList
          else
            x::insertList(i, succI, xs)
      }
    }
    /////
    var result: List[Long] = Nil
    for {i <- precArray.indices} {
      result = insertList(i, succArray(i), result)
    }
    result
  }

  /**
    * Checks whether a list of activity indexes is consistent with the precedences
    *
    * @param seq a sequence of activity indices
    * @return true iff all indices in seq respect the precedences relation
    */
  def consistentSeq(seq: List[Int]): Boolean = {
    // Auxiliary function
    def consistentSeq(postfix: List[Int], revPrefix: List[Int]): Boolean = postfix match {
      case Nil => true
      case act::acts =>
        val notPrecPref = !revPrefix.exists(descendants(act).contains(_))
        if (notPrecPref)
          consistentSeq(acts, act::revPrefix)
        else
          false
    }
    /////
    consistentSeq(seq, Nil)
  }

  /**
    * Get a s list from the precedence relation
    *
    * @return a list of pairs (a, b) where a->b according to the precedence relation
    */
  def toPairsList: List[(Int, Int)] = {
    var pairsList: List[(Int, Int)] = Nil
    for {i <- precArray.indices} {
      val indPairs = precArray(i).map((i,_)).toList
      pairsList :::= indPairs
    }
    pairsList
  }

  /**
    * Determine the "descendants" of an activity in the precedence constraints
    *
    * @param indAct the index of the activity
    * @return a list of the indices for activities in the transitive closure of
    *         the precedence relation
    */
  private def descendants(indAct: Int): List[Int] = {
    // Auxiliary function
    def descendants(lstActs: List[Int], visitedActs: List[Int]): List[Int] = lstActs match {
      case Nil => visitedActs
      case act::acts =>
        if (visitedActs.contains(act)) {
          descendants(acts, visitedActs)
        }
        else {
          descendants(succArray(act).toList:::acts, act::visitedActs)
        }
    }
    /////
    descendants(List(indAct), Nil)
  }

  /**
    * To string
    *
    * @return a readable string for this precedence relation
    */
  override def toString: String = {
    val strPrec = precArray.foldLeft("[")((str, s) => s"$str{$s} ")
    val strSucc = succArray.foldLeft("[")((str, s) => s"$str{$s} ")
    s"Precedences:\n** Direct Precedences : $strPrec\n** Direct Successors : $strSucc"
  }
}

object Precedences {
  def apply(numActivities: Int, beforeAfterPairs: List[(Int, Int)]): Precedences =
    new Precedences(numActivities, beforeAfterPairs)
}