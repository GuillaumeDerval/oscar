package oscar.cbls.business.seqScheduling.model

import scala.collection.mutable

/**
  * This class is a container for the precedence constraints between activity indices
  *
  * @param numActivities the number of activities
  */
class Precedences(numActivities: Int) {
  // Precedents array
  val precArray: Array[mutable.SortedSet[Int]] = Array.tabulate(numActivities)(_ => mutable.SortedSet[Int]())
  // Successors array
  val succArray: Array[mutable.SortedSet[Int]] = Array.tabulate(numActivities)(_ => mutable.SortedSet[Int]())

  /**
    * Adds the precedence (indA -> indB) to the container
    *
    * @param indA the index of activity A
    * @param indB the index of activity B
    */
  def addPrecedence(indA: Int, indB: Int): Unit = {
    // Check preconditions
    require(0 <= indA && indA < numActivities)
    require(0 <= indB && indB < numActivities)
    require(!descendants(indB).contains(indA))
    /////
    precArray(indB) += indA
    succArray(indA) += indB
  }

  def addPrecedence(actA: Activity, actB: Activity): Unit = {
    addPrecedence(actA.index, actB.index)
  }

  /**
    * Gets a priority list according to the precedences constraints
    *
    * @return a list containing a permutation of [0..numActivity) corresponding
    *         to a consistent priority list (if A->B, index of A is before index of B in the list)
    */
  def getPriorityList: List[Int] = {
    def descendants(ind: Int, blocked: List[Int]): List[Int] = {
      def descendants(inds: List[Int], acc: List[Int], blocked: List[Int]): List[Int] = {
        inds match {
          case Nil => acc.reverse
          case i::is =>
            val succI = succArray(i).filterNot(s => acc.contains(s) || blocked.contains(s)).toList
            descendants(succI:::is, i::acc, blocked)
        }
      }
      descendants(List(ind), Nil, blocked)
    }
    /////
    var result: List[Int] = Nil
    for {i <- 0 until numActivities} {
      if (!result.contains(i)) {
        result = descendants(i, result):::result
      }
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
        val notPrecPref = !revPrefix.exists(descendants(_).contains(act))
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