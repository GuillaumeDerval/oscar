package oscar.cbls.lib.search.combinators

import oscar.cbls._
import oscar.cbls.algo.heap.{BinomialHeapWithMove, BinomialHeap}
import oscar.cbls.core.search.{MoveFound, NoMoveFound, SearchResult, Neighborhood}

/**
 * this combinator always selects the best move between the two parameters
 * notice that this combinator makes more sense
 * if the two neighborhood return their best found move,
 * and not their first found move, as usually done.
 *
 * @author renaud.delandtsheer@cetic.be
 */
class Best(a: Neighborhood, b: Neighborhood) extends NeighborhoodCombinator(a, b) {

  override def getMove(obj: Objective, initialObj:Int, acceptanceCriteria: (Int, Int) => Boolean): SearchResult = {
    (a.getMove(obj, initialObj:Int, acceptanceCriteria), b.getMove(obj, initialObj:Int, acceptanceCriteria)) match {
      case (NoMoveFound, x) => x
      case (x, NoMoveFound) => x
      case (x: MoveFound, y: MoveFound) => if (x.objAfter < y.objAfter) x else y
    }
  }
}


case class BestSlopeFirst(l:List[Neighborhood],
                          tabuLength:Int = 10,
                          overrideTabuOnFullExhaust:Int = 9, refresh:Int = 100)
  extends BestNeighborhoodFirst(l, tabuLength, overrideTabuOnFullExhaust, refresh){
  override protected def bestKey(p:Profile):Int = -(p.slopeForCombinators())
}

case class FastestFirst(l:List[Neighborhood],
                        tabuLength:Int = 10,
                        overrideTabuOnFullExhaust:Int = 9,  refresh:Int = 100)
  extends BestNeighborhoodFirst(l, tabuLength, overrideTabuOnFullExhaust, refresh){
  override protected def bestKey(p:Profile):Int = -(p.slopeForCombinators())
}

abstract class BestNeighborhoodFirst(l:List[Neighborhood],
                                     tabuLength:Int,
                                     overrideTabuOnFullExhaust:Int, refresh:Int)
  extends NeighborhoodCombinator(l:_*) {
  require(overrideTabuOnFullExhaust < tabuLength, "overrideTabuOnFullExhaust should be < tabuLength")

  protected var it = 0
  protected def bestKey(p:Profile):Int

  protected val neighborhoodArray: Array[Profile] = l.map(Profile(_)).toArray
  protected val tabu:Array[Int] = Array.fill(neighborhoodArray.length)(0)
  protected var tabuNeighborhoods = new BinomialHeap[Int](tabu(_),tabu.length)

  protected val neighborhoodHeap = new BinomialHeapWithMove[Int]((neighborhoodID:Int) => bestKey(neighborhoodArray(neighborhoodID)), neighborhoodArray.length)
  neighborhoodArray.indices.foreach(neighborhoodHeap.insert(_))

  private def getBestNeighborhooID:Int = neighborhoodHeap.getFirst
  private def updateNeighborhodPerformances(neighborhooID:Int){
    neighborhoodHeap.notifyChange(neighborhooID)
  }
  private def updateTabu(): Unit ={
    it +=1
    while(tabuNeighborhoods.nonEmpty && tabu(tabuNeighborhoods.getFirst) <= it){
      val newNonTabu = tabuNeighborhoods.popFirst()
      neighborhoodArray(newNonTabu).reset()
      neighborhoodHeap.insert(newNonTabu)
    }
  }

  protected def makeTabu(neighborhooID:Int): Unit ={
    neighborhoodHeap.delete(neighborhooID)
    tabu(neighborhooID) = it + tabuLength
    tabuNeighborhoods.insert(neighborhooID)
  }

  /**
   * the method that returns a move from the neighborhood.
   * The returned move should typically be accepted by the acceptance criterion over the objective function.
   * Some neighborhoods are actually jumps, so that they might violate this basic rule however.
   *
   * @param obj the objective function. notice that it is actually a function. if you have an [[oscar.cbls.core.objective.Objective]] there is an implicit conversion available
   * @param acceptanceCriterion
   * @return
   */
  override def getMove(obj: Objective, initialObj:Int, acceptanceCriterion: (Int, Int) => Boolean): SearchResult = {
    if((it > 0) && ((it % refresh) == 0)){

      if(printPerformedSearches){
        println("refreshing knowledge on neighborhood; statistics since last refresh: ")
        printStatus
      }
      for(p <- neighborhoodArray.indices){
        neighborhoodArray(p).resetThisStatistics()
        if(tabu(p) <= it) updateNeighborhodPerformances(p)
      }
    }
    updateTabu()
    while(!neighborhoodHeap.isEmpty){
      val headID = neighborhoodHeap.getFirst
      val headNeighborhood = neighborhoodArray(headID)
      headNeighborhood.getMove(obj,initialObj, acceptanceCriterion) match{
        case NoMoveFound =>
          makeTabu(headID)
        case MoveFound(m) =>
          neighborhoodHeap.notifyChange(headID)
          return MoveFound(m)
      }
    }

    //ok, we try again with tabu, overriding tabu as allowed
    if(tabuNeighborhoods.nonEmpty && tabu(tabuNeighborhoods.getFirst) <= it + overrideTabuOnFullExhaust){
      val newNonTabu = tabuNeighborhoods.popFirst()
      neighborhoodArray(newNonTabu).reset()
      neighborhoodHeap.insert(newNonTabu)
      it -=1
      getMove(obj,initialObj,acceptanceCriterion)
    }else{
      NoMoveFound
    }
  }

  /**
   * prints the profile info for the neighborhoods, for verbosity purposes
   */
  def printStatus(){
    println(Profile.selectedStatisticInfo(neighborhoodArray))
  }
}



abstract class BestNeighborhoodFirstSlidingWindow(l:List[Neighborhood], windowsSize:Int,
                                                  tabuLength:Int,
                                                  overrideTabuOnFullExhaust:Int, refresh:Int)
  extends NeighborhoodCombinator(l:_*) {
  require(overrideTabuOnFullExhaust < tabuLength, "overrideTabuOnFullExhaust should be < tabuLength")

  protected var it = 0
  protected def bestKey(p:Profile):Int

  protected val neighborhoodArray: Array[Profile] = l.map(Profile(_)).toArray
  protected val tabu:Array[Int] = Array.fill(neighborhoodArray.length)(0)
  protected var tabuNeighborhoods = new BinomialHeap[Int](tabu(_),tabu.length)

  protected val neighborhoodHeap = new BinomialHeapWithMove[Int]((neighborhoodID:Int) => bestKey(neighborhoodArray(neighborhoodID)), neighborhoodArray.length)
  neighborhoodArray.indices.foreach(neighborhoodHeap.insert(_))

  private def getBestNeighborhooID:Int = neighborhoodHeap.getFirst
  private def updateNeighborhodPerformances(neighborhooID:Int){
    neighborhoodHeap.notifyChange(neighborhooID)
  }
  private def updateTabu(): Unit ={
    it +=1
    while(tabuNeighborhoods.nonEmpty && tabu(tabuNeighborhoods.getFirst) <= it){
      val newNonTabu = tabuNeighborhoods.popFirst()
      neighborhoodArray(newNonTabu).reset()
      neighborhoodHeap.insert(newNonTabu)
    }
  }

  protected def makeTabu(neighborhooID:Int): Unit ={
    neighborhoodHeap.delete(neighborhooID)
    tabu(neighborhooID) = it + tabuLength
    tabuNeighborhoods.insert(neighborhooID)
  }

  /**
   * the method that returns a move from the neighborhood.
   * The returned move should typically be accepted by the acceptance criterion over the objective function.
   * Some neighborhoods are actually jumps, so that they might violate this basic rule however.
   *
   * @param obj the objective function. notice that it is actually a function. if you have an [[oscar.cbls.core.objective.Objective]] there is an implicit conversion available
   * @param acceptanceCriterion
   * @return
   */
  override def getMove(obj: Objective, initialObj:Int, acceptanceCriterion: (Int, Int) => Boolean): SearchResult = {
    if((it > 0) && ((it % refresh) == 0)){

      if(printPerformedSearches){
        println("refreshing knowledge on neighborhood; statistics since last refresh: ")
        printStatus
      }
      for(p <- neighborhoodArray.indices){
        neighborhoodArray(p).resetThisStatistics()
        if(tabu(p) <= it) updateNeighborhodPerformances(p)
      }
    }
    updateTabu()
    while(!neighborhoodHeap.isEmpty){
      val headID = neighborhoodHeap.getFirst
      val headNeighborhood = neighborhoodArray(headID)
      headNeighborhood.getMove(obj,initialObj, acceptanceCriterion) match{
        case NoMoveFound =>
          makeTabu(headID)
        case MoveFound(m) =>
          neighborhoodHeap.notifyChange(headID)
          return MoveFound(m)
      }
    }

    //ok, we try again with tabu, overriding tabu as allowed
    if(tabuNeighborhoods.nonEmpty && tabu(tabuNeighborhoods.getFirst) <= it + overrideTabuOnFullExhaust){
      val newNonTabu = tabuNeighborhoods.popFirst()
      neighborhoodArray(newNonTabu).reset()
      neighborhoodHeap.insert(newNonTabu)
      it -=1
      getMove(obj,initialObj:Int, acceptanceCriterion)
    }else{
      NoMoveFound
    }
  }

  /**
   * prints the profile info for the neighborhoods, for verbosity purposes
   */
  def printStatus(){
    println(Profile.selectedStatisticInfo(neighborhoodArray))
  }
}


object RoundRobin {
  implicit def NeighborhoodToRoundRobin(n: Neighborhood): RoundRobin = new RoundRobin(List(n), 1)
}

/**
 * makes a round robin on the neighborhood. it swaps as soon as one does not find a move
 * and swaps neighborhood after "step" invocations
 *
 * @author renaud.delandtsheer@cetic.be
 */
class RoundRobin(l: List[Neighborhood], steps: Int = 1) extends NeighborhoodCombinator(l: _*) {
  val robins = l.length
  var remainingSteps: Int = steps
  var tail: List[Neighborhood] = l
  override def getMove(obj: Objective, initialObj:Int, acceptanceCriteria: (Int, Int) => Boolean): SearchResult =
    myGetImprovingMove(obj, initialObj, acceptanceCriteria)

  private def myGetImprovingMove(obj: Objective, initialObj:Int, acceptanceCriteria: (Int, Int) => Boolean, triedRobins: Int = 0): SearchResult = {
    if (triedRobins >= robins) {
      NoMoveFound
    } else if (remainingSteps > 0) {
      //no need to change neighborhood yet
      remainingSteps -= 1
      tail.head.getMove(obj, initialObj:Int, acceptanceCriteria) match {
        case NoMoveFound =>
          tail.head.reset()
          moveToNextRobin()
          myGetImprovingMove(obj, initialObj, acceptanceCriteria, triedRobins + 1)
        case x: MoveFound => x
      }
    } else {
      //move to next robin
      moveToNextRobin()
      myGetImprovingMove(obj, initialObj, acceptanceCriteria, triedRobins)
    }
  }

  private def moveToNextRobin() {
    if (tail.tail.isEmpty) {
      tail = l
    } else {
      tail = tail.tail
    }
    remainingSteps = steps
  }

  //this resets the internal state of the move combinators
  override def reset() {
    remainingSteps = steps
    tail = l
    super.reset()
  }

  /**
   * proposes a round-robin with that.
   * notice that you can chain steps; this will build a round-robin on the whole sequence (although this operation is not associative)
   *
   * @param b
   * @return
   */
  override def step(b: Neighborhood): RoundRobin = new RoundRobin(l ::: List(b))

  def repeat(i: Int): RoundRobin = {
    val last = l.last
    new RoundRobin(l ::: List.fill(i - 1)(last))
  }
}

class RoundRobinNoParam(val a: Neighborhood, val b: Neighborhood) {
  def step(s: Int): Neighborhood = new RoundRobin(List(a, b), s)
}

object RoundRobinNoParam {
  implicit def toNeighBorHood(rr: RoundRobinNoParam): Neighborhood = {
    val toReturn = new RoundRobin(List(rr.a, rr.b), 1)
    toReturn.verbose = rr.a.verbose
    toReturn
  }
}


/**
 * This combinators queries a once every n time it is queried.
 * the other times, it returns NoMovesFound.
 * if n finds no moves, depending on retryOnNoMoveFound,
 * it will either keep on querying n until a move is found, or continue its sequence of one out of n
 *
 * @param a the initial neighborhood
 * @param n the size of the sequence
 * @param retryOnNoMoveFound if true, keeps on querying a on NoMoveFound, otherwise, continues the sequence
 */
class OnceEvery(a: Neighborhood, n: Int, retryOnNoMoveFound: Boolean = false) extends NeighborhoodCombinator(a) {
  var remainingMoves = n - 1

  override def getMove(obj: Objective, initialObj:Int, acceptanceCriteria: (Int, Int) => Boolean): SearchResult = {
    if (remainingMoves > 0) {
      remainingMoves -= 1
      NoMoveFound
    } else {
      a.getMove(obj, initialObj:Int, acceptanceCriteria) match {
        case NoMoveFound =>
          if (!retryOnNoMoveFound) remainingMoves = n - 1
          NoMoveFound
        case x =>
          remainingMoves = n - 1
          x
      }
    }
  }

  //this resets the internal state of the move combinators
  override def reset() {
    remainingMoves = n - 1
    super.reset()
  }
}


/**
 * this combinator randomly tries one neighborhood.
 * it tries the other if the first did not find any move
 *
 * @param a a neighborhood
 * @author renaud.delandtsheer@cetic.be
 */
class Random(a: Neighborhood*) extends NeighborhoodCombinator(a:_*) {
  private val r = new scala.util.Random()

  override def getMove(obj: Objective, initialObj:Int, acceptanceCriteria: (Int, Int) => Boolean): SearchResult = {
    val neighborhoods = r.shuffle(a).toIterator
    while (neighborhoods.hasNext) {
      val current = neighborhoods.next
      current.getMove(obj, initialObj, acceptanceCriteria) match {
        case m: MoveFound => return m
        case _ => ;
      }
    }
    NoMoveFound
  }
}

/**
 * this combinator randomly tries one neighborhood.
 * it tries the other if the first did not find any move
 *
 * @param a a neighborhood
 * @author renaud.delandtsheer@cetic.be
 */
class BiasedRandom(a: (Neighborhood,Double)*)(noRetryOnExhaust:Boolean = false) extends NeighborhoodCombinator(a.map(_._1):_*) {
  require(a.nonEmpty)

  abstract sealed class Node(val weight:Double)
  case class MiddleNode(l:Node,r:Node) extends Node(l.weight + r.weight)
  case class TerminalNode(override val weight:Double, val n:Neighborhood) extends Node(weight)

  private val r = new scala.util.Random()

  def reduce(l:List[Node]):List[Node] = {
    l match{
      case h1 :: h2 :: t => MiddleNode(h1,h2) :: reduce(t)
      case List(h) => l
      case nil => nil
    }
  }

  def fixPoint[A](init:A, function:A => A, fixpointReached:A => Boolean):A={
    var current = init
    while(!fixpointReached(current)){
      current = function(current)
    }
    current
  }

  val initialNeighborhoodTree:Node =  fixPoint(
    a.toList.map(nw => new TerminalNode(nw._2,nw._1)),
    reduce,
    (_:List[Node]) match{case List(_) => true; case _ => false}
  ).head

  def findAndRemove(n:Node,p:Double):(Option[Node],Neighborhood) = {
    n match{
      case TerminalNode(w,n) => (None,n)
      case MiddleNode(l,r) =>
        val ((newNode,found),other) = if (p <= l.weight) (findAndRemove(l,p),r) else (findAndRemove(r,p-l.weight),l)
        newNode match{
          case None => (Some(other),found)
          case Some(p) => (Some(MiddleNode(p,other)),found)
        }
    }
  }

  var neighborhoodWithExhaustedRemoved:Option[Node] = Some(initialNeighborhoodTree)

  override def getMove(obj: Objective, initialObj:Int, acceptanceCriteria: (Int, Int) => Boolean): SearchResult = {
    var remainingNeighborhoods:Option[Node] = neighborhoodWithExhaustedRemoved
    while (true) {
      remainingNeighborhoods match{
        case None => return NoMoveFound
        case Some(node) =>
          val (newHead,selectedNeighborhood) = findAndRemove(node,r.nextFloat()*node.weight)
          remainingNeighborhoods = newHead
          selectedNeighborhood.getMove(obj, initialObj, acceptanceCriteria) match {
            case m: MoveFound => return m
            case _ =>
              if(noRetryOnExhaust) neighborhoodWithExhaustedRemoved = newHead
          }
      }
    }
    NoMoveFound
  }
}

/**
 * @param l
 * @param weightUpdate a function that updates the weight of a neighborhood. if the function returns a negative number, the neighborhood gets the average weight thatthe other received.
 * @param updateEveryXCalls
 */
class LearningRandom(l:List[Neighborhood],
                     weightUpdate:(Profile,Double) => Double =
                     (stat,oldWeight) => {if (stat.nbCalls == 0) -1 else {
                       val toReturn =  (stat.slopeOrZero + oldWeight)/2
                       stat.resetStatistics
                       toReturn}},
                     updateEveryXCalls:Int = 10)
  extends NeighborhoodCombinator(l:_*){

  val instrumentedNeighborhood:List[Profile] = l.map(Profile(_))
  var weightedInstrumentedNeighborhoods:List[(Profile,Double)] = instrumentedNeighborhood.map((_,1.0))
  var currentRandom = new BiasedRandom(weightedInstrumentedNeighborhoods:_*)()
  var stepsBeforeUpdate = updateEveryXCalls

  override def getMove(obj: Objective, initialObj:Int, acceptanceCriterion: (Int, Int) => Boolean): SearchResult = {
    if(stepsBeforeUpdate <= 0){
      val newlyWeightedNeighborhoods = weightedInstrumentedNeighborhoods.map((sd => (sd._1,weightUpdate(sd._1,sd._2))))
      val (totalWeightNonNegative,nonNegativeCount) = newlyWeightedNeighborhoods.foldLeft((0.0,0))((a:(Double,Int),b:(Profile,Double)) => (if(b._2 < 0) a else (a._1 + b._2,a._2+1)))
      val defaultWeight = totalWeightNonNegative / nonNegativeCount
      weightedInstrumentedNeighborhoods = newlyWeightedNeighborhoods.map(sw => (sw._1,(if (sw._2 < 0) defaultWeight else sw._2)))
      currentRandom = new BiasedRandom(weightedInstrumentedNeighborhoods :_*)()
      stepsBeforeUpdate = updateEveryXCalls
      if(printPerformedSearches) println("LearningRandom: weights updated: " + weightedInstrumentedNeighborhoods )
    }
    stepsBeforeUpdate -=1
    currentRandom.getMove(obj,initialObj,acceptanceCriterion)
  }
}


/**
 * this combinator is stateful.
 * it returns the result of the first Neighborhood until it returns NoMoveFound.
 * It then switches to the other Neighborhood,
 * but only if a move was found by the first neighborhood
 * it does not come back to the first one after the second one is exhausted
 *
 * @author renaud.delandtsheer@cetic.be
 */
class ExhaustAndContinueIfMovesFound(a: Neighborhood, b: Neighborhood) extends NeighborhoodCombinator(a, b) {
  var currentIsA = true
  var movesFoundWithCurrent = false
  override def getMove(obj: Objective, initialObj:Int, acceptanceCriteria: (Int, Int) => Boolean): SearchResult = {
    def search(): SearchResult = {
      val current = if (currentIsA) a else b
      current.getMove(obj, initialObj:Int, acceptanceCriteria) match {
        case NoMoveFound =>
          if (currentIsA) {
            currentIsA = false
            movesFoundWithCurrent = false
            search()
          } else NoMoveFound
        case x: MoveFound =>
          movesFoundWithCurrent = true
          x
      }
    }
    search()
  }

  //this resets the internal state of the move combinators
  override def reset() {
    currentIsA = true
    movesFoundWithCurrent = false
    super.reset()
  }
}

