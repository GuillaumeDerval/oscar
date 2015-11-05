/**
 * *****************************************************************************
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
 * ****************************************************************************
 */
package oscar.cbls.search.combinators

import oscar.cbls.invariants.core.computation.{IntValue, SetValue}
import oscar.cbls.objective.{CascadingObjective, Objective}
import oscar.cbls.search.core.{NoMoveFound, _}
import oscar.cbls.search.move.{CallBackMove, CompositeMove, InstrumentedMove, Move}

import scala.language.implicitConversions

//TODO: les combinateurs devraient avoir une liste de voisinnages (ou neighborhood*), pas juste un seul.
//TODO: proposer du benchmarking des voisinages (nombre de moves trouvés, gain moyen sur une fct objectif, temps de recherche, nombre de recherche effectuées, ...)

/**
 * @author renaud.delandtsheer@cetic.be
 */
abstract class NeighborhoodCombinator(a: Neighborhood*) extends Neighborhood {
  //this resets the internal state of the move combinators
  override def reset() {
    for (n <- a) n.reset()
  }

  override def verbose_=(i: Int): Unit = {
    for (n <- a) n.verbose = i
    super.verbose_=(i)
  }

  override def toString: String = this.getClass.getSimpleName + "(" + a.mkString(",") + ")"

  override def collectStatistics: String = a.map(_.collectStatistics).mkString("\n")
}

class BasicSaveBest(a: Neighborhood, o: Objective) extends NeighborhoodCombinator(a) {

  protected val s = o.model

  require(s != null, "you are using an objective function that has no attached model, so "
    + this.getClass.getSimpleName + " cannot save the model; pass it explicitely to the Objective creation to solve this issue")

  protected var bestObj = if (currentSolutionIsAcceptable) o.value else Int.MaxValue
  protected var best = if (currentSolutionIsAcceptable) s.solution() else null

  //this resets the internal state of the move combinators
  override def reset() {
    super.reset()
    bestObj = Int.MaxValue
    best = null
  }

  override def getMove(obj: Objective, acceptanceCriteria: (Int, Int) => Boolean): SearchResult = {

    //we record the obj before move to prevent an additional useless propagation
    val objBeforeMove = o.value

    a.getMove(obj, acceptanceCriteria) match {
      case NoMoveFound => NoMoveFound
      case MoveFound(m) =>
        if (m.objAfter > objBeforeMove && objBeforeMove < bestObj && currentSolutionIsAcceptable) {
          //solution degrades, and we were better than the best recorded
          //so we save
          best = s.solution(true)
          bestObj = objBeforeMove
          if (verbose >= 2) println("saving best solution before degradation (obj:" + bestObj + ")")
        }
        MoveFound(m)
    }
  }

  protected def currentSolutionIsAcceptable = true

  def restoreBest() {
    val isCurrentAccepteable = currentSolutionIsAcceptable
    if (best == null && !isCurrentAccepteable) {
      if (verbose >= 1) println("no single acceptable solution seen")
    } else if (o.value > bestObj || !isCurrentAccepteable) {
      s.restoreSolution(best)
      if (verbose >= 1) println("restoring best solution (obj:" + bestObj + ")")
    } else if (verbose >= 1) println("no better solution to restore")
  }

  /**
   * same as doAllImprovingMoves and calling restoreBest after.
   * @param shouldStop a function that takes the iteration number and returns true if search should be stopped
   *                   eg if the problem is considered as solved
   *                   you can evaluate some objective function there such as a violation degree
   *                   notice that although you can use it to stop your algorithm, the primary purpose is to avoid blow-up.
   *                   Smarter stop criterion cen be made using combinators, and this stop should be considered only as a protection againt blow up.
   * @param acceptanceCriterion a criterion for accepting a move
   *                            by default, we only accept strictly improving moves
   * @return the number of moves performed
   */
  def doAllMovesAndRestoreBest(shouldStop: Int => Boolean = _ => false, obj: Objective, acceptanceCriterion: (Int, Int) => Boolean = (oldObj, newObj) => oldObj > newObj): Int = {
    val toReturn = doAllMoves(shouldStop, obj, acceptanceCriterion)
    restoreBest()
    toReturn
  }

  def restoreBestOnExhaust: RestoreBestOnExhaust = new RestoreBestOnExhaust(this)
}

class SaveBest(a: Neighborhood, o: Objective) extends BasicSaveBest(a: Neighborhood, o: Objective) {

  def whenEmpty(violation: SetValue) = new SaveBestWhen(a, o, () => violation.value.isEmpty)
  def whenZero(violation: IntValue) = new SaveBestWhen(a, o, () => violation.value == 0)

  /**
   * this method restricts the save operation to only the situation where "shouldSave" returns true
   * notice that this is an override of the "when" method found in neighborhood.
   * @param shouldSave
   * @return
   */
  override def when(shouldSave: () => Boolean) = new SaveBestWhen(a, o, shouldSave)
}

class SaveBestWhen(a: Neighborhood, o: Objective, shouldSave: () => Boolean) extends BasicSaveBest(a, o) {
  override protected def currentSolutionIsAcceptable: Boolean = shouldSave()
}

class RestoreBestOnExhaust(a: BasicSaveBest) extends NeighborhoodCombinator(a) {

  def restoreBest(): Unit = {
    a.restoreBest()
  }

  /**
   * same as doAllImprovingMoves and calling restoreBest after.
   * @param shouldStop a function that takes the iteration number and returns true if search should be stopped
   *                   eg if the problem is considered as solved
   *                   you can evaluate some objective function there such as a violation degree
   * @param acceptanceCriterion a criterion for accepting a move
   *                            by default, we only accept strictly improving moves
   * @return the number of moves performed
   */
  def doAllMovesAndRestoreBest(shouldStop: Int => Boolean, obj: Objective, acceptanceCriterion: (Int, Int) => Boolean = (oldObj, newObj) => oldObj > newObj): Int = {
    a.doAllMovesAndRestoreBest(shouldStop, obj, acceptanceCriterion)
  }

  override def getMove(obj: Objective, acceptanceCriteria: (Int, Int) => Boolean): SearchResult = {
    a.getMove(obj, acceptanceCriteria) match {
      case m: MoveFound => m
      case x =>
        restoreBest()
        x
    }
  }
}

/**
 * this combinator attaches a custom code to a given neighborhood.
 * the code is called whenever a move is asked to the neighborhood.
 * @param a a neighborhood
 * @param proc the procedure to execute before the neighborhood is queried
 */
case class DoOnQuery(a: Neighborhood, proc: () => Unit) extends NeighborhoodCombinator(a) {
  override def getMove(obj: Objective, acceptanceCriteria: (Int, Int) => Boolean): SearchResult = {
    proc()
    a.getMove(obj, acceptanceCriteria)
  }
}

/**
 * this combinator attaches a custom code to a given neighborhood.
 * the code is called whenever a move from this neighborhood is taken
 * The callBack is performed before the move is actually taken.
 * @param a a neighborhood
 * @param procBeforeMove the procedure to execute when the move is taken, , with the move as input parameter
 *                   use this to update a Tabu for instance
 * @param procAfterMove a procedure to execute after the move is taken, with the move as input parameter
 */
case class DoOnMove(a: Neighborhood,
                    procBeforeMove: Move => Unit = null,
                    procAfterMove: Move => Unit = null) extends NeighborhoodCombinator(a) {
  override def getMove(obj: Objective, acceptanceCriteria: (Int, Int) => Boolean): SearchResult = {
    a.getMove(obj, acceptanceCriteria) match {
      case m: MoveFound =>
        InstrumentedMove(m.m, callBackBeforeMove(m.m), callBackAfterMove(m.m))
      case x => x
    }
  }

  def callBackBeforeMove(m: Move)() {
    if (procBeforeMove != null) procBeforeMove(m)
  }

  def callBackAfterMove(m: Move)() {
    if (procAfterMove != null) procAfterMove(m)
  }
}

/**
 * this combinator attaches a custom code to a given neighborhood.
 * the code is called whenever a move from this neighborhood is taken for the first time.
 * notice that this neighborhood is reset, so first time can occur several times.
 * @param a a neighborhood
 * @param proc the procedure to call on one first move that is performed from this neighborhood
 */
class DoOnFirstMove(a: Neighborhood, proc: () => Unit) extends NeighborhoodCombinator(a) {
  var isFirstMove = true
  override def getMove(obj: Objective, acceptanceCriteria: (Int, Int) => Boolean): SearchResult = {
    if (isFirstMove) {
      a.getMove(obj, acceptanceCriteria) match {
        case m: MoveFound => InstrumentedMove(m.m, notifyMoveTaken)
        case x => x
      }
    } else {
      a.getMove(obj, acceptanceCriteria)
    }
  }

  //this resets the internal state of the move combinators
  override def reset() {
    isFirstMove = true
    super.reset()
  }

  private def notifyMoveTaken() {
    proc()
    isFirstMove = false
  }
}

/**
 * this combinator randomly tries one neighborhood.
 * it tries the other if the first did not find any move
 * @param a a neighborhood
 * @author renaud.delandtsheer@cetic.be
 */
class Random(a: Neighborhood*) extends NeighborhoodCombinator(a:_*) {
  private val r = new scala.util.Random()

  override def getMove(obj: Objective, acceptanceCriteria: (Int, Int) => Boolean): SearchResult = {
    val neighborhoods = r.shuffle(a).toIterator
    while (neighborhoods.hasNext) {
      val current = neighborhoods.next
      current.getMove(obj, acceptanceCriteria) match {
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

  override def getMove(obj: Objective, acceptanceCriteria: (Int, Int) => Boolean): SearchResult = {
    var remainingNeighborhoods:Option[Node] = neighborhoodWithExhaustedRemoved
    while (true) {
      remainingNeighborhoods match{
        case None => return NoMoveFound
        case Some(node) =>
          val (newHead,selectedNeighborhood) = findAndRemove(node,r.nextFloat()*node.weight)
          remainingNeighborhoods = newHead
          selectedNeighborhood.getMove(obj, acceptanceCriteria) match {
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
class LearningRandom(l:List[Neighborhood], weightUpdate:(Statistics,Double) => Double =
(stat,oldWeight) => {if (stat.nbCalls == 0) -1 else {
  val toReturn =  (stat.slopeOrZero + oldWeight)/2
  stat.resetStatistics
  toReturn}}, updateEveryXCalls:Int = 10)
  extends NeighborhoodCombinator(l:_*){

  val instrumentedNeighborhood:List[Statistics] = l.map(Statistics(_))
  var weightedInstrumentedNeighborhoods:List[(Statistics,Double)] = instrumentedNeighborhood.map((_,1.0))
  var currentRandom = new BiasedRandom(weightedInstrumentedNeighborhoods:_*)()
  var stepsBeforeUpdate = updateEveryXCalls

  override def getMove(obj: Objective, acceptanceCriterion: (Int, Int) => Boolean): SearchResult = {
    if(stepsBeforeUpdate <= 0){
      val newlyWeightedNeighborhoods = weightedInstrumentedNeighborhoods.map((sd => (sd._1,weightUpdate(sd._1,sd._2))))
      val (totalWeightNonNegative,nonNegativeCount) = newlyWeightedNeighborhoods.foldLeft((0.0,0))((a:(Double,Int),b:(Statistics,Double)) => (if(b._2 <0) a else (a._1 + b._2,a._2+1)))
      val defaultWeight = totalWeightNonNegative / nonNegativeCount
      weightedInstrumentedNeighborhoods = newlyWeightedNeighborhoods.map(sw => (sw._1,(if (sw._2 < 0) defaultWeight else sw._2)))
      currentRandom = new BiasedRandom(weightedInstrumentedNeighborhoods :_*)()
      stepsBeforeUpdate = updateEveryXCalls
      if(amIVerbose) println("learning done:" + weightedInstrumentedNeighborhoods )
    }
    stepsBeforeUpdate -=1
    currentRandom.getMove(obj,acceptanceCriterion)
  }

  //this resets the internal state of the move combinators
  override def reset(): Unit ={
    super.reset()
  }
}

/**
 * this combinator sequentially tries all neighborhoods until one move is found
 * between calls, it will roll back to the first neighborhood
 * it tries a first, and if no move it found, tries b
 * a is reset if it did not find anything.
 * @param a a neighborhood
 * @param b another neighborhood
 * @author renaud.delandtsheer@cetic.be
 */
class OrElse(a: Neighborhood, b: Neighborhood) extends NeighborhoodCombinator(a, b) {
  override def getMove(obj: Objective, acceptanceCriteria: (Int, Int) => Boolean): SearchResult = {
    a.getMove(obj, acceptanceCriteria) match {
      case NoMoveFound =>
        a.reset()
        b.getMove(obj, acceptanceCriteria)
      case x => x
    }
  }
}

/**
 * this combinator always selects the best move between the two parameters
 * notice that this combinator makes more sense
 * if the two neighborhood return their best found move,
 * and not their first found move, as usually done.
 * @author renaud.delandtsheer@cetic.be
 */
class Best(a: Neighborhood, b: Neighborhood) extends NeighborhoodCombinator(a, b) {

  override def getMove(obj: Objective, acceptanceCriteria: (Int, Int) => Boolean): SearchResult = {
    (a.getMove(obj, acceptanceCriteria), b.getMove(obj, acceptanceCriteria)) match {
      case (NoMoveFound, x) => x
      case (x, NoMoveFound) => x
      case (x: MoveFound, y: MoveFound) => if (x.objAfter < y.objAfter) x else y

    }
  }
}

/**
 * this combinator is stateful.
 * it returns the result of the first Neighborhood until it returns NoMoveFound.
 * It then switches to the other Neighborhood.
 * it does not come back to the first one after the second one is exhausted
 * @author renaud.delandtsheer@cetic.be
 */
class Exhaust(a: Neighborhood, b: Neighborhood) extends NeighborhoodCombinator(a, b) {
  var currentIsA = true
  override def getMove(obj: Objective, acceptanceCriteria: (Int, Int) => Boolean): SearchResult = {
    def search(): SearchResult = {
      val current = if (currentIsA) a else b
      current.getMove(obj, acceptanceCriteria) match {
        case NoMoveFound => if (currentIsA) { currentIsA = false; search() } else NoMoveFound
        case x: MoveFound => x
      }
    }
    search()
  }

  //this resets the internal state of the move combinators
  override def reset() {
    currentIsA = true
    super.reset()
  }
}

/**
 * once given condition has turned true,
 * retries n times the move before concluding to noMove can be found
 * resets on the first found move, or on reset
 * @param a the neighborhood on which we will perform retries
 * @param cond condition that takes the number of consecutive NoMoveFound, and says if we should try again returns true if yes, false otherwise
 * @author renaud.delandtsheer@cetic.be
 * @author yoann.guyot@cetic.be
 */
class Retry(a: Neighborhood, cond: Int => Boolean) extends NeighborhoodCombinator(a) {
  var consecutiveFails = 0
  override def getMove(obj: Objective, acceptanceCriteria: (Int, Int) => Boolean): SearchResult = {
    a.getMove(obj, acceptanceCriteria) match {
      case NoMoveFound =>
        consecutiveFails = consecutiveFails + 1
        if (cond(consecutiveFails)) {
          a.reset()
          getMove(obj, acceptanceCriteria)
        } else {
          NoMoveFound
        }
      case x =>
        consecutiveFails = 0
        x
    }
  }

  //this resets the internal state of the move combinators
  override def reset() {
    super.reset()
    consecutiveFails = 0
  }
}

case class NoReset(a: Neighborhood) extends NeighborhoodCombinator(a) {
  override def getMove(obj: Objective, acceptanceCriteria: (Int, Int) => Boolean) = a.getMove(obj, acceptanceCriteria)

  //this resets the internal state of the move combinators
  override def reset() {}
}
/**
 * this combinator is stateful.
 * it returns the result of one Neighborhood until it returns NoMoveFound.
 * It then switches to the other Neighborhood.
 * it starts with Neighborhood a
 * @author renaud.delandtsheer@cetic.be
 */
class ExhaustBack(a: Neighborhood, b: Neighborhood) extends NeighborhoodCombinator(a, b) {
  var currentIsA = true
  override def getMove(obj: Objective, acceptanceCriteria: (Int, Int) => Boolean): SearchResult = {
    def search(): SearchResult = {
      val current = if (currentIsA) a else b
      current.getMove(obj, acceptanceCriteria) match {
        case NoMoveFound =>
          if (currentIsA) {
            currentIsA = false
            b.reset()
            b.getMove(obj, acceptanceCriteria)
          } else {
            currentIsA = true
            a.reset()
            a.getMove(obj, acceptanceCriteria)
          }
        case x: MoveFound => x
      }
    }
    search()
  }

  //this resets the internal state of the move combinators
  override def reset() {
    currentIsA = true
    super.reset()
  }
}

/**
 * @author renaud.delandtsheer@cetic.be
 */
class ResetOnExhausted(a: Neighborhood) extends NeighborhoodCombinator(a) {
  override def getMove(obj: Objective, acceptanceCriteria: (Int, Int) => Boolean): SearchResult = {
    a.getMove(obj, acceptanceCriteria) match {
      case NoMoveFound =>
        a.reset()
        a.getMove(obj, acceptanceCriteria)
      case m: MoveFound => m
    }
  }
}

/**
 * this combinator is stateful.
 * it returns the result of the first Neighborhood until it returns NoMoveFound.
 * It then switches to the other Neighborhood,
 * but only if a move was found by the first neighborhood
 * it does not come back to the first one after the second one is exhausted
 * @author renaud.delandtsheer@cetic.be
 */
class ExhaustAndContinueIfMovesFound(a: Neighborhood, b: Neighborhood) extends NeighborhoodCombinator(a, b) {
  var currentIsA = true
  var movesFoundWithCurrent = false
  override def getMove(obj: Objective, acceptanceCriteria: (Int, Int) => Boolean): SearchResult = {
    def search(): SearchResult = {
      val current = if (currentIsA) a else b
      current.getMove(obj, acceptanceCriteria) match {
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

/**
 * this combinator is stateless, it checks the condition on every invocation. If the condition is false,
 * it does not try the Neighborhood and finds no move.
 * @author renaud.delandtsheer@cetic.be
 */
class Conditional(c: () => Boolean, b: Neighborhood) extends NeighborhoodCombinator(b) {
  override def getMove(obj: Objective, acceptanceCriteria: (Int, Int) => Boolean): SearchResult = {
    if (c()) b.getMove(obj, acceptanceCriteria)
    else NoMoveFound
  }
}

/**
 * this combinator bounds the number of time the search is actually performed
 * @author renaud.delandtsheer@cetic.be
 */
class BoundSearches(a: Neighborhood, val maxMove: Int) extends NeighborhoodCombinator(a) {
  var remainingMoves = maxMove
  override def getMove(obj: Objective, acceptanceCriteria: (Int, Int) => Boolean): SearchResult = {
    if (remainingMoves > 0) {
      remainingMoves -= 1
      a.getMove(obj, acceptanceCriteria)
    } else NoMoveFound
  }

  //this resets the internal state of the move combinators
  override def reset() {
    remainingMoves = maxMove
    super.reset()
  }
}

/**
 * This combinators queries a once every n time it is queried.
 * the other times, it returns NoMovesFound.
 * if n finds no moves, depending on retryOnNoMoveFound,
 * it will either keep on querying n until a move is found, or continue its sequence of one out of n
 * @param a the initial neighborhood
 * @param n the size of the sequence
 * @param retryOnNoMoveFound if true, keeps on querying a on NoMoveFound, otherwise, continues the sequence
 */
class OnceEvery(a: Neighborhood, n: Int, retryOnNoMoveFound: Boolean = false) extends NeighborhoodCombinator(a) {
  var remainingMoves = n - 1

  override def getMove(obj: Objective, acceptanceCriteria: (Int, Int) => Boolean): SearchResult = {
    if (remainingMoves > 0) {
      remainingMoves -= 1
      NoMoveFound
    } else {
      a.getMove(obj, acceptanceCriteria) match {
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
 * this combinator bounds the number of moves done with this neighborhood
 * notice that the count is reset by the reset operation
 * @author renaud.delandtsheer@cetic.be
 */
class MaxMoves(a: Neighborhood, val maxMove: Int, cond: Move => Boolean = null) extends NeighborhoodCombinator(a) {
  var remainingMoves = maxMove
  override def getMove(obj: Objective, acceptanceCriteria: (Int, Int) => Boolean): SearchResult = {
    if (remainingMoves > 0) {
      a.getMove(obj, acceptanceCriteria) match {
        case m: MoveFound => InstrumentedMove(m.m, () => notifyMoveTaken(m.m))
        case x => x
      }
    } else {
      if (verbose >= 1)
        println("MaxMoves: reached " + (if (maxMove == 1) "1 move " else maxMove + " moves"))
      NoMoveFound
    }
  }

  //this resets the internal state of the move combinators
  override def reset() {
    remainingMoves = maxMove
    super.reset()
  }

  def notifyMoveTaken(m: Move) {
    if (cond == null || cond(m)) remainingMoves -= 1
  }

  /**
   * this will modify the effect of the maxMoves by transforming it into a [[MaxMovesWithoutImprovement]]
   * the initial maxMoves is deleted by this method, and the integer bound is passed to [[MaxMovesWithoutImprovement]]
   */
  def withoutImprovementOver(obj: () => Int) = new MaxMovesWithoutImprovement(a, cond, maxMove, obj)

  def suchThat(cond: Move => Boolean) = new MaxMoves(a, maxMove, if (this.cond == null) cond else (m: Move) => this.cond(m) && cond(m))
}

/**
 * This combinator finds no move starting from the point where cond evaluates to false,
 * otherwise, it forwards the search request to "a"
 * this combinator is reset on reset
 * @param a a neighborhood
 * @param cond a stop criterion
 * @author renaud.delandtsheer@cetic.be
 */
class StopWhen(a: Neighborhood, cond: () => Boolean) extends NeighborhoodCombinator(a) {
  var isStopped: Boolean = false
  override def getMove(obj: Objective, acceptanceCriterion: (Int, Int) => Boolean): SearchResult = {
    if (isStopped || cond()) { isStopped = true; NoMoveFound }
    else a.getMove(obj, acceptanceCriterion)
  }

  //this resets the internal state of the move combinators
  override def reset() {
    isStopped = false
    super.reset()
  }
}

object RoundRobin {
  implicit def NeighborhoodToRoundRobin(n: Neighborhood): RoundRobin = new RoundRobin(List(n), 1)
}

/**
 * makes a round robin on the neighborhood. it swaps as soon as one does not find a move
 * and swaps neighborhood after "step" invocations
 * @author renaud.delandtsheer@cetic.be
 */
class RoundRobin(l: List[Neighborhood], steps: Int = 1) extends NeighborhoodCombinator(l: _*) {
  val robins = l.length
  var remainingSteps: Int = steps
  var tail: List[Neighborhood] = l
  override def getMove(obj: Objective, acceptanceCriteria: (Int, Int) => Boolean): SearchResult =
    myGetImprovingMove(obj, acceptanceCriteria)

  private def myGetImprovingMove(obj: Objective, acceptanceCriteria: (Int, Int) => Boolean, triedRobins: Int = 0): SearchResult = {
    if (triedRobins >= robins) {
      NoMoveFound
    } else if (remainingSteps > 0) {
      //no need to change neighborhood yet
      remainingSteps -= 1
      tail.head.getMove(obj, acceptanceCriteria) match {
        case NoMoveFound =>
          moveToNextRobin()
          myGetImprovingMove(obj, acceptanceCriteria, triedRobins + 1)
        case x: MoveFound => x
      }
    } else {
      //move to next robin
      remainingSteps = steps
      moveToNextRobin()
      myGetImprovingMove(obj, acceptanceCriteria, triedRobins + 1)
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
    super.reset()
  }

  /**
   * proposes a round-robin with that.
   * notice that you can chain steps; this will build a round-robin on the whole sequence (although this operation is not associative)
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
 * to build a composite neighborhood.
 * the first neighborhood is used only to provide a round robin exploration on its possible moves
 * you must ensure that this first neighborhood will perform a hotRestart, so that it will enumerate all its moves
 * internally, this neighborhood will be called with a fully acceptant acceptanceCriteria,
 *
 * the move combinator for every move provided by the first neighborhood, the combinator calls the second one
 * and we consider the composition of the two moves for the acceptance criteria.
 * the returned move is the composition of the two found moves
 *
 * you must also ensure that the two neighborhood evaluate the same objective function,
 * since this combinator needs to evaluate the whole composite move, and not only the last part of the composition
 *
 * A native composite neighborhood will probably be much faster than this combinator, so use this for prototyping
 * for instance, this combinator does not allow for some form of symmetry breaking, unless you are really doing it the hard way.
 *
 * this move will reset the first neighborhood on every call, since it is probably bounded by the number of moves it can provide
 *
 * @param a the first neighborhood, all moves delivered by this one will be considered
 * @param b given that the move returned by the first neighborhood is committed, we explore the globally improving moves of this one
 * @param maximalIntermediaryDegradation the maximal degradation that is admitted for the intermediary step; the higher, the more moves will be considered
 *
 * @author renaud.delandtsheer@cetic.be
 */
class AndThen(a: Neighborhood, b: Neighborhood, maximalIntermediaryDegradation: Int = Int.MaxValue)
  extends NeighborhoodCombinator(a, b) {

  /**
   * this method is called by AndThen to notify the first step, and that it is now exploring successors of this step.
   * this method is called before the step is actually taken.
   * @param m
   */
  def notifyFirstStep(m: Move) {}

  override def getMove(obj: Objective, acceptanceCriteria: (Int, Int) => Boolean): SearchResult = {

    var secondMove: Move = null //the move performed by b
    var oldObj: Int = obj.value

    //the acceptance criterion is on the diff between the oldObj and the newObj over the two consecutive moves
    //it is evaluated for the second move
    //the first move is about accepting all moves that are not maxVal, since the newObj is for the consecutive moves,
    // and is already accepted by the time it is returned to the first neighrhood
    def firstAcceptanceCriterion(oldObj: Int, newObj: Int): Boolean = {
      newObj != Int.MaxValue
    }

    def secondAcceptanceCriteria(intermediaryObj: Int, newObj: Int): Boolean = {
      //we ignore the intermediaryObj.
      acceptanceCriteria(oldObj, newObj)
    }

    class InstrumentedObjective() extends Objective {

      override def detailedString(short: Boolean, indent: Int = 0): String = nSpace(indent) + "AndThenInstrumentedObjective(initialObjective:" + obj.detailedString(short) + ")"

      override def model = obj.model

      override def value: Int = {

        if (maximalIntermediaryDegradation != Int.MaxValue) {
          //we need to ensure that intermediary step is admissible
          val intermediaryVal = obj.value
          val intermediaryDegradation = intermediaryVal - oldObj
          if (intermediaryDegradation > maximalIntermediaryDegradation)
            return Int.MaxValue //we do not consider this first step
        }

        //now, we need to check the other neighborhood
        b.getMove(obj, secondAcceptanceCriteria) match {
          case NoMoveFound => Int.MaxValue
          case MoveFound(m: Move) =>
            secondMove = m
            m.objAfter
        }
      }
    }

    a.getMove(new InstrumentedObjective(), firstAcceptanceCriterion) match {
      case NoMoveFound => NoMoveFound
      case MoveFound(m: Move) => CompositeMove(List(m, secondMove), m.objAfter, this.toString)
    }
  }
}

/**
 * bounds the number of tolerated moves without improvements over the best value
 * the count is reset by the reset action.
 * @author renaud.delandtsheer@cetic.be
 */
class MaxMovesWithoutImprovement(a: Neighborhood, val cond: Move => Boolean, val maxMovesWithoutImprovement: Int, obj: () => Int) extends NeighborhoodCombinator(a) {

  var stepsSinceLastImprovement = 0
  var bestObj = Int.MaxValue

  override def getMove(obj: Objective, acceptanceCriteria: (Int, Int) => Boolean): SearchResult = {
    if (stepsSinceLastImprovement < maxMovesWithoutImprovement) {
      a.getMove(obj, acceptanceCriteria) match {
        case m: MoveFound => InstrumentedMove(m.m, afterMove = () => notifyMoveTaken(m.m))
        case x => x
      }
    } else {
      if (verbose >= 1) println("MaxStepsWithoutImprovement: reached " + maxMovesWithoutImprovement + " moves without improvement")
      NoMoveFound
    }
  }

  //this resets the internal state of the move combinators
  override def reset() {
    stepsSinceLastImprovement = 0
    super.reset()
  }

  def notifyMoveTaken(m: Move) {
    if (cond == null || cond(m)) {
      val newObj = obj()
      if (newObj < bestObj) {
        bestObj = newObj
        stepsSinceLastImprovement = 0
      } else {
        stepsSinceLastImprovement += 1
      }
    }
  }
}

/**
 * calls the neighborhood until an improvement over obj is achieved
 * the improvement is "since the last reset"
 * @param a
 * @param minMoves the min number of queries that will be forwarded to a (priority over the improvement)
 * @param maxMove the max number of queries that will be forwarded to a (priority over the improvement)
 * @param over the obj that is looked for improvement
 * @author renaud.delandtsheer@cetic.be
 */
class UntilImprovement(a: Neighborhood, over: () => Int, val minMoves: Int = 0, val maxMove: Int = Int.MaxValue)
  extends NeighborhoodCombinator(a) {

  //TODO: pas sûr que cela fonctionne du premier coup; peut-être faut-il faire un reset au début de toute descente.
  var oldObjOnReset = over()
  var movesQueriedSinceReset = 0

  override def getMove(obj: Objective, acceptanceCriterion: (Int, Int) => Boolean): SearchResult = {
    movesQueriedSinceReset += 1
    if (movesQueriedSinceReset < maxMove
      && (movesQueriedSinceReset < minMoves || over() >= oldObjOnReset))
      a.getMove(obj, acceptanceCriterion)
    else NoMoveFound
  }

  //this resets the internal state of the move combinators
  override def reset() {
    oldObjOnReset = over()
    movesQueriedSinceReset = 0
    super.reset()
  }
}

/**
 * the purpose of this combinator is to change the name of the neighborhood it is given as parameter.
 * it will add a prefix to all moves sent back by this combinator
 * the only purposes are documentation and debug
 * @param a
 * @param name
 */
class Name(a: Neighborhood, val name: String) extends NeighborhoodCombinator(a) {
  /**
   * @param acceptanceCriterion oldObj,newObj => should the move to the newObj be kept (default is oldObj > newObj)
   *                            beware that a changing criteria might interact unexpectedly with stateful neighborhood combinators
   * @return an improving move
   */
  override def getMove(obj: Objective, acceptanceCriterion: (Int, Int) => Boolean): SearchResult = {
    a.getMove(obj, acceptanceCriterion) match {
      case NoMoveFound => NoMoveFound
      case MoveFound(m) => CompositeMove(List(m), m.objAfter, name)
    }
  }
}

/**
 * tis combinator overrides the acceptance criterion given to the whole neighborhood
 * this can be necessary if you have a neighborhood with some phases only including simulated annealing
 * notice that the actual acceptance criteria is the one that you give,
 * with a slight modification: it will reject moves that lead to MaxInt, except if we are already at MaxInt.
 * Since MaxInt is used to represent that a strong constraint is violated, we cannot tolerate such moves at all.
 * @param a the neighborhood
 * @param overridingAcceptanceCriterion the acceptance criterion that is used instead of the one given to the overall search
 *                                      with the addition that moves leading to MaxInt will be rejected anyway, except if we are already at MaxInt
 *
 */
class WithAcceptanceCriterion(a: Neighborhood, overridingAcceptanceCriterion: (Int, Int) => Boolean) extends NeighborhoodCombinator(a) {
  /**
   * @param acceptanceCriterion this criterion is not considered by this combinator.
   * @return an improving move
   */
  override def getMove(obj: Objective, acceptanceCriterion: (Int, Int) => Boolean): SearchResult = a.getMove(obj,
    (a, b) => (a == Int.MaxValue || b != Int.MaxValue) && overridingAcceptanceCriterion(a, b))
}

/**
 * this combinator injects a metropolis acceptation function.
 * the criterion accepts all improving moves, and for worsening moves, it applies the metropolis criterion:
 * accept if math.random(0.0; 1.0) < base exponent (-gain / temperatureValue)
 * @param a the original neighborhood
 * @param temperature a function that inputs the number of moves of a that have been actually taken,
 *                    and outputs a temperature, for use in the criterion
 *                    the number of steps is reset to zero when the combinator is reset
 *                    by default, it is the constant function returning 100
 * @param base the base for the exponent calculation. default is 2
 */
class Metropolis(a: Neighborhood, temperature: Int => Float = _ => 100, base: Float = 2) extends NeighborhoodCombinator(a) {

  var moveCount = 0
  var temperatureValue: Float = temperature(moveCount)
  override def getMove(obj: Objective, acceptanceCriterion: (Int, Int) => Boolean): SearchResult =
    a.getMove(obj, acceptation) match {
      case NoMoveFound => NoMoveFound
      case MoveFound(m) => InstrumentedMove(m, notifyMoveTaken)
    }

  def acceptation(oldObj: Int, newObj: Int): Boolean = {
    val gain = oldObj - newObj
    if (gain > 0) return true
    // metropolis criterion
    return math.random < math.pow(base, -gain / temperatureValue)
  }

  def notifyMoveTaken() {
    moveCount += 1
    temperatureValue = temperature(moveCount)
  }

  //this resets the internal state of the move combinators
  override def reset() {
    super.reset()
    moveCount = 0
    temperatureValue = temperature(moveCount)
  }
}

/**
 * This is an atomic combinator, it represent that the neighborhood below should be considered as a single piece.
 * When you commit a move from this neighborhood, "a" is reset, and exhausted in a single move from Atomic(a)
 * Also, Atomic is a jump neighborhood as it cannot evaluate any objective function before the move is committed.
 * @param a
 * @param name
 */
case class Atomic(a: Neighborhood, name: String = "Atomic", bound: Int = Int.MaxValue) extends Neighborhood {
  override def getMove(obj: Objective, acceptanceCriterion: (Int, Int) => Boolean = (oldObj, newObj) => oldObj > newObj): SearchResult = {
    CallBackMove(() => a.doAllMoves(_ > bound, obj, acceptanceCriterion), Int.MaxValue, this.getClass.getSimpleName, () => ("Atomic(" + a + ")"))
  }

  override def collectStatistics: String = a.collectStatistics
}

/**
 * This represents a guided local search where a series of objective criterion are optimized one after the other
 * the switching is performed on exhaustion, and a is reset on switching.
 * Notice that if you want to use different neighborhoods depending on the objective function, you should rather use a series of neighborhood with the objectiveFucntion combinator
 * @param a the neighborhood to consider
 * @param objectives the list of objective to consider
 * @param resetOnExhaust  on exhaustion of the current objective, restores the best value for this objective before switching to the next objective
 */
class GuidedLocalSearch(a: Neighborhood, objectives: List[Objective], resetOnExhaust: Boolean) extends NeighborhoodCombinator(a) {

  var currentObjective: Objective = null
  var tailObjectives: List[Objective] = objectives
  var currentSun: Neighborhood = null

  def switchToNext(): Boolean = {
    tailObjectives match {
      case h :: t =>
        currentObjective = h
        tailObjectives = t
        currentSun = if (resetOnExhaust) new SaveBest(a, h) else a
        true
      case _ =>
        currentObjective = null
        tailObjectives = null
        currentSun = null
        false
    }
  }

  switchToNext()

  /**
   * the method that returns a move from the neighborhood.
   * The returned move should typically be accepted by the acceptance criterion over the objective function.
   * Some neighborhoods are actually jumps, so that they might violate this basic rule however.
   * @param obj the objective function. notice that it is actually a function. if you have an [[oscar.cbls.objective.Objective]] there is an implicit conversion available
   * @param acceptanceCriterion
   * @return
   */
  override def getMove(obj: Objective, acceptanceCriterion: (Int, Int) => Boolean): SearchResult = {
    if (currentSun == null) {
      NoMoveFound
    } else {
      currentSun.getMove(currentObjective, acceptanceCriterion) match {
        case NoMoveFound =>
          if (resetOnExhaust) currentSun.asInstanceOf[SaveBest].restoreBest()
          switchToNext()
          getMove(obj, acceptanceCriterion)
        case m: MoveFound => m
      }
    }
  }

  //this resets the internal state of the Neighborhood
  override def reset() {
    tailObjectives = objectives
    switchToNext()
    if (currentSun != null) currentSun.reset()
    else super.reset()
  }
}

/**
 * This represents an accumulatingSearch: it searches on a given objective until this objective gets to zero,
 * then it switches to the second one, and rejects all update that would actually decrease the first objective
 * it will use the acceptance criterion, but extend it in the second phase
 * @param a the neighborhood
 * @param firstObjective the first objective function
 * @param secondObjective the second objective function
 */
class AccumulatingSearch(a: Neighborhood, firstObjective: Objective, secondObjective: Objective) extends NeighborhoodCombinator(a) {

  val fullSecondObjective = new CascadingObjective(firstObjective, secondObjective)

  /**
   * the method that returns a move from the neighborhood.
   * The returned move should typically be accepted by the acceptance criterion over the objective function.
   * Some neighborhoods are actually jumps, so that they might violate this basic rule however.
   * @param obj the objective function. notice that it is actually a function. if you have an [[oscar.cbls.objective.Objective]] there is an implicit conversion available
   * @param acceptanceCriterion
   * @return
   */
  override def getMove(obj: Objective, acceptanceCriterion: (Int, Int) => Boolean): SearchResult = {
    if (firstObjective() != 0) {
      a.getMove(firstObjective, acceptanceCriterion)
    } else {
      a.getMove(fullSecondObjective, acceptanceCriterion)
    }
  }
}

/**
 * Forces the use of a given objective function.
 * this overrides the one that you might pass in the higher level
 * @param a the combined neighborhood
 * @param overridingObjective the objective to use instead of the given one
 */
class OverrideObjective(a: Neighborhood, overridingObjective: Objective) extends NeighborhoodCombinator(a) {
  /**
   * the method that returns a move from the neighborhood.
   * The returned move should typically be accepted by the acceptance criterion over the objective function.
   * Some neighborhoods are actually jumps, so that they might violate this basic rule however.
   * @param obj the objective function. notice that it is actually a function. if you have an [[oscar.cbls.objective.Objective]] there is an implicit conversion available
   * @param acceptanceCriterion
   * @return
   */
  override def getMove(obj: Objective, acceptanceCriterion: (Int, Int) => Boolean): SearchResult = getMove(overridingObjective, acceptanceCriterion)
}

case class Statistics(a:Neighborhood,ignoreInitialObj:Boolean = false)extends NeighborhoodCombinator(a){

  var nbCalls = 0
  var nbFound = 0
  var totalGain = 0
  var totalTimeSpent: Long = 0

  def resetStatistics(){
    nbCalls = 0
    nbFound = 0
    totalGain = 0
    totalTimeSpent = 0
  }

  /**
   * the method that returns a move from the neighborhood.
   * The returned move should typically be accepted by the acceptance criterion over the objective function.
   * Some neighborhoods are actually jumps, so that they might violate this basic rule however.
   * @param obj the objective function. notice that it is actually a function. if you have an [[oscar.cbls.objective.Objective]] there is an implicit conversion available
   * @param acceptanceCriterion
   * @return
   */
  override def getMove(obj: Objective, acceptanceCriterion: (Int, Int) => Boolean): SearchResult = {

    nbCalls += 1
    val oldObj = obj.value
    val startTime = System.currentTimeMillis

    a.getMove(obj, acceptanceCriterion) match {
      case NoMoveFound =>
        totalTimeSpent += System.currentTimeMillis - startTime
        NoMoveFound
      case m: MoveFound =>
        totalTimeSpent += System.currentTimeMillis - startTime
        nbFound += 1
        if (!ignoreInitialObj || nbCalls > 1) totalGain += oldObj - m.objAfter
        m
    }
  }

  def gainPerCall:String = if(nbCalls ==0) "NA" else ("" + totalGain / nbCalls)
  def callDuration:String = if(nbCalls == 0 ) "NA" else ("" + totalTimeSpent / nbCalls)
  //gain in obj/100ms
  def slope:String = if(totalTimeSpent == 0) "NA" else ("" + ((100 * totalGain) / totalTimeSpent).toInt)

  override def collectStatistics: String =
    padToLength("" + a,31) +
      padToLength("" + nbCalls,6) +
      padToLength("" + nbFound,6) +
      padToLength("" + totalGain,8) +
      padToLength("" + totalTimeSpent,12) +
      padToLength("" + gainPerCall,8) +
      padToLength("" + callDuration,12)+
      slope

  private def padToLength(s: String, l: Int) = (s + nStrings(l, " ")).substring(0, l)
  private def nStrings(n: Int, s: String): String = if (n <= 0) "" else s + nStrings(n - 1, s)

  override def toString: String = "Statistics(" + a + " nbCalls:" + nbCalls + " nbFound:" + nbFound + " totalGain:" + totalGain + " totalTimeSpent " + totalTimeSpent + " ms" + ")"

  def slopeOrZero:Int = if(totalTimeSpent == 0) 0 else ((100 * totalGain) / totalTimeSpent).toInt
}

object Statistics{
  private def padToLength(s: String, l: Int) = (s + nStrings(l, " ")).substring(0, l)
  private def nStrings(n: Int, s: String): String = if (n <= 0) "" else s + nStrings(n - 1, s)
  def statisticsHeader = padToLength("Neighborhood",30) + " calls found sumGain sumTime(ms) avgGain avgTime(ms) slope(-Dobj/.1s)"
}

