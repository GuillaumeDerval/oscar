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

package oscar.cbls.search.core

import oscar.cbls.invariants.core.computation.Store
import oscar.cbls.objective.{LoggingObjective, Objective}
import oscar.cbls.search.combinators._
import oscar.cbls.search.move.{CallBackMove, Move}

import scala.language.{implicitConversions, postfixOps}

abstract sealed class SearchResult
case object NoMoveFound extends SearchResult

case class MoveFound(m: Move) extends SearchResult {
  def commit() { m.commit() }
  def objAfter = m.objAfter
  override def toString: String = m.toString
}

object SearchResult {
  implicit def moveToSearchResult(m: Move): MoveFound = MoveFound(m)
}

abstract class JumpNeighborhood extends Neighborhood {

  /**
   * the method that actually performs the move
   * notice that this method is called when the move is committed,
   * which happens after the neighborhood returns the move.
   */
  def doIt()

  /**
   * this method checks that the jump can actually be performed
   * it is called before the neighborhood returns either MoveFound or NoMoveFound
   * notice that the doIt method is called only if canDoIt returned true.
   * override it if your jump might not be applicable
   * (and do not forget to handle this case in your search strategy)
    *
    * @return
   */
  def canDoIt: Boolean = true

  def shortDescription(): String

  override def getMove(obj: Objective, acceptanceCriterion: (Int, Int) => Boolean = (oldObj, newObj) => oldObj > newObj): SearchResult = {
    if (canDoIt) CallBackMove(() => doIt, valueAfter, this.getClass.getSimpleName, shortDescription)
    else NoMoveFound
  }

  /**
   * returns the value after the move
   * called by getMove, by default, this is Int.MaxValue, which is the correct value for a jump
   * in case your jump does not modify the obj function and you want to include this in the move description, override this method
    *
    * @return
   */
  def valueAfter = Int.MaxValue
}

abstract class JumpNeighborhoodParam[T] extends Neighborhood {

  final def doIt() {
    doIt(getParam)
  }

  def doIt(param: T)

  /**if null is returned, the neighborhood returns NoMoveFound*/
  def getParam: T
  def getShortDescription(param: T): String

  override def getMove(obj: Objective, acceptanceCriterion: (Int, Int) => Boolean): SearchResult = {
    val param: T = getParam
    if (param == null) NoMoveFound
    else CallBackMove((param: T) => doIt(param), Int.MaxValue, this.getClass.getSimpleName, () => getShortDescription(param), param)
  }
}

/**
 * @author renaud.delandtsheer@cetic.be
 */
abstract class Neighborhood(name:String = null) {

  /**
   * collects and returns the statistics that have been requested in the neighborhood.
   * use the Statistics combinator to activate the collection of statistics
 *
   * @return
   */
  final def profilingStatistics:String = Profile.statisticsHeader + "\n" + collectProfilingStatistics.mkString("\n")
  def collectProfilingStatistics:List[String] = List.empty

  /**
   * the method that returns a move from the neighborhood.
   * The returned move should typically be accepted by the acceptance criterion over the objective function.
   * Some neighborhoods are actually jumps, so that they might violate this basic rule however.
 *
   * @param obj the objective function. notice that it is actually a function. if you have an [[oscar.cbls.objective.Objective]] there is an implicit conversion available
   * @param acceptanceCriterion
   * @return
   */
  def getMove(obj: Objective, acceptanceCriterion: (Int, Int) => Boolean = (oldObj, newObj) => oldObj > newObj): SearchResult

  //this resets the internal state of the Neighborhood
  def reset() {}

  def resetStatistics() {}

  override def toString: String = (if(name == null) this.getClass.getSimpleName else name)

  //TODO: ajouter un niveau qui montre les exhausted
  /**
   * verbosity: 0: none
   * 1: moves
   * 2: moves + failed searches
   * 3: moves + explored neighbors
   */
  var _verbose: Int = 0
  def verbose: Int = _verbose
  def verbose_=(i: Int) {
    _verbose = i
    additionalStringGenerator = null
  }

  var additionalStringGenerator: () => String = null

  /**
   * sets the verbosity level with an additiojnal string generator that ins called eieher on eahc move (level = 1)
   *   or for each explored neighbor (level = 2)
   */
  def verboseWithExtraInfo(verbosity: Int, additionalString: () => String) {
    verbose = verbosity
    additionalStringGenerator = additionalString
  }

  protected def printTakenMoves: Boolean = verbose >= 1
  protected def printPerformedSearches: Boolean = verbose >= 2
  protected def printExploredNeighbors: Boolean = verbose >= 3


  //the number of characters to display in case a verbose approach is deployed.
  var paddingLength: Int = 100

  /**
   * @return true if a move has been performed, false otherwise
   */
  def doImprovingMove(obj: Objective): Boolean = 0 != doAllMoves(_ >= 1, obj)

  /**
   * @param shouldStop a function that takes the iteration number and returns true if search should be stopped
   *                   eg if the problem is considered as solved
   *                   you can evaluate some objective function there such as a violation degree
   * @param acceptanceCriterion a criterion for accepting a move
   *                            by default, we only accept improving moves, but you could change it
   *                            and accept degrading moves as well (eg for tabu search)
   *                            notice that some moves do not consider the acceptance criterion
   *                            because their purpose is to randomize the current solution.
   * @return the number of moves performed
   */
  def doAllMoves(shouldStop: Int => Boolean = _ => false, obj: Objective, acceptanceCriterion: (Int, Int) => Boolean = (oldObj, newObj) => oldObj > newObj): Int = {
    if (verbose != 0){
      //println("start doAllMove at " + java.time.LocalDateTime.now)
      println("initial objective function:" + obj)
    }
    val startSearchNanotime = System.nanoTime()
    var bestObj = Int.MaxValue
    var prevObj = Int.MaxValue
    var toReturn = 0
    var moveCount = 0
    val enrichedObj = if (additionalStringGenerator == null) obj else new ObjWithStringGenerator(obj, additionalStringGenerator)
    while (!shouldStop(moveCount)) {
      getMove(enrichedObj, acceptanceCriterion) match {
        case NoMoveFound =>
          if (printTakenMoves) println("no more move found after " + toReturn + " it, " + ((System.nanoTime() - startSearchNanotime)/1000000).toInt + " ms ")
          return toReturn;
        case m: MoveFound =>
          if (printTakenMoves) {

            def nStrings(n: Int, s: String): String = if (n <= 0) "" else s + nStrings(n - 1, s)
            def padToLength(s: String, l: Int) = (s + nStrings(l, " ")).substring(0, l)
            def trimToLength(s: String, l: Int) = if (s.length >= l) s.substring(0, l) else s

            if (m.objAfter != Int.MaxValue) {
              val firstPostfix = if (m.objAfter < prevObj) "-"
              else if (m.objAfter == prevObj) "="
              else "+"

              prevObj = m.objAfter

              val secondPostfix = if (m.objAfter < bestObj) {
                bestObj = m.objAfter
                " #"
              } else if (m.objAfter == bestObj) " °"
              else ""

              println(padToLength(m.toString(), paddingLength) + " " + firstPostfix + secondPostfix)
            } else {
              prevObj = m.objAfter
              println(trimToLength(m.toString(), paddingLength))
            }
          }

          m.commit()
          if (additionalStringGenerator != null) println("after move is committed: " + additionalStringGenerator())
          if (obj.value == Int.MaxValue) println("Warning : objective == MaxInt, maybe you have some strong constraint violated?")
          assert(obj.value == m.objAfter, "neighborhood was lying!:" + m + " got " + obj)
          true
      }
      toReturn += 1
      moveCount += 1
    }
    if (printTakenMoves) {
      println("stop criteria of doAllMove met after " + moveCount + " moves, " + ((System.nanoTime() - startSearchNanotime)/1000000).toInt + " ms")
    }
    toReturn
  }

  /**
   * this combinator randomly tries one neighborhood.
   * it tries the other if the first did not find any move
 *
   * @param b another neighborhood
   * @author renaud.delandtsheer@cetic.be
   */
  def random(b: Neighborhood): Neighborhood = new Random(this, b)

  /**
   * this combinator sequentially tries all neighborhoods until one move is found
   * between calls, it will roll back to the first neighborhood
   * it tries a first, and if no move it found, tries b
   * a is reset if it did not find anything.
 *
   * @param b another neighborhood
   * @author renaud.delandtsheer@cetic.be
   */
  def orElse(b: Neighborhood): Neighborhood = new OrElse(this, b)

  /**
   * performs a restart of the search for a number of time.
   * it queries neighborhood on the left every time (this is the search neighborhood)
   * if the search neighborhood is exhausted, it queries the randomizationNeighborhood once (this is the randomization neighborhood, and resets the neighborhood on the left
   * the process of restarting is allowed maxRestartWithoutImprovement time without improvement over the objective function obj,
   * that is: every time the search neighborhood is exhausted, it checks if the search delivered an improvement over the objective function,
   * and the restart is only performed if it could find an improvement at least once in the last "maxRestartWithoutImprovement" descents.
   *
   * the best solution is reloaded at exhaustion of this neighborhood.
   *
   * @param randomizationNeighborhood the neighborhood that will randomize the current solution
   * @param maxRestartWithoutImprovement the stop criterion of the restarting
   * @param obj the objective function
   */
  def onExhaustRestartAfter(randomizationNeighborhood:Neighborhood, maxRestartWithoutImprovement:Int, obj:Objective) = {

    (this orElse (randomizationNeighborhood
      maxMoves maxRestartWithoutImprovement withoutImprovementOver obj improvementBeignMeasuredBeforeNeighborhoodExploration)
      ) saveBestAndRestoreOnExhaust obj
  }


  /**
   * alias for (this maxMoves 1) exhaust b
   * this wil be queried once, and then, queries will be forwarded to b.
 *
   * @param b
   * @return
   */
  def sequence(b: Neighborhood): Neighborhood = this maxMoves 1 exhaust b

  /**
   * this combinator always selects the best move between the two parameters
   * notice that this combinator makes more sense
   * if the two neighborhood return their best found move,
   * and not their first found move, as usually done.
 *
   * @author renaud.delandtsheer@cetic.be
   */
  def best(b: Neighborhood): Neighborhood = new Best(this, b)

  /**
   * this combinator is stateful.
   * it returns the result of the first Neighborhood until it returns NoMoveFound.
   * It then switches to the other Neighborhood.
   * it does not come back to the first one after the second one is exhausted
 *
   * @author renaud.delandtsheer@cetic.be
   */
  def exhaust(b: Neighborhood): Neighborhood = new Exhaust(this, b)

  /**
   * this combinator is stateful.
   * it returns the result of one Neighborhood until it returns NoMoveFound.
   * It then switches to the other Neighborhood.
   * it starts with Neighborhood a
 *
   * @author renaud.delandtsheer@cetic.be
   */
  def exhaustBack(b: Neighborhood): Neighborhood = new ExhaustBack(this, b)

  /**
   * this combinator is stateful.
   * it returns the result of the first Neighborhood until it returns NoMoveFound.
   * It then switches to the other Neighborhood,
   * but only if a move was found by the first neighborhood
   * it does not come back to the first one after the second one is exhausted
 *
   * @author renaud.delandtsheer@cetic.be
   */
  def exhaustAndContinueIfMovesFound(b: Neighborhood) = new ExhaustAndContinueIfMovesFound(this, b)


  /**
   * this one bounds the number of time the search is actually performed
   * notice that the count is reset by the reset operation
 *
   * @author renaud.delandtsheer@cetic.be
   */
  def maxSearches(maxMove: Int) = new BoundSearches(this, maxMove)

  /**
   * this one bounds the number of moves done with this neighborhood
   * notice that the count is reset by the reset operation
 *
   * @author renaud.delandtsheer@cetic.be
   */
  def maxMoves(maxMove: Int) = new MaxMoves(this, maxMove)

  /**
   * no move if cond evaluates to false, otherwise ,it forwards the search request to a
 *
   * @param cond a stop criterion
   */
  def stopWhen(cond: () => Boolean) = new StopWhen(this, cond)

  /**
   * if it is false,it returns NoMovesFound without exploring the neighborhood at all.
 *
   * @param cond
   * @return
   */
  def guard(cond:()=>Boolean) = Guard(cond,this)

  /**
   * this is an alias for maxMoves 1
   */
  def once = new MaxMoves(this, 1)

  /**
   * This combinators queries a once avery n time it is queried.
   * the other times, it returns NoMovesFound.
   * if n finds no moves, depending on retryOnNoMoveFound,
   * it will either keep on querying n until a move is found, or continue its sequence of one out of n
 *
   * @param n the size of teh sequence
   * @param retryOnNoMoveFound if true, keeps on querying n on NoMoveFound, otherwise, continues the sequence
   */
  def onceEvery(n: Int, retryOnNoMoveFound: Boolean = false) = new OnceEvery(this, n, retryOnNoMoveFound)

  /**
   * bounds the number of tolerated moves without improvements over the best value
   * the count is reset by the reset action.
 *
   * @author renaud.delandtsheer@cetic.be
   */
  def maxMovesWithoutImprovement(maxMove: Int, obj: () => Int) = new MaxMovesWithoutImprovement(this, null, maxMove, obj)

  /**
   * makes a round robin on the neighborhood. it swaps as soon as one does not find a move
   * and swaps neighborhood after "step" invocations
 *
   * @author renaud.delandtsheer@cetic.be
   */
  def roundRobin(b: Neighborhood): RoundRobinNoParam = new RoundRobinNoParam(this, b)

  /**
   * this combinator attaches a custom code to a given neighborhood.
   * the code is called whenever a move is asked to the neighborhood.
 *
   * @param proc the procedure to execute before the neighborhood is queried
   */
  def onQuery(proc: => Unit) = new DoOnQuery(this, () => proc)

  /**
   * this combinator attaches a custom code to a given neighborhood.
   * the code is called whenever a move from this neighborhood is taken
   * The callBack is performed before the move is actually taken.
 *
   * @param proc the procedure to execute when the move is taken
   */
  def beforeMove(proc: => Unit) = new DoOnMove(this, procBeforeMove = (_) => proc)

  /**
   * this combinator attaches a custom code to a given neighborhood.
   * the code is called whenever a move from this neighborhood is taken
   * is gets the applied move in input.
   * The callBack is performed before the move is actually taken.
 *
   * @param procOnMove a procedure that inputs the move that is applied;
   *                   use this to update a Tabu for instance
   */
  def beforeMove(procOnMove: Move => Unit) = new DoOnMove(this, procBeforeMove = procOnMove)

  /**
   * this combinator attaches a custom code to a given neighborhood.
   * the code is called whenever a move from this neighborhood is taken
   * The callBack is performed after the move is actually taken.
 *
   * @param proc the procedure to execute when the move is taken
   */
  def afterMove(proc: => Unit) = new DoOnMove(this, procAfterMove = (_) => proc)

  /**
   * this combinator attaches a custom code to a given neighborhood.
   * the code is called whenever a move from this neighborhood is taken
   * is gets the applied move in input.
   * The callBack is performed after the move is actually taken.
 *
   * @param procOnMove a procedure that inputs the move that is applied;
   *                   use this to update a Tabu for instance
   */
  def afterMoveOnMove(procOnMove: Move => Unit) = new DoOnMove(this, procAfterMove = procOnMove)

  /**
   * this combinator attaches a custom code to a given neighborhood.
   * the code is called whenever a move from this neighborhood is taken for the first time.
   * notice that this neighborhood is reset, so first time can occur several times.
 *
   * @param proc the procedure to call on one first move that is performed from this neighborhood
   */
  def onFirstMove(proc: => Unit) = new DoOnFirstMove(this, () => proc)

  def saveBest(o: Objective) = new SaveBest(this, o)

  def saveBestAndRestoreOnExhaust(obj: Objective) = (new SaveBest(this, obj)) restoreBestOnExhaust

  /**
   * retries the move before concluding to noMove can be found
 *
   * @param cond condition that takes the number of consecutive NoMoveFound, and says if we should try again returns true if yes, false otherwise
   *             by default, we allow a single retry.
   */
  def retry(cond: Int => Boolean = (n: Int) => n <= 1) = new Retry(this, cond)

  /**
   * to prevent resetting the internal state of this neighborhood
 *
   * @return
   */
  def noReset: Neighborhood = new NoReset(this)

  /**
   * defines a name wor this (composite) neighborhood
   * this will be used as prefix for each move returned by this neighborhood (the original name will still exist)
   * use this for debug and documentation purpose only
 *
   * @param name the name that will prefix all returned moves, used for verbosities
   * @return
   */
  def name(name: String) = new Name(this, name)

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
   * @param b given that the move returned by the first neighborhood is committed, we explore the globally improving moves of this one
   * @author renaud.delandtsheer@cetic.be
   */
  def andThen(b: Neighborhood) = new AndThen(this, b)

  /**
   * tis combinator overrides the acceptance criterion given to the whole neighborhood
   * this can be necessary if you have a neighborhood with some phases only including simulated annealing
 *
   * @param overridingAcceptanceCriterion the acceptance criterion that is used instead of the one given to the overall sear
   */
  def withAcceptanceCriterion(overridingAcceptanceCriterion: (Int, Int) => Boolean) = new WithAcceptanceCriterion(this, overridingAcceptanceCriterion)

  /**
   * this combinator overrides accepts all moves (this is the withAcceptanceCriteria, given the fully acceptant criterion
   */
  def acceptAll() = new WithAcceptanceCriterion(this, (_: Int, _: Int) => true)

  /**
   * proposes a round-robin with that.
   * notice that you can chain steps; this will build a round-robin on the whole sequence (although this operation is not associative, so better not use parentheses)
 *
   * @param b
   * @return
   */
  def step(b: Neighborhood) = new RoundRobin(List(this, b))

  /**
   * calls the neighborhood until an improvement over obj is achieved
   * the improvement is "since the last reset"
 *
   * @param minMoves the min number of queries that will be forwarded to a (priority over the improvement)
   * @param maxMove the max number of queries that will be forwarded to a (priority over the improvement)
   * @param obj the obj that is looked for improvement
   * @author renaud.delandtsheer@cetic.be
   */
  def untilImprovement(obj: () => Int, minMoves: Int = 0, maxMove: Int = Int.MaxValue) = new UntilImprovement(this, obj, minMoves, maxMove)

  /**
   * this combinator injects a metropolis acceptation function.
   * the criterion accepts all improving moves, and for worsening moves, it applies the metropolis criterion:
   * accept if math.random(0.0; 1.0) < base exponent (-gain / temperatureValue)
 *
   * @param temperature a function that inputs the number of moves taken, and outputs a temperature, for use in the criterion
   *                    the number of steps is reset to zero when the combinator is reset.
   *                    By default, the temperature is 100/the number of steps
   * @param base the base for the exponent calculation. default is 2
   */
  def metropolis(temperature: Int => Float = (it: Int) => 100 / (it + 1), base: Float = 2) = new Metropolis(this, temperature, base)

  /**
   * This is an atomic combinator, it represent that the neighborhood below should be considered as a single piece.
   * When you commit a move from this neighborhood, "a" is reset, and exhausted in a single move from Atomic(a)
   * Also, Atomic is a jump neighborhood as it cannot evaluate any objective function before the move is committed.
 *
   * @param name a name for the atomic move
   */
  def atomic(name: String = "Atomic", bound: Int = Int.MaxValue) = new Atomic(this, name, bound)

  /**
   * Forces the use of a given objetive function.
   * this overrides the one that you might pass in the higher level
 *
   * @param overridingObjective the objective to use instead of the given one
   */
  def overrideObjective(a: Neighborhood, overridingObjective: Objective) = new OverrideObjective(a, overridingObjective)

  /**
   * This represents a guided local search where a series of objective criterion are optimized one after the other
   * the switching is performed on exhaustion, and a is reset on switching.
   * Notice that if you want to use different neighborhoods depending on the objective function, you should rather use a series of neighborhood with the objectiveFucntion combinator
 *
   * @param objectives the list of objective to consider
   * @param resetOnExhaust  on exhaustion of the current objective, restores the best value for this objective before switching to the next objective
   */
  def guidedLocalSearch(a: Neighborhood, objectives: List[Objective], resetOnExhaust: Boolean) = new GuidedLocalSearch(a, objectives, resetOnExhaust)

  /**
   * This represents an accumulatingSearch: it searches on a given objective until this objective gets to zero,
   * then it switches to the second one, and rejects all update that would actually decrease the first objective
   * it will use the acceptance criterion, but extend it in the second phase
 *
   * @param firstObjective the first objective function
   * @param secondObjective the second objective function
   */
  def accumulatingSearch(a: Neighborhood, firstObjective: Objective, secondObjective: Objective) = new AccumulatingSearch(a, firstObjective, secondObjective)
}

/**
 * a neighborhood that never finds any move (quite useless, actually)
 */
case object NoMoveNeighborhood extends Neighborhood {
  override def getMove(obj: Objective, acceptanceCriterion: (Int, Int) => Boolean): SearchResult = NoMoveFound
}

/**
 * This neighborhood always returns the same move, given in the constructor
 *
 * @param m the move to return when the neighborhood is queried for a move
 */
case class ConstantMoveNeighborhood(m: Move) extends Neighborhood {
  override def getMove(obj: Objective, acceptanceCriterion: (Int, Int) => Boolean): SearchResult = m
}

trait SupportForAndThenChaining[MoveType<:Move] extends Neighborhood{

  def instantiateCurrentMove(newObj:Int):MoveType

  def dynAndThen(other:MoveType => Neighborhood,maximalIntermediaryDegradation: Int = Int.MaxValue):DynAndThen[MoveType] = {
    DynAndThen[MoveType](this,other,maximalIntermediaryDegradation)
  }
}

/**
 * This is an easier way to implement your neighborhood; it provides a simplified interface and hides away searching for the best move vs. the first move
 * and the management of the acceptingCriterion.
 *
 * to implement a neighborhood, you must implement the method exploreNeighborhood
 * in this method, you evaluate moves, and to notify that a move has been
 * explored you call the method evaluateCurrentMoveObjTrueIfStopRequired(newObj:Int)
 *
 * this method tells you if the search must be stopped, or not.
 *
 * you must also implement the method instantiateCurrentMove,
 * so that the framework can actually get the current move, notably to return and comit it.
 *
 * this method must be able to return its result when you call the method evaluateCurrentMoveObjTrueIfStopRequired
 *
 * to evaluate the objective function, the Objective is in the variable obj
 *
 * @param best true if you want the best move false if you want the first acceptable move
 * @param neighborhoodName the name of the neighborhood, used for verbosities
 */
abstract class EasyNeighborhood[M<:Move](best:Boolean = false, neighborhoodName:String=null)
  extends Neighborhood with SupportForAndThenChaining[M]{

  protected def neighborhoodNameToString: String = if (neighborhoodName != null) neighborhoodName else this.getClass().getSimpleName()

  override def toString: String = neighborhoodNameToString

  //passing parameters, and getting return values from the search
  private var oldObj: Int = 0
  private var acceptanceCriterion: (Int, Int) => Boolean = null
  private var toReturnMove: Move = null
  private var bestNewObj: Int = Int.MaxValue
  protected var obj: Objective = null

  override final def getMove(obj: Objective, acceptanceCriterion: (Int, Int) => Boolean): SearchResult = {

    oldObj = obj.valueNoSearch
    this.acceptanceCriterion = acceptanceCriterion
    toReturnMove = null
    bestNewObj = Int.MaxValue
    this.obj = if (printExploredNeighbors) new LoggingObjective(obj) else obj
    if (printPerformedSearches)
      println(neighborhoodNameToString + ": start exploration")

    //    if (printExploredNeighbors) println(obj.detailedString(false, 2))

    exploreNeighborhood()

    if (toReturnMove == null || (best && !acceptanceCriterion(oldObj, bestNewObj))) {
      if (printPerformedSearches) {
        println(neighborhoodNameToString + ": no move found")
      }
      NoMoveFound
    } else {
      if (printPerformedSearches) {
        println(neighborhoodNameToString + ": move found: " + toReturnMove)
      }
      toReturnMove
    }
  }

  /**
   * This is the method you must implement and that performs the search of your neighborhood.
   * every time you explore a neighbor, you must perform the calls to notifyMoveExplored or moveRequested(newObj) && submitFoundMove(myMove)){
   * as explained in the documentation of this class
   */
  def exploreNeighborhood()

  def instantiateCurrentMove(newObj: Int): M

  var tmpNewObj: Int = 0

  def evaluateCurrentMoveObjTrueIfStopRequired(newObj: Int): Boolean = {
    //cas à gérer:
    //verbose (on affiche le mouvement; qu'on instancie donc avec obj)
    //andThen (pas utile de l'instancier sns obj pq on va de tt façons propager en commençant le voisinage suivant.
    //normal
    //pour l'instant, cas normal uniquement (en fait le AndThen sera géré par le combinateur idoine)

    val myPrintExploredNeighbors = printExploredNeighbors

    if (best) {
      if (newObj < bestNewObj) {
        bestNewObj = newObj
        toReturnMove = instantiateCurrentMove(newObj)
        if (myPrintExploredNeighbors) {
          println("Explored " + toReturnMove + ", new best, saved (might be filtered out if best is not accepted)")
          println(obj.asInstanceOf[LoggingObjective].getAndCleanEvaluationLog.mkString("\n"))
        }
      } else {
        if (myPrintExploredNeighbors) {
          println("Explored " + instantiateCurrentMove(newObj) + ", not the new best, not saved")
          println(obj.asInstanceOf[LoggingObjective].getAndCleanEvaluationLog.mkString("\n"))
        }
      }
      false //since we are looking for the best one, we do not stop
    } else {
      if (acceptanceCriterion(oldObj, newObj)) {
        bestNewObj = newObj
        toReturnMove = instantiateCurrentMove(newObj)
        if (myPrintExploredNeighbors) {
          println("Explored " + toReturnMove + ", accepted, exploration stopped")
          println(obj.asInstanceOf[LoggingObjective].getAndCleanEvaluationLog.mkString("\n"))
        }
        true //since we are looking for the first one, we stop
      } else {
        //explored, but not saved
        if (myPrintExploredNeighbors) {
          println("Explored " + instantiateCurrentMove(newObj) + ", not accepted, not saved")
          println(obj.asInstanceOf[LoggingObjective].getAndCleanEvaluationLog.mkString("\n"))
        }
        false
      }
    }
  }

  def afterMoveOnMove(proc:M => Unit):Neighborhood = super.afterMoveOnMove((m:Move) => proc(m.asInstanceOf[M]))
}

class ObjWithStringGenerator(obj: Objective, additionalStringGenerator: () => String) extends Objective {
  override def detailedString(short: Boolean, indent: Int): String = {
    obj.detailedString(short,indent)+ "\n" + nSpace(indent) + additionalStringGenerator().split("\\R",-1).mkString("\n" + nSpace(indent)) + "\n"
  }

  override def model: Store = obj.model

  override def value: Int = obj.value
}



