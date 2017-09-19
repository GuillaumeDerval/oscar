package oscar.cbls.lib.search.combinators

import java.awt.Color

import oscar.cbls._
import oscar.cbls.core.objective.Objective
import oscar.cbls.core.search.{Move, Neighborhood}
import oscar.cbls.util.StopWatch
import oscar.examples.cbls.routing.visual.ColorGenerator

/**
 * Created by rdl on 11-09-17.
 */
trait CombinatorsAPI {

  def bestSlopeFirst(l:List[Neighborhood],
                     tabuLength:Int = 10,
                     overrideTabuOnFullExhaust:Int = 9, refresh:Int = 100) =
    new BestSlopeFirst(l, tabuLength, overrideTabuOnFullExhaust, refresh)

  /**
   * collects statistics about the run time and progress achieved by neighborhood a
   * they can be obtained by querying this object with method toString
   * or globally on the whole neighborhood using the method statistics
   * WARNING: do not use this inside an AndThen,
   *          since the objective function is instrumented by this combinator, so the statistics will be counter-intuitive
   *
   * @param a
   * @param ignoreInitialObj
   */
  def profile(a:Neighborhood,ignoreInitialObj:Boolean = false) = new Profile(a,ignoreInitialObj)

}




class InstrumentedNeighborhood(n:Neighborhood){
  /**
   * this combinator randomly tries one neighborhood.
   * it tries the other if the first did not find any move
   *
   * @param b another neighborhood
   * @author renaud.delandtsheer@cetic.be
   */
  def random(b: Neighborhood): Neighborhood = new Random(n, b)

  /**
   * this combinator sequentially tries all neighborhoods until one move is found
   * between calls, it will roll back to the first neighborhood
   * it tries a first, and if no move it found, tries b
   * a is reset if it did not find anything.
   *
   * @param b another neighborhood
   * @author renaud.delandtsheer@cetic.be
   */
  def orElse(b: Neighborhood): Neighborhood = new OrElse(n, b)

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

    (n orElse (randomizationNeighborhood
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
  def sequence(b: Neighborhood): Neighborhood = n maxMoves 1 exhaust b

  /**
   * this combinator always selects the best move between the two parameters
   * notice that this combinator makes more sense
   * if the two neighborhood return their best found move,
   * and not their first found move, as usually done.
   *
   * @author renaud.delandtsheer@cetic.be
   */
  def best(b: Neighborhood): Neighborhood = new oscar.cbls.lib.search.combinators.Best(n, b)

  /**
   * this combinator is stateful.
   * it returns the result of the first Neighborhood until it returns NoMoveFound.
   * It then switches to the other Neighborhood.
   * it does not come back to the first one after the second one is exhausted
   *
   * @author renaud.delandtsheer@cetic.be
   */
  def exhaust(b: Neighborhood): Neighborhood = new Exhaust(n, b)

  /**
   * this combinator is stateful.
   * it returns the result of one Neighborhood until it returns NoMoveFound.
   * It then switches to the other Neighborhood.
   * it starts with Neighborhood a
   *
   * @author renaud.delandtsheer@cetic.be
   */
  def exhaustBack(b: Neighborhood): Neighborhood = new ExhaustBack(n, b)

  /**
   * this combinator is stateful.
   * it returns the result of the first Neighborhood until it returns NoMoveFound.
   * It then switches to the other Neighborhood,
   * but only if a move was found by the first neighborhood
   * it does not come back to the first one after the second one is exhausted
   *
   * @author renaud.delandtsheer@cetic.be
   */
  def exhaustAndContinueIfMovesFound(b: Neighborhood) = new ExhaustAndContinueIfMovesFound(n, b)


  /**
   * this one bounds the number of time the search is actually performed
   * notice that the count is reset by the reset operation
   *
   * @author renaud.delandtsheer@cetic.be
   */
  def maxSearches(maxMove: Int) = new BoundSearches(n, maxMove)

  /**
   * this one bounds the number of moves done with this neighborhood
   * notice that the count is reset by the reset operation
   *
   * @author renaud.delandtsheer@cetic.be
   */
  def maxMoves(maxMove: Int) = new MaxMoves(n, maxMove)

  /**
   * no move if cond evaluates to false, otherwise ,it forwards the search request to a
   *
   * @param cond a stop criterion
   */
  def stopWhen(cond: () => Boolean) = new StopWhen(n, cond)

  /**
   * if it is false,it returns NoMovesFound without exploring the neighborhood at all.
   *
   * @param cond
   * @return
   */
  def guard(cond:()=>Boolean) = Guard(cond,n)

  /**
   * this is an alias for maxMoves 1
   */
  def once = new MaxMoves(n, 1)

  /**
   * This combinators queries a once avery n time it is queried.
   * the other times, it returns NoMovesFound.
   * if n finds no moves, depending on retryOnNoMoveFound,
   * it will either keep on querying n until a move is found, or continue its sequence of one out of n
   *
   * @param n the size of teh sequence
   * @param retryOnNoMoveFound if true, keeps on querying n on NoMoveFound, otherwise, continues the sequence
   */
  def onceEvery(x: Int, retryOnNoMoveFound: Boolean = false) = new OnceEvery(n, x, retryOnNoMoveFound)

  def onExhaust(proc:()=>Unit) = OnExhaust(n,proc,false)

  def onFirstExhaust(proc:()=>Unit) = OnExhaust(n,proc,true)

  /**
   * bounds the number of tolerated moves without improvements over the best value
   * the count is reset by the reset action.
   *
   * @author renaud.delandtsheer@cetic.be
   */
  def maxMovesWithoutImprovement(maxMove: Int, obj: () => Int) = new MaxMovesWithoutImprovement(n, null, maxMove, obj)

  /**
   * makes a round robin on the neighborhood. it swaps as soon as one does not find a move
   * and swaps neighborhood after "step" invocations
   *
   * @author renaud.delandtsheer@cetic.be
   */
  def roundRobin(b: Neighborhood): RoundRobinNoParam = new RoundRobinNoParam(n, b)

  /**
   * this combinator attaches a custom code to a given neighborhood.
   * the code is called whenever a move is asked to the neighborhood.
   *
   * @param proc the procedure to execute before the neighborhood is queried
   */
  def onQuery(proc: => Unit) = new DoOnQuery(n, () => proc)

  /**
   * this combinator attaches a custom code to a given neighborhood.
   * the code is called whenever a move from this neighborhood is taken
   * The callBack is performed before the move is actually taken.
   *
   * @param proc the procedure to execute when the move is taken
   */
  def beforeMove(proc: => Unit) = new DoOnMove(n, procBeforeMove = (_) => proc)

  /**
   * this combinator attaches a custom code to a given neighborhood.
   * the code is called whenever a move from this neighborhood is taken
   * is gets the applied move in input.
   * The callBack is performed before the move is actually taken.
   *
   * @param procOnMove a procedure that inputs the move that is applied;
   *                   use this to update a Tabu for instance
   */
  def beforeMove(procOnMove: Move => Unit) = new DoOnMove(n, procBeforeMove = procOnMove)

  /**
   * this combinator attaches a custom code to a given neighborhood.
   * the code is called whenever a move from this neighborhood is taken
   * The callBack is performed after the move is actually taken.
   *
   * @param proc the procedure to execute when the move is taken
   */
  def afterMove(proc: => Unit) = new DoOnMove(n, procAfterMove = (_) => proc)

  /**
   * this combinator attaches a custom code to a given neighborhood.
   * the code is called whenever a move from this neighborhood is taken
   * is gets the applied move in input.
   * The callBack is performed after the move is actually taken.
   *
   * @param procOnMove a procedure that inputs the move that is applied;
   *                   use this to update a Tabu for instance
   */
  def afterMoveOnMove(procOnMove: Move => Unit) = new DoOnMove(n, procAfterMove = procOnMove)

  /**
   * This combinator create a frame that draw the evolution curve of the objective function.
   * The drawn curve possess a scrollbar on the right that allow the user to decrease or
   * increase the number of value displayed.
   *
   * @param obj the objective function
   * @param stopWatch the StopWatch attached to the Test
   * @param withZoom if true the Zoom thread will be used in stead of the AdjustMaxValues trait
   * @param neighborhoodColors a function used to defined the color of each neighborhood encountered during the search
   *                           the default function use the generateColorFromHash method of the ColorGenerator object.
   * @author fabian.germeau@student.vinci.be
   */
  def showObjectiveFunction(obj: Objective,
                            stopWatch: StopWatch = new StopWatch {startWatch()},
                            withZoom:Boolean = false,
                            neighborhoodColors: String => Color = (name:String)=>{ColorGenerator.generateColorFromHash(name.hashCode)}
                             ) = new ShowObjectiveFunction(n,obj,stopWatch,withZoom,neighborhoodColors)

  /**
   * this combinator attaches a custom code to a given neighborhood.
   * the code is called whenever a move from this neighborhood is taken for the first time.
   * notice that this neighborhood is reset, so first time can occur several times.
   *
   * @param proc the procedure to call on one first move that is performed from this neighborhood
   */
  def onFirstMove(proc: => Unit) = new DoOnFirstMove(n, () => proc)

  def saveBest(o: Objective) = new SaveBest(n, o)

  def saveBestAndRestoreOnExhaust(obj: Objective) = (new SaveBest(n, obj)) restoreBestOnExhaust

  /**
   * retries the move before concluding to noMove can be found
   *
   * @param cond condition that takes the number of consecutive NoMoveFound, and says if we should try again returns true if yes, false otherwise
   *             by default, we allow a single retry.
   */
  def retry(cond: Int => Boolean = (n: Int) => n <= 1) = new Retry(n, cond)

  /**
   * to prevent resetting the internal state of this neighborhood
   *
   * @return
   */
  def noReset: Neighborhood = new NoReset(n)

  /**
   * defines a name wor this (composite) neighborhood
   * this will be used as prefix for each move returned by this neighborhood (the original name will still exist)
   * use this for debug and documentation purpose only
   *
   * @param name the name that will prefix all returned moves, used for verbosities
   * @return
   */
  def name(name: String) = new Name(n, name)

  /**
   * tis combinator overrides the acceptance criterion given to the whole neighborhood
   * this can be necessary if you have a neighborhood with some phases only including simulated annealing
   *
   * @param overridingAcceptanceCriterion the acceptance criterion that is used instead of the one given to the overall sear
   */
  def withAcceptanceCriterion(overridingAcceptanceCriterion: (Int, Int) => Boolean) = new WithAcceptanceCriterion(n, overridingAcceptanceCriterion)

  /**
   * this combinator overrides accepts all moves (this is the withAcceptanceCriteria, given the fully acceptant criterion
   */
  def acceptAll() = new WithAcceptanceCriterion(n, (_: Int, _: Int) => true)

  /**
   * proposes a round-robin with that.
   * notice that you can chain steps; this will build a round-robin on the whole sequence (although this operation is not associative, so better not use parentheses)
   *
   * @param b
   * @return
   */
  def step(b: Neighborhood) = new RoundRobin(List(n, b))

  /**
   * calls the neighborhood until an improvement over obj is achieved
   * the improvement is "since the last reset"
   *
   * @param minMoves the min number of queries that will be forwarded to a (priority over the improvement)
   * @param maxMove the max number of queries that will be forwarded to a (priority over the improvement)
   * @param obj the obj that is looked for improvement
   * @author renaud.delandtsheer@cetic.be
   */
  def untilImprovement(obj: () => Int, minMoves: Int = 0, maxMove: Int = Int.MaxValue) = new UntilImprovement(n, obj, minMoves, maxMove)

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
  def metropolis(temperature: Int => Float = (it: Int) => 100 / (it + 1), base: Float = 2) = new Metropolis(n, temperature, base)

}