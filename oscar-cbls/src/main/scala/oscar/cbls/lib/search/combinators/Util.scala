package oscar.cbls.lib.search.combinators

import java.awt.{Dimension, Color}
import javax.swing.JFrame

import oscar.cbls._
import oscar.cbls.core.search._
import oscar.cbls.util.StopWatch
import oscar.cbls.visual.FunctionGraphic.{AdjustMaxValue, Zoom, ObjFunctionGraphicContainer}

trait UtilityCombinators{
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
  def profile(a:Neighborhood,ignoreInitialObj:Boolean = false) = Profile(a,ignoreInitialObj)
}


/**
 * This combinator create a frame that draw the evolution curve of the objective function.
 * The drawn curve possess a scrollbar on the right that allow the user to decrease or
 * increase the number of value displayed.
 *
 * @param a a neighborhood
 * @param obj the objective function
 * @param stopWatch the StopWatch attached to the Test
 * @param withZoom if true the Zoom thread will be used in stead of the AdjustMaxValues trait
 * @author fabian.germeau@student.vinci.be
 */
class ShowObjectiveFunction(a: Neighborhood, obj: Objective, stopWatch: StopWatch, withZoom:Boolean) extends NeighborhoodCombinator(a){
  //objGraphic is an internal frame that contains the curve itself and visualFrame is a basic frame that contains objGraphic
  val objGraphic = if(withZoom) new ObjFunctionGraphicContainer(dimension = new Dimension(940L,500L)) with Zoom
  else new ObjFunctionGraphicContainer(dimension = new Dimension(960L,540L)) with AdjustMaxValue

  new Thread(objGraphic,"Graphic Thread").start()

  val visualFrame = new JFrame()
  visualFrame.setPreferredSize(new Dimension(960L,540L))
  visualFrame.add(objGraphic)
  visualFrame.pack()
  visualFrame.setVisible(true)

  override def getMove(obj: Objective, initialObj:Long, acceptanceCriteria: (Long, Long) => Boolean): SearchResult ={
    a.getMove(obj, initialObj, acceptanceCriteria) match {
      case m: MoveFound =>
        InstrumentedMove(m.m, null, () => notifyNewObjValue(m.m))
      case x => x
    }
  }

  /*
    After each move we send the new value and time to objGraphic who will register the value
    and then we write the curve
   */
  def notifyNewObjValue(m:Move): Unit ={
    objGraphic.objCurveDatas.synchronized{
      objGraphic.objCurveDatas = (obj.value,stopWatch.getWatch,m.neighborhoodName) :: objGraphic.objCurveDatas
    }
  }
}


/**
 * the purpose of this combinator is to change the name of the neighborhood it is given as parameter.
 * it will add a prefix to all moves sent back by this combinator
 * the only purposes are documentation and debug
 *
 * @param a
 * @param name
 */
class Name(a: Neighborhood, val name: String) extends NeighborhoodCombinator(a) {
  /**
   * @param acceptanceCriterion oldObj,newObj => should the move to the newObj be kept (default is oldObj > newObj)
   *                            beware that a changing criteria might interact unexpectedly with stateful neighborhood combinators
   * @return an improving move
   */
  override def getMove(obj: Objective, initialObj:Long, acceptanceCriterion: (Long, Long) => Boolean): SearchResult = {
    a.getMove(obj, initialObj:Long, acceptanceCriterion) match {
      case NoMoveFound => NoMoveFound
      case MoveFound(m) => NamedMove(m, name)
    }
  }

  override def toString: String = name
}


/**
 * the purpose of this combinator is to change the name of the neighborhood it is given as parameter.
 * it will add a prefix to all moves sent back by this combinator
 * the only purposes are documentation and debug
 *
 * @param a
 * @param name
 */
class ChainableName[MoveType <: Move](a: Neighborhood with SupportForAndThenChaining[MoveType], val name: String)
  extends NeighborhoodCombinator(a) with SupportForAndThenChaining[MoveType]{
  /**
   * @param acceptanceCriterion oldObj,newObj => should the move to the newObj be kept (default is oldObj > newObj)
   *                            beware that a changing criteria might interact unexpectedly with stateful neighborhood combinators
   * @return an improving move
   */
  override def getMove(obj: Objective, initialObj:Long, acceptanceCriterion: (Long, Long) => Boolean): SearchResult = {
    a.getMove(obj, initialObj:Long, acceptanceCriterion) match {
      case NoMoveFound => NoMoveFound
      case MoveFound(m) => NamedMove(m, name)
    }
  }

  override def toString: String = name

  override def instantiateCurrentMove(newObj : Long) : MoveType = a.instantiateCurrentMove(newObj)
}

/**
 * tis combinator overrides the acceptance criterion given to the whole neighborhood
 * this can be necessary if you have a neighborhood with some phases only including simulated annealing
 * notice that the actual acceptance criteria is the one that you give,
 * with a slight modification: it will reject moves that lead to MaxInt, except if we are already at MaxInt.
 * Since MaxInt is used to represent that a strong constraint is violated, we cannot tolerate such moves at all.
 *
 * @param a the neighborhood
 * @param overridingAcceptanceCriterion the acceptance criterion that is used instead of the one given to the overall search
 *                                      with the addition that moves leading to MaxInt will be rejected anyway, except if we are already at MaxInt
 *
 */
class WithAcceptanceCriterion(a: Neighborhood, overridingAcceptanceCriterion: (Long, Long) => Boolean) extends NeighborhoodCombinator(a) {
  /**
   * @param acceptanceCriterion this criterion is not considered by this combinator.
   * @return an improving move
   */
  override def getMove(obj: Objective, initialObj:Long, acceptanceCriterion: (Long, Long) => Boolean): SearchResult
  = a.getMove(obj,initialObj:Long,
    (a, b) => (a == Long.MaxValue || b != Long.MaxValue) && overridingAcceptanceCriterion(a, b))
}


/**
 * Forces the use of a given objective function.
 * this overrides the one that you might pass in the higher level
 *
 * @param a the combined neighborhood
 * @param overridingObjective the objective to use instead of the given one
 */
class OverrideObjective(a: Neighborhood, overridingObjective: Objective) extends NeighborhoodCombinator(a) {
  /**
   * the method that returns a move from the neighborhood.
   * The returned move should typically be accepted by the acceptance criterion over the objective function.
   * Some neighborhoods are actually jumps, so that they might violate this basic rule however.
   *
   * @param obj the objective function. notice that it is actually a function. if you have an [[oscar.cbls.core.objective.Objective]] there is an implicit conversion available
   * @param acceptanceCriterion
   * @return
   */
  override def getMove(obj: Objective, initialObj:Long, acceptanceCriterion: (Long, Long) => Boolean): SearchResult =
    getMove(overridingObjective, overridingObjective.value,acceptanceCriterion)
}


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
case class Profile(a:Neighborhood,ignoreInitialObj:Boolean = false) extends NeighborhoodCombinator(a){

  var nbCalls = 0L
  var nbFound = 0L
  var totalGain:Double = 0L
  var totalTimeSpentMoveFound:Long = 0L
  var totalTimeSpentNoMoveFound:Long = 0L

  def totalTimeSpent = totalTimeSpentMoveFound + totalTimeSpentNoMoveFound

  override def resetStatistics(){
    resetThisStatistics()
    super.resetStatistics()
  }

  def resetThisStatistics() {
    nbCalls = 0L
    nbFound = 0L
    totalGain = 0L
    totalTimeSpentMoveFound = 0L
    totalTimeSpentNoMoveFound = 0L
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
  override def getMove(obj: Objective, initialObj:Long, acceptanceCriterion: (Long, Long) => Boolean): SearchResult = {

    nbCalls += 1L
    val startTime = System.nanoTime()

    a.getMove(obj, initialObj:Long, acceptanceCriterion) match {
      case NoMoveFound =>
        totalTimeSpentNoMoveFound += (System.nanoTime() - startTime) / 1000000L
        NoMoveFound
      case m: MoveFound =>
        totalTimeSpentMoveFound += (System.nanoTime() - startTime) / 1000000L
        nbFound += 1L
        if (!ignoreInitialObj || nbCalls > 1L) totalGain += initialObj - m.objAfter
        m
    }
  }

  def gainPerCall:String = if(nbCalls ==0L) "NA" else "" + (totalGain / nbCalls).toInt
  def callDuration:String = if(nbCalls == 0L ) "NA" else "" + (totalTimeSpent / nbCalls).toInt
  //gain in obj/s
  def slope:String = if(totalTimeSpent == 0L) "NA" else "" + (1000L * totalGain.toDouble / totalTimeSpent.toDouble).toInt

  def avgTimeSpendNoMove:String = if(nbCalls - nbFound == 0L) "NA" else "" + (totalTimeSpentNoMoveFound / (nbCalls - nbFound))
  def avgTimeSpendMove:String = if(nbFound == 0L) "NA" else "" + (totalTimeSpentMoveFound / nbFound)
  def waistedTime:String = if(nbCalls - nbFound == 0L) "NA" else "" + (totalTimeSpentNoMoveFound / (nbCalls - nbFound))

  override def collectProfilingStatistics: List[String] =
    collectThisProfileStatistics :: super.collectProfilingStatistics

  def collectThisProfileStatistics:String =
    padToLength("" + a,31L) + " " +
      padToLength("" + nbCalls,6L) + " " +
      padToLength("" + nbFound,6L) + " " +
      padToLength("" + totalGain.toInt,8L) + " " +
      padToLength("" + totalTimeSpent,12L) + " " +
      padToLength("" + gainPerCall,8L) + " " +
      padToLength("" + callDuration,12L)+ " " +
      padToLength("" + slope,11L)+ " " +
      padToLength("" + avgTimeSpendNoMove,13L)+ " " +
      padToLength("" + avgTimeSpendMove,12L)+ " " +
      totalTimeSpentNoMoveFound

  private def padToLength(s: String, l: Long) = {
    val extended = s + nStrings(l+1L, " ")
    val nextchar = extended.substring(l+1L, l+1L)
    if(nextchar equals " "){
      extended.substring(0L, l-1L) + "§"
    }else{
      extended.substring(0L, l)
    }
  }
  private def nStrings(n: Long, s: String): String = if (n <= 0L) "" else s + nStrings(n - 1L, s)

  //  override def toString: String = "Statistics(" + a + " nbCalls:" + nbCalls + " nbFound:" + nbFound + " totalGain:" + totalGain + " totalTimeSpent " + totalTimeSpent + " ms timeSpendWithMove:" + totalTimeSpentMoveFound + " ms totalTimeSpentNoMoveFound " + totalTimeSpentNoMoveFound + " ms)"
  override def toString: String = "Profile(" + a + ")"

  def slopeOrZero:Long = if(totalTimeSpent == 0L) 0L else ((100L * totalGain) / totalTimeSpent).toInt

  def slopeForCombinators(defaultIfNoCall:Long = Long.MaxValue):Long =  if(totalTimeSpent == 0L) defaultIfNoCall else ((1000L * totalGain) / totalTimeSpent).toInt
}

object Profile{
  private def padToLength(s: String, l: Long) = (s + nStrings(l, " ")).substring(0L, l)
  private def nStrings(n: Long, s: String): String = if (n <= 0L) "" else s + nStrings(n - 1L, s)
  def statisticsHeader: String = padToLength("Neighborhood",30L) + "  calls  found  sumGain  sumTime(ms)  avgGain  avgTime(ms)  slope(-/s)  avgTimeNoMove avgTimeMove  wastedTime"
  def selectedStatisticInfo(i:Iterable[Profile]) = {
    (statisticsHeader :: i.toList.map(_.collectThisProfileStatistics)).mkString("\n")
  }
}


case class NoReset(a: Neighborhood) extends NeighborhoodCombinator(a) {
  override def getMove(obj: Objective, initialObj:Long, acceptanceCriteria: (Long, Long) => Boolean) =
    a.getMove(obj, initialObj:Long, acceptanceCriteria)

  //this resets the internal state of the move combinators
  override def reset() {}
}


/**
 * @author renaud.delandtsheer@cetic.be
 */
class ResetOnExhausted(a: Neighborhood) extends NeighborhoodCombinator(a) {
  override def getMove(obj: Objective, initialObj:Long, acceptanceCriteria: (Long, Long) => Boolean): SearchResult = {
    a.getMove(obj, initialObj:Long, acceptanceCriteria) match {
      case NoMoveFound =>
        a.reset()
        a.getMove(obj, initialObj:Long, acceptanceCriteria)
      case m: MoveFound => m
    }
  }
}
