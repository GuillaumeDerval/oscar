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

package oscar.cbls.lib.search.neighborhoods

import oscar.cbls._
import oscar.cbls.algo.search.HotRestart
import oscar.cbls.core.computation.{CBLSIntVar, Variable}
import oscar.cbls.core.search.{EasyNeighborhoodMultiLevel, First, LoopBehavior, Move}


case class TransferNeighborhood(vars:Array[CBLSIntVar],
                                name:String = "TransferNeighborhood",
                                searchZone1:()=>Iterable[Long] = null,
                                searchZone2:() => (Long,Long)=>Iterable[Long] = null,
                                searchZoneForDelta:() => (Long,Long) => (Long,Long) => LinearOptimizer, //donne des delta à essayer (TOTO: faire un enwton raphson ou regula falsi ou dichotomoe ici!!!
                                symmetryCanBeBrokenOnIndices:Boolean = true,
                                selectFirstVariableBehavior:LoopBehavior = First(),
                                selectSecondVariableBehavior:LoopBehavior = First(),
                                hotRestart:Boolean = true)
  extends EasyNeighborhoodMultiLevel[TransferMove](name){

  //the indice to start with for the exploration
  var firstVarIndice:Long = 0L
  var firstVar:CBLSIntVar = null

  var secondVarIndice:Long = -1L
  var secondVar:CBLSIntVar = null

  var delta:Long = 0L

  override def exploreNeighborhood(initialObj: Long){

    val firstIterationSchemeZone =
      if (searchZone1 == null) {
        if (hotRestart) {
          if (firstVarIndice >= vars.length) firstVarIndice = 0L
          vars.indices startBy firstVarIndice
        } else vars.indices
      } else if (hotRestart) HotRestart(searchZone1(), firstVarIndice) else searchZone1()

    val searchZone2ForThisSearch = if (searchZone2 == null) null else searchZone2()
    val searchZoneForDeltaL1 = searchZoneForDelta()

    val (iIterator,notifyFound1) = selectFirstVariableBehavior.toIterator(firstIterationSchemeZone)
    while (iIterator.hasNext) {
      firstVarIndice = iIterator.next()

      firstVar = vars(firstVarIndice)
      val oldValOfFirstVar = firstVar.newValue

      val secondIterationSchemeZone = if (searchZone2ForThisSearch == null) vars.indices else searchZone2ForThisSearch(firstVarIndice,oldValOfFirstVar)
      val searchZoneForDeltaL2 = searchZoneForDeltaL1(firstVarIndice,oldValOfFirstVar)

      val (jIterator,notifyFound2) = selectSecondVariableBehavior.toIterator(secondIterationSchemeZone)
      while (jIterator.hasNext) {
        secondVarIndice = jIterator.next()
        secondVar = vars(secondVarIndice)
        val oldValOfSecondVar = secondVar.newValue

        if ((!symmetryCanBeBrokenOnIndices || firstVarIndice < secondVarIndice) //we break symmetry on variables
          && firstVarIndice != secondVarIndice
          && secondVar.domain.contains(oldValOfFirstVar)
          && firstVar.domain.contains(oldValOfSecondVar)) {

          this.secondVar = secondVar

          val iterationOnDelta:LinearOptimizer = searchZoneForDeltaL2(secondVarIndice,oldValOfSecondVar)

          //TODO: il faut cadrer dans le domaine des variables!!

          def evaluate(delta:Long): Long ={
            val newObj = obj.assignVal(Seq((firstVar,firstVar.value + delta),(secondVar,secondVar.value - delta)))
            newObj
          }
          val minValueForDelta = (secondVar.min - oldValOfSecondVar) max (oldValOfFirstVar - firstVar.max)
          val maxValueForDelta = (secondVar.max - oldValOfSecondVar) min (oldValOfFirstVar - firstVar.min)

          val (bestDelta,objForDelta) = iterationOnDelta.search(0L, initialObj, minValueForDelta, maxValueForDelta, evaluate)

          this.delta = bestDelta

          if (evaluateCurrentMoveObjTrueIfSomethingFound(objForDelta)) {
            notifyFound1()
            notifyFound2()
          }
        }
      }
    }
    firstVarIndice = firstVarIndice +1L
    secondVarIndice = -1L
  }


  override def instantiateCurrentMove(newObj: Long) =
    TransferMove(firstVar, secondVar, delta:Long, firstVarIndice,secondVarIndice,newObj, name)

  //this resets the internal state of the Neighborhood
  override def reset(): Unit = {
    firstVarIndice = 0L
  }
}


/** standard move that swaps the value of two CBLSIntVar
  *
  * @param firstVar the variable
  * @param secondVar the other variable
  * @param objAfter the objective after this assignation will be performed
  * @param neighborhoodName a string describing the neighborhood hat found the move (for debug purposes)
  * @author renaud.delandtsheer@cetic.be
  */
case class TransferMove(firstVar:CBLSIntVar, secondVar:CBLSIntVar, delta:Long, idI:Long, idJ:Long, override val objAfter:Long, override val neighborhoodName:String = null)
  extends Move(objAfter, neighborhoodName){

  override def commit() {
    firstVar :+= delta
    secondVar :-= delta
  }

  override def toString: String  = {
    neighborhoodNameToString + "Transfer(" + firstVar + " :+= " + delta + " "  + secondVar + ":-=" + delta + objToString + ")"
  }

  override def touchedVariables: List[Variable] = List(firstVar,secondVar)
}
