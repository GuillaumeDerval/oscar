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
                                searchZone1:()=>Iterable[Int] = null,
                                searchZone2:() => (Int,Int)=>Iterable[Int] = null,
                                searchZoneForDelta:() => (Int,Int) => (Int,Int) => LinearOptimizer, //donne des delta à essayer (TOTO: faire un enwton raphson ou regula falsi ou dichotomoe ici!!!
                                symmetryCanBeBrokenOnIndices:Boolean = true,
                                selectFirstVariableBehavior:LoopBehavior = First(),
                                selectSecondVariableBehavior:LoopBehavior = First(),
                                hotRestart:Boolean = true)
  extends EasyNeighborhoodMultiLevel[TransferMove](name){

  //the indice to start with for the exploration
  var firstVarIndice:Int = 0
  var firstVar:CBLSIntVar = null

  var secondVarIndice:Int = -1
  var secondVar:CBLSIntVar = null

  var delta:Int = 0

  override def exploreNeighborhood(initialObj: Int){

    val firstIterationSchemeZone =
      if (searchZone1 == null) {
        if (hotRestart) {
          if (firstVarIndice >= vars.length) firstVarIndice = 0
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

          def evaluate(delta:Int): Int ={
            this.delta = delta
            val newObj = obj.swapVal(firstVar, secondVar)
            if (evaluateCurrentMoveObjTrueIfSomethingFound(newObj)) {
              notifyFound1()
              notifyFound2()
            }
            newObj
          }
          val minValueForDelta = (secondVar.min - oldValOfSecondVar) min (oldValOfFirstVar - firstVar.max)
          val maxValueForDelta = (secondVar.max - oldValOfSecondVar) max (oldValOfFirstVar - firstVar.min)

          iterationOnDelta.search(0,initialObj,minValueForDelta,maxValueForDelta,evaluate)
        }
      }
    }
    firstVarIndice = firstVarIndice +1
    secondVarIndice = -1
  }


  override def instantiateCurrentMove(newObj: Int) =
    TransferMove(firstVar, secondVar, delta:Int, firstVarIndice,secondVarIndice,newObj, name)

  //this resets the internal state of the Neighborhood
  override def reset(): Unit = {
    firstVarIndice = 0
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
case class TransferMove(firstVar:CBLSIntVar, secondVar:CBLSIntVar, delta:Int, idI:Int, idJ:Int, override val objAfter:Int, override val neighborhoodName:String = null)
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
