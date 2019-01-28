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

import oscar.cbls.algo.search.HotRestart
import oscar.cbls.core.computation.{CBLSIntVar, Variable}
import oscar.cbls.core.search.{EasyNeighborhoodMultiLevel, Move}
import oscar.cbls._

import scala.collection.immutable.SortedSet

case class GradientComponent(variable:CBLSIntVar,
                             initiValue:Long,
                             indice:Long,
                             slope:Double,
                             minStep:Long,
                             maxStep:Long){
  override def toString: String =
    "GradientComponent(variable:" + variable + "," +
      "initiValue:" + initiValue + "," +
      "indice:" + indice + "," +
      "slope:" + slope+ ","+
      "maxStep:" + maxStep + "," +
      "minStep:" + minStep + ")"
}

case class GradientDescent(vars:Array[CBLSIntVar],
                           name:String = "GradientDescent",
                           maxNbVars:Long = Integer.MAX_VALUE,
                           selectVars:Iterable[Long],
                           variableIndiceToDeltaForGradientDefinition:Long => Long,
                           hotRestart:Boolean = true,
                           linearSearch:LinearOptimizer,
                           maxSlopeRatio:Long,
                           trySubgradient:Boolean)
  extends EasyNeighborhoodMultiLevel[GradientMove](name) {

  var gradientDefinition:List[GradientComponent] = List.empty
  var currentStep:Long = 0L

  var startIndiceForHotRestart:Long = 0L

  /**
    * This is the method you must implement and that performs the search of your neighborhood.
    * every time you explore a neighbor, you must perform the calls to notifyMoveExplored or moveRequested(newObj) && submitFoundMove(myMove)){
    * as explained in the documentation of this class
    */
  override def exploreNeighborhood(initialObj: Long): Unit = {
    //step1: interroger le gradient dans toutes les directions de selectedVars
    gradientDefinition = List.empty
    var selectedVarSet:SortedSet[Long] = SortedSet.empty
    currentStep  = 0L

    val selectVarsWithHotRestart =
      if (hotRestart) HotRestart(selectVars, startIndiceForHotRestart)
      else selectVars

    val selectVarsIt = selectVarsWithHotRestart.iterator

    while(selectedVarSet.size < maxNbVars && selectVarsIt.hasNext){
      val currentVarIndice = selectVarsIt.next
      startIndiceForHotRestart = currentVarIndice

      if(! (selectedVarSet contains currentVarIndice)){
        val currentVar = vars(currentVarIndice)
        val deltaForVar = variableIndiceToDeltaForGradientDefinition(currentVarIndice)

        val oldVal = currentVar.newValue

        val slope:Double = {
          //valueAbove
          val valueAbove = oldVal + deltaForVar
          if (valueAbove >= currentVar.max) {
            //no point of increasing it; thy decreasing?
            val valueBelow = (oldVal - deltaForVar) max currentVar.min
            val objBelow = obj.assignVal(currentVar, valueBelow)
            (valueBelow - oldVal).toDouble / (objBelow - initialObj).toDouble
          } else {
            val objAbove = obj.assignVal(currentVar, valueAbove)
            (valueAbove - oldVal).toDouble / (objAbove - initialObj).toDouble
          }
        }

        if(slope != 0L &&
          (slope < 0L || ((oldVal - deltaForVar) > currentVar.min))
          && (slope > 0L || ((oldVal + deltaForVar) < currentVar.max))){

          val bound1 = ((currentVar.max - oldVal) / slope).toLong
          val bound2 = ((currentVar.min - oldVal) / slope).toLong

          val (minStep,maxStep) = if (bound1 < bound2) (bound1,bound2) else (bound2,bound1)

          gradientDefinition =
            GradientComponent(
              currentVar,
              oldVal,
              currentVarIndice,
              slope,
              minStep,
              maxStep) :: gradientDefinition
          selectedVarSet += currentVarIndice
        }
      }
    }

    if(selectVarsIt.hasNext){
      startIndiceForHotRestart = selectVarsIt.next
    }else{
      startIndiceForHotRestart = startIndiceForHotRestart+1L
    }

    while(gradientDefinition.nonEmpty){

      //println("\t" + gradientDefinition.mkString("\n\t"))

      val minStep = gradientDefinition.map(_.minStep).max
      val maxStep = gradientDefinition.map(_.maxStep).min

      require(minStep < maxStep)
      def evaluateStep(step:Long):Long = {
        this.currentStep = step
        val newObj = obj.assignVal(gradientDefinition.map(component => (component.variable, component.initiValue + (component.slope * step).toLong)))
        evaluateCurrentMoveObjTrueIfSomethingFound(newObj)
        newObj
      }

      //step2: find proper step with numeric method considered
      val (bestStep,newObj) = linearSearch.search(0L,initialObj,minStep,maxStep,evaluateStep)
      //we do not consider the value because it is saved through the evaluateStep method.

      if(moveHasBeenFound){
        return
      } else if (trySubgradient){

        //we remove the steepest slope because the absence of move is possibly due to numerical artifacts

        def abs(d:Double):Double = if (d < 0L) -d else d
        val maxSlope = gradientDefinition.map(s => abs(s.slope)).max

        gradientDefinition = gradientDefinition.filter(c => abs(c.slope) != maxSlope)
        //println("prune gradient")
      }
    }
  }

  override def instantiateCurrentMove(newObj: Long): GradientMove = {
    GradientMove(gradientDefinition, currentStep, newObj, name)
  }
}

case class GradientMove(gradientDefinition : List[GradientComponent], step:Long, override val objAfter:Long, override val neighborhoodName:String = null)
  extends Move(objAfter, neighborhoodName){

  override def commit() {
    for(component <- gradientDefinition) {
      component.variable := component.initiValue + (component.slope * step).toLong
    }
  }

  override def toString: String = {
    neighborhoodNameToString + "GradientMove(" + gradientDefinition.map(component => component.variable.toString + ":=" + (component.initiValue + (component.slope * step).toLong)).mkString(";")  + objToString + ")"
  }

  override def touchedVariables: List[Variable] = gradientDefinition.map(_.variable)
}

