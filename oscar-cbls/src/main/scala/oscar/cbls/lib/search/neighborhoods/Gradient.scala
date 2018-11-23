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

import scala.collection.immutable.SortedSet

case class GradientComponent(variable:CBLSIntVar,
                             initiValue:Int,
                             indice:Int,
                             slope:Double,
                             minStep:Int,
                             maxStep:Int)

case class GradientDescent(vars:Array[CBLSIntVar],
                           name:String = "GradientDescent",
                           maxNbVars:Int,
                           selectVars:Iterable[Int],
                           variableIndiceToDeltaForGradientDefinition:Int => Int,
                           hotRestart:Boolean = true,
                           linearSearch:LinearOptimizer)
  extends EasyNeighborhoodMultiLevel[GradientMove](name) {

  var gradientDefinition:List[GradientComponent] = List.empty
  var currentStep:Int = 0

  var startIndiceForHotRestart:Int = 0

  /**
    * This is the method you must implement and that performs the search of your neighborhood.
    * every time you explore a neighbor, you must perform the calls to notifyMoveExplored or moveRequested(newObj) && submitFoundMove(myMove)){
    * as explained in the documentation of this class
    */
  override def exploreNeighborhood(initialObj: Int): Unit = {
    //step1: interroger le gradient dans toutes les directions de selectedVars
    gradientDefinition = List.empty
    var selectedVarSet:SortedSet[Int] = SortedSet.empty
    currentStep  = 0

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

        val oldVal = currentVar.value

        val slope:Double = {
          //valueAbove
          val valueAbove = oldVal + deltaForVar
          if (valueAbove >= currentVar.max) {
            //no point of increasing it; thy decreasing?
            val valueBelow = (oldVal - deltaForVar) max currentVar.min
            val objBelow = obj.assignVal(currentVar, valueBelow)

            (objBelow - initialObj) / (valueBelow - oldVal)
          } else {
            val objAbove = obj.assignVal(currentVar, valueAbove)

            (objAbove - initialObj) / (valueAbove - oldVal)
          }
        }

        if(slope != 0 &&
          (slope < 0 || (oldVal - deltaForVar) > currentVar.min)
          && (slope > 0 || (oldVal + deltaForVar) < currentVar.max)){

          val (minStep,maxStep) = if(slope>0){
            //minStep =
            (((currentVar.min - oldVal) * slope).toInt,
            //maxStep =
            ((currentVar.max - oldVal) * slope).toInt)
          }else{
            //minStep =
            (((currentVar.max - oldVal) * slope).toInt,
            //maxStep =
            ((currentVar.min - oldVal) * slope).toInt)
          }

          //println("currentVar.max:" + currentVar.max)
          //println("currentVar.min:" + currentVar.min)
          //println("oldVal:" + oldVal)
          //println("slope:" + slope)
          //println("minStep:" + minStep)
          //println("maxStep:" + maxStep)


          gradientDefinition =
            GradientComponent(
              currentVar,
              oldVal,
              currentVarIndice,
              slope,
              minStep,
              maxStep) :: gradientDefinition
          selectedVarSet += currentVarIndice
        }else{
          //println("got partial derivative, stuck against domain range")
        }
      }
    }

    if(selectVarsIt.hasNext){
      startIndiceForHotRestart = selectVarsIt.next
    }else{
      startIndiceForHotRestart = startIndiceForHotRestart+1
    }

    if(gradientDefinition.isEmpty) {
      ////println("no gradient found for " + name)
      return
    }

    //println("gradient found:{")
    //println("\t" + gradientDefinition.mkString("\n\t") + "\n}")

    val minStep = gradientDefinition.map(_.minStep).max
    val maxStep = gradientDefinition.map(_.maxStep).min

    //println("minStep:" + minStep + " maxStep:" + maxStep)
    require(minStep < maxStep)


    def evaluateStep(step: Int): Int = {
      this.currentStep = step
      obj.assignVal(gradientDefinition.map(component => (component.variable, component.initiValue + ((step / component.slope).toInt))))
    }

    //step2: find proper step with numeric method considered
    val (bestStep, newObj) = linearSearch.search(0, initialObj, minStep, maxStep, evaluateStep)


    for (component <- gradientDefinition){
      component.variable := component.initiValue //to be sure that we do not mess up the model
    }

    this.currentStep = bestStep //because this is now that the move is created, possibly
    evaluateCurrentMoveObjTrueIfSomethingFound(newObj)
  }

  override def instantiateCurrentMove(newObj: Int): GradientMove = {
    GradientMove(gradientDefinition, currentStep, newObj, name)
  }
}

case class GradientMove(gradientDefinition : List[GradientComponent], step:Int, override val objAfter:Int, override val neighborhoodName:String = null)
  extends Move(objAfter, neighborhoodName){

  override def commit() {
    for(component <- gradientDefinition) {
      component.variable := component.initiValue + ((step / component.slope).toInt)
    }
  }

  override def toString: String = {
    neighborhoodNameToString + "GradientMove(" + gradientDefinition.map(component => component.variable + ":=" + (component.initiValue + (component.slope * step).toInt)).mkString(";")  + objToString + ")"
  }

  override def touchedVariables: List[Variable] = gradientDefinition.map(_.variable)
}

