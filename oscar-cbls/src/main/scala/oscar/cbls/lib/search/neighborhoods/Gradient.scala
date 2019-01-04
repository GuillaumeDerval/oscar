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
                             maxStep:Int){
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
                           maxNbVars:Int = Integer.MAX_VALUE,
                           selectVars:Iterable[Int],
                           variableIndiceToDeltaForGradientDefinition:Int => Int,
                           hotRestart:Boolean = true,
                           linearSearch:LinearOptimizer,
                           trySubgradient:Boolean = false)
  extends EasyNeighborhoodMultiLevel[GradientMove](name) {

  var gradientDefinition: List[GradientComponent] = List.empty
  var currentStep: Int = 0

  var startIndiceForHotRestart: Int = 0

  /**
    * This is the method you must implement and that performs the search of your neighborhood.
    * every time you explore a neighbor, you must perform the calls to notifyMoveExplored or moveRequested(newObj) && submitFoundMove(myMove)){
    * as explained in the documentation of this class
    */
  override def exploreNeighborhood(initialObj: Int): Unit = {
    //println("start gradient")
    //step1: interroger le gradient dans toutes les directions de selectedVars
    gradientDefinition = List.empty
    var selectedVarSet: SortedSet[Int] = SortedSet.empty
    currentStep = 0

    val selectVarsWithHotRestart =
      if (hotRestart) HotRestart(selectVars, startIndiceForHotRestart)
      else selectVars

    val selectVarsIt = selectVarsWithHotRestart.iterator

    while (selectedVarSet.size < maxNbVars && selectVarsIt.hasNext) {
      val currentVarIndice = selectVarsIt.next
      startIndiceForHotRestart = currentVarIndice

      if (!(selectedVarSet contains currentVarIndice)) {
        val currentVar = vars(currentVarIndice)
        val deltaForVar = variableIndiceToDeltaForGradientDefinition(currentVarIndice)

        val oldVal = currentVar.newValue

        val slope: Double = {
          //valueAbove
          val valueAbove = oldVal + deltaForVar
          if (valueAbove >= currentVar.max) {
            //no point of increasing it; thy decreasing?
            val valueBelow = (oldVal - deltaForVar) max currentVar.min
            val objBelow = evaluateAssign(currentVar, valueBelow)
            (objBelow - initialObj).toDouble / (valueBelow - oldVal).toDouble
          } else {
            val objAbove = evaluateAssign(currentVar, valueAbove)
            (objAbove - initialObj).toDouble / (valueAbove - oldVal).toDouble
          }
        }

        //if(slope == 0){
        //  println("null slope")
        //}

        if (slope != 0 &&
          (slope < 0 || ((oldVal - deltaForVar) > currentVar.min))
          && (slope > 0 || ((oldVal + deltaForVar) < currentVar.max))) {

          val bound1 = ((currentVar.max - oldVal).toDouble / slope).toInt
          //println("currentVar.max:" + currentVar.max + " oldVal:" + oldVal + " slope:" + slope)
          val bound2 = ((currentVar.min - oldVal).toDouble / slope).toInt

          val (minStep, maxStep) = if (bound1 < bound2) (bound1, bound2) else (bound2, bound1)

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

    //normalizing gradient definition
    val gradientNorm = math.sqrt(gradientDefinition.map(gd => (gd.slope * gd.slope)).sum)

    val targetGradientNorm = 1
    val multFactor = targetGradientNorm / gradientNorm

    gradientDefinition = gradientDefinition.map(gr => {
      val newSope = gr.slope * multFactor
      val oldVal = gr.initiValue
      val currentVar = gr.variable
      val bound1 = ((currentVar.max - oldVal).toDouble / newSope).toInt
      //println("currentVar.max:" + currentVar.max + " oldVal:" + oldVal + " newSlope:" + newSope)
      val bound2 = ((currentVar.min - oldVal).toDouble / newSope).toInt

      val (minStep, maxStep) = if (bound1 < bound2) (bound1, bound2) else (bound2, bound1)
      gr.copy(slope = newSope, minStep = minStep, maxStep = maxStep)
    })


    if (selectVarsIt.hasNext) {
      startIndiceForHotRestart = selectVarsIt.next
    } else {
      startIndiceForHotRestart = startIndiceForHotRestart + 1
    }

    while (gradientDefinition.nonEmpty) {

      val minStep = gradientDefinition.map(_.minStep).max
      val maxStep = gradientDefinition.map(_.maxStep).min

      require(minStep < maxStep, "minStep:" + minStep + " maxStep:" + maxStep + " gradient: " + gradientDefinition)


      //step2: find proper step with numeric method considered
      val (bestStep, newObj) = linearSearch.search(0, initialObj, minStep, maxStep, evaluateStep)
      //we do not consider the value because it is saved through the evaluateStep method.

      if (!moveHasBeenFound && trySubgradient) {
        //we remove the steepest slope
        def abs(d: Double): Double = if (d < 0) -d else d

        val minSlope = gradientDefinition.map(s => abs(s.slope)).min

        gradientDefinition = gradientDefinition.filter(c => abs(c.slope) != minSlope)
      } else {
        //either a move was found, or no move was found and we do not want subgradient
        return
      }
    }
  }

  var evaluatingGradient: Boolean = false

  def evaluateStep(step: Int): Int = {
    this.currentStep = step
    evaluatingGradient = true
    val newObj = obj.assignVal(gradientDefinition.map(component => (component.variable, component.initiValue + (step * component.slope).toInt)))
    evaluateCurrentMoveObjTrueIfSomethingFound(newObj)
    newObj
  }

  var currentVar: CBLSIntVar = null
  var newValue: Int = Int.MinValue

  def evaluateAssign(variable: CBLSIntVar, newValue: Int): Int = {
    evaluatingGradient = false
    this.currentVar = variable
    this.newValue = newValue
    val newObj = obj.assignVal(variable, newValue)
    evaluateCurrentMoveObjTrueIfSomethingFound(newObj)
    newObj
  }

  override def instantiateCurrentMove(newObj: Int): GradientMove = {
    if (evaluatingGradient) {
      new FullGradientMove(gradientDefinition, currentStep, newObj, name)
    } else {
      new PartialGradient(currentVar, newValue, newObj, name)
    }
  }
}

abstract sealed class GradientMove(objAfter:Int, override val neighborhoodName:String = null)
  extends Move(objAfter, neighborhoodName){}

class PartialGradient(variable:CBLSIntVar,newValue:Int,objAfter:Int, override val neighborhoodName:String = null)
  extends GradientMove(objAfter,neighborhoodName){
  /** to actually take the move */
  override def commit(): Unit = {
    variable := newValue
  }
}


class FullGradientMove(gradientDefinition : List[GradientComponent], step:Int, override val objAfter:Int, override val neighborhoodName:String = null)
  extends GradientMove(objAfter, neighborhoodName){

  override def commit() {
    for(component <- gradientDefinition) {
      component.variable := component.initiValue + (step * component.slope).toInt
    }
  }

  override def toString: String = {
    neighborhoodNameToString + "GradientMove(" + gradientDefinition.map(component => component.variable + ":=" + (component.initiValue + (step * component.slope).toInt)).mkString(";")  + objToString + ")"
  }

  override def touchedVariables: List[Variable] = gradientDefinition.map(_.variable)
}

