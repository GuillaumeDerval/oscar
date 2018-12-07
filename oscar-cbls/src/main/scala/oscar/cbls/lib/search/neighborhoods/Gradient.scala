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
                           maxSlopeRatio:Int)
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
    println("initialObj" + initialObj)
    while(selectedVarSet.size < maxNbVars && selectVarsIt.hasNext){
      val currentVarIndice = selectVarsIt.next
      startIndiceForHotRestart = currentVarIndice

      println("analyzing " + currentVarIndice )

      if(! (selectedVarSet contains currentVarIndice)){
        val currentVar = vars(currentVarIndice)
        val deltaForVar = variableIndiceToDeltaForGradientDefinition(currentVarIndice)

        val oldVal = currentVar.newValue

        val slope:Double = {
          //valueAbove
          val valueAbove = oldVal + deltaForVar
          if (valueAbove >= currentVar.max) {
            //no point of increasing it; thy decreasing?
            println("no point of increasing it; thy decreasing?")

            val valueBelow = (oldVal - deltaForVar) max currentVar.min
            val objBelow = obj.assignVal(currentVar, valueBelow)
            println("objBelow:" + objBelow)
            (valueBelow - oldVal).toDouble / (objBelow - initialObj).toDouble
          } else {
            val objAbove = obj.assignVal(currentVar, valueAbove)
            println("objAbove:" + objAbove)
            (valueAbove - oldVal).toDouble / (objAbove - initialObj).toDouble
          }
        }

        println("got slope" + slope)

        println("cond1:" + (slope != 0))
        println("cond2:" + (slope < 0 || ((oldVal - deltaForVar) > currentVar.min)))
        println("cond3:" + (slope > 0 || (oldVal + deltaForVar < currentVar.max)))


        if(slope != 0 &&
          (slope < 0 || ((oldVal - deltaForVar) > currentVar.min))
          && (slope > 0 || ((oldVal + deltaForVar) < currentVar.max))){

          println("keep for gradient")
          val bound1 = ((currentVar.max - oldVal) / slope).toInt
          val bound2 = ((currentVar.min - oldVal) / slope).toInt

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
      startIndiceForHotRestart = startIndiceForHotRestart+1
    }

    if(gradientDefinition.isEmpty){
      println("no gradient found")
      return
    }

    def abs(d:Double):Double = if (d < 0) -d else d
    val maxSlope = gradientDefinition.map(s => abs(s.slope)).max

    val minSlope = maxSlope / maxSlopeRatio
    println("before filter:"  + gradientDefinition.length)
    gradientDefinition = gradientDefinition.filter(c => (abs(c.slope) > minSlope))
    println("after filter:"  +gradientDefinition.length)

    println("\t" + gradientDefinition.mkString("\n\t"))

    val minStep = gradientDefinition.map(_.minStep).max
    val maxStep = gradientDefinition.map(_.maxStep).min

    require(minStep < maxStep)
    def evaluateStep(step:Int):Int = {
      this.currentStep = step
      val newObj = obj.assignVal(gradientDefinition.map(component => (component.variable, component.initiValue + (component.slope * step).toInt)))
      evaluateCurrentMoveObjTrueIfSomethingFound(newObj)
      newObj
    }

    //step2: find proper step with numeric method considered
    val (bestStep,newObj) = linearSearch.search(0,initialObj,minStep,maxStep,evaluateStep)
    //we do not consider the value because it is saved through the evaluateStep method.
  }

  override def instantiateCurrentMove(newObj: Int): GradientMove = {
    GradientMove(gradientDefinition, currentStep, newObj, name)
  }
}

case class GradientMove(gradientDefinition : List[GradientComponent], step:Int, override val objAfter:Int, override val neighborhoodName:String = null)
  extends Move(objAfter, neighborhoodName){

  override def commit() {
    for(component <- gradientDefinition) {
      component.variable := component.initiValue + (component.slope * step).toInt
    }
  }

  override def toString: String = {
    neighborhoodNameToString + "GradientMove(" + gradientDefinition.map(component => component.variable + ":=" + (component.initiValue + (component.slope * step).toInt)).mkString(";")  + objToString + ")"
  }

  override def touchedVariables: List[Variable] = gradientDefinition.map(_.variable)
}

