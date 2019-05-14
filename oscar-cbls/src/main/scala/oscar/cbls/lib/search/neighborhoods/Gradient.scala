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
                             initValue:Long,
                             indice:Int,
                             slope:Double,
                             minStep:Long,
                             maxStep:Long){
  require(slope != 0, "zero slope!")
  require(!slope.isInfinity, "infinite slope") //handles both + and - infty
  require(!slope.isNaN,"NAN slope")

  override def toString: String =
    "GradientComponent(variable:" + variable + "," +
      "initValue:" + initValue + "," +
      "indice:" + indice + "," +
      "slope:" + slope + ","+
      "maxStep:" + maxStep + "," +
      "minStep:" + minStep + ")"

  def takeStep(step:Long): Unit ={
    variable := initValue + (step / slope).toLong
  }

  def rollBack(): Unit ={
    variable := initValue
  }
}

case class GradientDescent(vars:Array[CBLSIntVar],
                           name:String = "GradientDescent",
                           maxNbVars:Int = Integer.MAX_VALUE,
                           selectVars:Iterable[Long],
                           variableIndiceToDeltaForGradientDefinition:Long => Long,
                           hotRestart:Boolean = true,
                           linearSearch:LinearOptimizer,
                           trySubgradient:Boolean = false)
  extends EasyNeighborhoodMultiLevel[GradientMove](name) {

  var gradientDefinition:List[GradientComponent] = List.empty
  var currentStep:Long = 0

  var startIndiceForHotRestart:Long = 0

  /**
    * This is the method you must implement and that performs the search of your neighborhood.
    * every time you explore a neighbor, you must perform the calls to notifyMoveExplored or moveRequested(newObj) && submitFoundMove(myMove)){
    * as explained in the documentation of this class
    */
  override def exploreNeighborhood(initialObj: Long): Unit = {
    //step1: interroger le gradient dans toutes les directions de selectedVars
    gradientDefinition = List.empty
    var selectedVarSet:SortedSet[Long] = SortedSet.empty
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

        val oldVal = currentVar.newValue

        val slope:Double = {
          //valueAbove
          val valueAbove = oldVal + deltaForVar
          if (valueAbove >= currentVar.max) {
            //no point of increasing it; thy decreasing?
            val valueBelow = (oldVal - deltaForVar) max currentVar.min
            val objBelow = obj.assignVal(currentVar, valueBelow)
            (objBelow - initialObj).toDouble / (valueBelow - oldVal).toDouble
          } else {
            val objAbove = obj.assignVal(currentVar, valueAbove)
            (objAbove - initialObj).toDouble / (valueAbove - oldVal).toDouble
          }
        }

        if(slope != 0L &&
          (slope < 0L || ((oldVal - deltaForVar) > currentVar.min))
          && (slope > 0L || ((oldVal + deltaForVar) < currentVar.max))){

          val bound1 = ((currentVar.max - oldVal) * slope).toLong
          val bound2 = ((currentVar.min - oldVal) * slope).toLong

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

     // println("\t" + gradientDefinition.mkString("\n\t"))

      val minStep = gradientDefinition.map(_.minStep).max
      val maxStep = gradientDefinition.map(_.maxStep).min

      require(minStep < maxStep, "minStep:" + minStep + " should be < maxStep:" + maxStep)

      def evaluateStep(step:Long):Long = {
        this.currentStep = step

        for(component <- gradientDefinition){
          component.takeStep(step)
        }

        val newObj = obj.value

        for(component <- gradientDefinition){
          component.rollBack()
        }

        evaluateCurrentMoveObjTrueIfSomethingFound(newObj)
        newObj
      }

      //step2: find proper step with numeric method considered
      val (bestStep,newObj) = linearSearch.search(0,initialObj,minStep,maxStep,evaluateStep)
      //we do not consider the value because it is saved through the evaluateStep method.

      if(moveHasBeenFound){
        return
      } else if (trySubgradient){

        //we remove the smallest slope because the absence of move is possibly due to numerical artifacts

        def abs(d:Double):Double = if (d < 0) -d else d
        val flattestSlope= gradientDefinition.map(s => abs(s.slope)).min

        gradientDefinition = gradientDefinition.filter(c => abs(c.slope) != flattestSlope)
      }else{
        return
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
    for(component <- gradientDefinition){
      component.takeStep(step)
    }
  }

  override def toString: String = {
    neighborhoodNameToString + "GradientMove(" + gradientDefinition.map(component => component.variable.toString + ":=" + (component.initValue + (step / component.slope).toLong)).mkString(";")  + objToString + ")"
  }

  override def touchedVariables: List[Variable] = gradientDefinition.map(_.variable)
}

