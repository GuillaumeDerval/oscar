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
                             slope:Double){
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

  val bound1 = ((variable.max - initValue) * slope).toLong
  val bound2 = ((variable.min - initValue) * slope).toLong

  val (minStep, maxStep) = if (bound1 < bound2) (bound1, bound2) else (bound2, bound1)
}



/**
  * this neighborhood persorms a gradient descent.
  * i first sense each dimension independently, and the performs a linear descent on the steepest identified gradient.
  * @param vars
  * @param name
  * @param maxNbVars
  * @param selectVars
  * @param variableIndiceToDeltaForGradientDefinition
  * @param hotRestart
  * @param linearSearch
  * @param trySubgradient
  */
case class GradientDescentRotating(vars:Array[CBLSIntVar],
                                   name:String = "GradientDescent",
                                   maxNbVars:Int = Integer.MAX_VALUE,
                                   selectVars:Iterable[Long],
                                   variableIndiceToDeltaForGradientDefinition:Long => Long,
                                   linearSearch:LinearOptimizer,
                                   gradientSearchBehavior:LoopBehavior,
                                   trySubgradient:Boolean = false)
  extends AbstractGradientDescent(vars:Array[CBLSIntVar],
    name:String,
    linearSearch,
    trySubgradient)  {


  override def findGradient(initialObj: Long): List[GradientComponent] = {

    val (iterator,notifyFound) = gradientSearchBehavior.toIterator(0 until Int.MaxValue)

    var bestObj = initialObj
    var bestGradient:List[GradientComponent]= Nil

    def testCurrentGradient(currentGradient:List[GradientComponent]):Boolean = {
      if(currentGradient.isEmpty) return false

      val newObj = obj.value

      if(newObj < bestObj){
        bestObj = newObj
        bestGradient = currentGradient
        notifyFound()
      }

      iterator.next
      iterator.hasNext
    }

    //on itère sur toutes les variables

    //on teste +delta et - delta en construisant un cube.
    //on arrête dès qu'on a trouvé une pente négative ou on prend la meilleure
    //il y a donc un first/best

    //pour itérer,on doit faire un truc récursif?
    //peut-on faire par nombre de variable incluse?
    //oui; probablement plus rapide d'ailleurs.

    ///returns true if shouldStop
    def exploreVars(varsToTest:List[Int],
                    maxNbVarsToTest:Int,
                    currentGradient:List[GradientComponent]): Boolean ={
      if(testCurrentGradient(currentGradient)) return true

      if(maxNbVarsToTest == 0) return false
      if(varsToTest.isEmpty) return false

      val currentVarID::tail = varsToTest

      val initVal = vars(currentVarID).newValue
      val delta = variableIndiceToDeltaForGradientDefinition(currentVarID)

      //first: no change on this var
      exploreVars(tail, maxNbVarsToTest, currentGradient)

      //2: +delta
      if(initVal + delta < vars(currentVarID).max){
        //We can explore + delta
        vars(currentVarID) :+= delta
        val component = GradientComponent(
          vars(currentVarID),
          initVal,
          currentVarID,
          1.0/delta)

        if(exploreVars(tail, maxNbVarsToTest-1, component :: currentGradient)){
          vars(currentVarID) := initVal
          return true
        }
        vars(currentVarID) := initVal
      }

      //3: -delta
      if(vars(currentVarID).min < initVal - delta){
        //We can explore - delta
        vars(currentVarID) :-= delta
        val component = GradientComponent(
          vars(currentVarID),
          initVal,
          currentVarID,
          -1.0/delta)

        if(exploreVars(tail, maxNbVarsToTest-1, component :: currentGradient)){
          vars(currentVarID) := initVal
          return true
        }
        vars(currentVarID) := initVal
      }

      false
    }

    for(nbVar <- 1 to (maxNbVars min vars.length-1)){
      //il y a donc l'ensemble des sous-ensembles = 3^nbVars posibilités

      if(exploreVars(
        varsToTest = selectVars.toList.map(v => v.toInt),
        nbVar,
        currentGradient = Nil)) return bestGradient
    }

    bestGradient
  }
}






/**
  * this neighborhood persorms a gradient descent.
  * i first sense each dimension independently, and the performs a linear descent on the steepest identified gradient.
  * @param vars
  * @param name
  * @param maxNbVars
  * @param selectVars
  * @param variableIndiceToDeltaForGradientDefinition
  * @param hotRestart
  * @param linearSearch
  * @param trySubgradient
  */
case class GradientDescent(vars:Array[CBLSIntVar],
                           name:String = "GradientDescent",
                           maxNbVars:Int = Integer.MAX_VALUE,
                           selectVars:Iterable[Long],
                           variableIndiceToDeltaForGradientDefinition:Long => Long,
                           hotRestart:Boolean = true,
                           linearSearch:LinearOptimizer,
                           trySubgradient:Boolean = false)
  extends AbstractGradientDescent(vars:Array[CBLSIntVar],
    name:String,
    linearSearch,
    trySubgradient) {

  var startIndiceForHotRestart: Long = 0

  override def findGradient(initialObj: Long):List[GradientComponent] = {

    var toReturnGradient : List[GradientComponent] = Nil
    var selectedVarSet:SortedSet[Int] = SortedSet.empty

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
            val objBelow = obj.assignVal(currentVar, valueBelow)
            (objBelow - initialObj).toDouble / (valueBelow - oldVal).toDouble
          } else {
            val objAbove = obj.assignVal(currentVar, valueAbove)
            (objAbove - initialObj).toDouble / (valueAbove - oldVal).toDouble
          }
        }

        if (slope != 0L &&
          (slope < 0L || ((oldVal - deltaForVar) > currentVar.min))
          && (slope > 0L || ((oldVal + deltaForVar) < currentVar.max))) {

          toReturnGradient =
            GradientComponent(
              currentVar,
              oldVal,
              currentVarIndice,
              slope) :: toReturnGradient
          selectedVarSet += currentVarIndice
        }
      }
    }

    if(selectVarsIt.hasNext){
      startIndiceForHotRestart = selectVarsIt.next
    }else{
      startIndiceForHotRestart = startIndiceForHotRestart + 1
    }

    toReturnGradient
  }
}


abstract class AbstractGradientDescent(vars:Array[CBLSIntVar],
                                       name:String,

                                       linearSearch:LinearOptimizer,
                                       trySubgradient:Boolean = false)
  extends EasyNeighborhoodMultiLevel[GradientMove](name) {

  private var gradientDefinition:List[GradientComponent] = List.empty
  private var currentStep:Long = 0

  def findGradient(initialObj: Long):List[GradientComponent]

  /**
    * This is the method you must implement and that performs the search of your neighborhood.
    * every time you explore a neighbor, you must perform the calls to notifyMoveExplored or moveRequested(newObj) && submitFoundMove(myMove)){
    * as explained in the documentation of this class
    */
  override def exploreNeighborhood(initialObj: Long): Unit = {
    //step1: interroger le gradient dans toutes les directions de selectedVars

    val gradientDefinition:List[GradientComponent] = findGradient(initialObj: Long)
    performDescent(initialObj: Long, gradientDefinition)
  }

  def performDescent(initialObj: Long, initGradientDefinition:List[GradientComponent]) {
    this.gradientDefinition = initGradientDefinition
    currentStep = 0
    while (gradientDefinition.nonEmpty) {

      // println("\t" + gradientDefinition.mkString("\n\t"))

      val minStep = gradientDefinition.map(_.minStep).max
      val maxStep = gradientDefinition.map(_.maxStep).min

      require(minStep < maxStep, "minStep:" + minStep + " should be < maxStep:" + maxStep)

      def evaluateStep(step: Long): Long = {
        this.currentStep = step

        for (component <- gradientDefinition) {
          component.takeStep(step)
        }

        val newObj = obj.value

        for (component <- gradientDefinition) {
          component.rollBack()
        }

        evaluateCurrentMoveObjTrueIfSomethingFound(newObj)
        newObj
      }

      //step2: find proper step with numeric method considered
      val (bestStep, newObj) = linearSearch.search(0, initialObj, minStep, maxStep, evaluateStep)
      //we do not consider the value because it is saved through the evaluateStep method.

      if (moveHasBeenFound) {
        return
      } else if (trySubgradient) {

        //we remove the smallest slope because the absence of move is possibly due to numerical artifacts

        def abs(d: Double): Double = if (d < 0) -d else d

        val flattestSlope = gradientDefinition.map(s => abs(s.slope)).min

        gradientDefinition = gradientDefinition.filter(c => abs(c.slope) != flattestSlope)
      } else {
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

