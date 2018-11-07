package oscar.cbls.lib.search.neighborhoods
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

/**
  * this is a class that explores a range of value between 0 and maxValue.
  * It only needs to poll the value, not to return anything
  */
abstract class LinearOptimizer{
  /**
    * the method that linear selectors must implement.
    * it performs a search and has not other interface than this one.
    * @param evaluate a function to evaluate a value; it returns an objective (BEWARE this is time consuming!)
    * @param maxValue the maximal value to explore. the min is zero.
    * @param objAtZero the objective value at zero
    * @param shouldStop poll this at each iteration, and stop if it returns true
    */
  def search(evaluate:Int => Int,
             minValue:Int,
             maxValue:Int,
             objAtZero:Int,
             shouldStop:() => Boolean)

}

class Exhaustive(step:Int = 1) extends LinearOptimizer{
  override def search(evaluate: Int => Int,
                      minValue:Int,
                      maxValue: Int,
                      objAtZero: Int,
                      shouldStop: () => Boolean): Unit = {

    for(value <- minValue to maxValue by step if !shouldStop()){
      evaluate(value)
    }
  }
}

class ExponentialStepSlide(dividingRatio:Int)  extends LinearOptimizer{

  override def search(evaluate: Int => Int,
                      minValue: Int, maxValue: Int,
                      objAtZero: Int,
                      shouldStop: () => Boolean): Unit = {
    new SlideVaryingSteps(generateStepSide(minValue.abs max maxValue.abs), false).
      search(evaluate: Int => Int,
        minValue: Int, maxValue: Int,
        objAtZero: Int,
        shouldStop: () => Boolean)
  }

  def generateStepSide(maxStepSize:Int):List[Int] = {
    if(maxStepSize < dividingRatio) List(1)
    else{
      maxStepSize :: generateStepSide(maxStepSize/dividingRatio)
    }
  }
}

class SlideVaryingSteps(stepSequence:List[Int] = List(1), gradualIncrease:Boolean)
  extends LinearOptimizer{

  override def search(evaluate: Int => Int,
                      minValue: Int, maxValue: Int,
                      objAtZero: Int,
                      shouldStop: () => Boolean): Unit = {

    var didFind:Boolean = false

    var positionOfBestSoFar:Int = 0
    var bestObjSoFar:Int = objAtZero

    var currentOffset = 0
    def shiftedEvaluate(v:Int):Int = {
      val toReturn = evaluate(v + currentOffset)
      if(toReturn < bestObjSoFar){
        positionOfBestSoFar = v+currentOffset
        bestObjSoFar = toReturn
      }
      toReturn
    }

    def recurExploreNoGradualIncrease(toExploreSteps:List[Int]){
      if(shouldStop()) return
      toExploreSteps match{
        case Nil => ;
        case head::tail =>
          recurExploreNoGradualIncrease(tail)
          //bigger step did improve, try again this one step
          currentOffset = positionOfBestSoFar
          new Slide(head).search(
            shiftedEvaluate,
            minValue-currentOffset, maxValue-currentOffset,
            bestObjSoFar, shouldStop)
      }
    }

    def recurExploreGradualIncrease(toExploreSteps:List[Int]):Boolean = {
      if(shouldStop()) return false
      toExploreSteps match{
        case Nil => false
        case head::tail =>

          currentOffset = positionOfBestSoFar
          new Slide(head).search(
            shiftedEvaluate,
            minValue-currentOffset, maxValue-currentOffset,
            bestObjSoFar, shouldStop)

          if(currentOffset != positionOfBestSoFar) {
            //found something, no need to use bigger step
            return true
          }

          if(!recurExploreGradualIncrease(tail)) {
            //bigger step did not improve, no more try
            return false
          }

          //bigger step did improve, try again this one step
          currentOffset = positionOfBestSoFar
          new Slide(head).search(
            shiftedEvaluate,
            minValue-currentOffset, maxValue-currentOffset,
            bestObjSoFar, shouldStop)
          true
      }
    }
    if(gradualIncrease) {
      recurExploreGradualIncrease(stepSequence)
    }else{
      recurExploreNoGradualIncrease(stepSequence)
    }
  }
}


class Slide(step:Int = 1) extends LinearOptimizer{
  override def search(evaluate: Int => Int,
                      minValue:Int,
                      maxValue: Int,
                      objAtZero: Int,
                      shouldStop: () => Boolean): Unit = {

    var currentPoint = 0
    var currentValue = objAtZero

    val pointAbove = maxValue min (currentPoint + step)
    val valueAbove = evaluate(pointAbove)

    val pointBelow = minValue max (currentPoint - step)
    val valueBelow = evaluate(pointBelow)

    if((valueAbove min valueBelow) >= currentValue) return

    val goingUp = valueAbove < valueBelow

    if(goingUp){
      currentPoint = pointAbove
      currentValue = valueAbove
    }else{
      currentPoint = pointBelow
      currentValue = valueBelow
    }

    while(!shouldStop()){
      val nextPoint = if(goingUp){
        if(currentPoint == maxValue) return
        maxValue min (currentPoint + step)
      }else{
        if(currentPoint == minValue) return
        minValue max (currentPoint - step)
      }
      val nextValue = evaluate(nextPoint)

      if(nextValue >= currentValue){
        return
      } else{
        currentPoint = nextPoint
        currentValue = nextValue
      }
    }
  }
}

class NewtonRaphson() extends LinearOptimizer{
  override def search(evaluate: Int => Int,
                      minValue:Int,
                      maxValue: Int,
                      objAtZero: Int,
                      shouldStop: () => Boolean): Unit = {

  }
}
