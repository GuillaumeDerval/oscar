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
             maxIt:Int):Int

}

class Exhaustive(step:Int = 1) extends LinearOptimizer{
  override def search(evaluate: Int => Int,
                      minValue:Int,
                      maxValue: Int,
                      objAtZero: Int,
                      maxIt:Int): Int = {

    var it = maxIt
    var bestX = 0
    var bestF = objAtZero

    for(value <- minValue to maxValue by step if it > 0){
      it = it - 1
      val newF = evaluate(value)
      if(newF < bestF){
        bestF = newF
        bestX = value
      }
    }
    bestX
  }
}

class ExponentialStepSlide(dividingRatio:Int)  extends LinearOptimizer{

  override def search(evaluate: Int => Int,
                      minValue: Int, maxValue: Int,
                      objAtZero: Int,
                      maxIt:Int): Int = {
    new SlideVaryingSteps(generateStepSide(minValue.abs max maxValue.abs).reverse, false).
      search(evaluate: Int => Int,
        minValue: Int, maxValue: Int,
        objAtZero: Int,
        maxIt:Int)
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
                      maxIt:Int): Int = {

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
      toExploreSteps match{
        case Nil => ;
        case head::tail =>
          recurExploreNoGradualIncrease(tail)
          //bigger step did improve, try again this one step
          currentOffset = positionOfBestSoFar
          new Slide(head).search(
            shiftedEvaluate,
            minValue-currentOffset, maxValue-currentOffset,
            bestObjSoFar, maxIt:Int)
      }
    }

    def recurExploreGradualIncrease(toExploreSteps:List[Int]):Boolean = {
      toExploreSteps match{
        case Nil => false
        case head::tail =>

          currentOffset = positionOfBestSoFar
          new Slide(head).search(
            shiftedEvaluate,
            minValue-currentOffset, maxValue-currentOffset,
            bestObjSoFar, maxIt:Int)

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
            bestObjSoFar, maxIt:Int)
          true
      }
    }
    if(gradualIncrease) {
      recurExploreGradualIncrease(stepSequence)
    }else{
      recurExploreNoGradualIncrease(stepSequence)
    }

    positionOfBestSoFar
  }
}


class Slide(step:Int = 1) extends LinearOptimizer{
  override def search(evaluate: Int => Int,
                      minValue:Int,
                      maxValue: Int,
                      objAtZero: Int,
                      maxIt:Int): Int = {

    var currentPoint = 0
    var currentValue = objAtZero

    val pointAbove = maxValue min (currentPoint + step)
    val valueAbove = evaluate(pointAbove)

    val pointBelow = minValue max (currentPoint - step)
    val valueBelow = evaluate(pointBelow)

    if((valueAbove min valueBelow) >= currentValue) {
      println("not moving")
      return currentPoint
    }

    val goingUp = valueAbove < valueBelow

    if(goingUp){
      println("going up")
      currentPoint = pointAbove
      currentValue = valueAbove
    }else{
      println("going down")
      currentPoint = pointBelow
      currentValue = valueBelow
    }
    var it = maxIt
    while(it > 0){

      println("nextPoint:" + currentPoint + " nextValue:" + currentValue)

      it -= 1
      val nextPoint = if(goingUp){
        if(currentPoint == maxValue) return currentPoint
        maxValue min (currentPoint + step)
      }else{
        if(currentPoint == minValue) return currentPoint
        minValue max (currentPoint - step)
      }
      val nextValue = evaluate(nextPoint)



      if(nextValue >= currentValue){
        println("stopped")
        return currentPoint
      } else{
        currentPoint = nextPoint
        currentValue = nextValue
      }
    }
    currentPoint
  }
}

//this one finds a root!!!
class NewtonRaphsonRoot(dXForDetivativeEvalution:Int) extends LinearOptimizer{
  override def search(evaluate: Int => Int,
                      minValue:Int,
                      maxValue: Int,
                      objAtZero: Int,
                      maxIt:Int): Int = {

    var it = maxIt
    var x = 0
    while(it > 0) {
      it -= 1
      val f = evaluate(x)
      println("iterate x:" + x + " f:" + f)

      val fPdx = evaluate(x + dXForDetivativeEvalution)
      val fMdx = evaluate(x - dXForDetivativeEvalution)
      if (fPdx > f && fMdx > f) {
        //best is closer to x than dx, we stop here.
        println("best answer: " + x)
        return x
      }

      val slope:Double = (fPdx - fMdx).toDouble / (2*dXForDetivativeEvalution)
      println("slope:" + slope)
      val newX = (x - (f / slope)).toInt
      if(x == newX) return x
      x = newX
    }
    x
  }
}


//this one finds a root!!!
class NewtonRaphson(dXForDetivativeEvalution:Int) extends LinearOptimizer{
  override def search(evaluate: Int => Int,
                      minValue:Int,
                      maxValue: Int,
                      objAtZero: Int,
                      maxIt:Int): Int = {

    val evaluate2: Int => Int = x => {
      val f = evaluate(x)
      val fpdx = evaluate(x + dXForDetivativeEvalution)

      ((fpdx - f).toDouble / dXForDetivativeEvalution) toInt
    }

    new NewtonRaphsonRoot(dXForDetivativeEvalution: Int).search(
      evaluate2,
      minValue: Int,
      maxValue: Int,
      evaluate2(0),
      maxIt:Int)
  }
}


class Composite(a:LinearOptimizer,b:LinearOptimizer) extends LinearOptimizer{
  override def search(evaluate: Int => Int, minValue: Int, maxValue: Int, objAtZero: Int, maxIt:Int): Int = {

    val newStartPoint = a.search(evaluate: Int => Int, minValue: Int, maxValue: Int, objAtZero: Int, maxIt:Int)
    val newEval:Int => Int = x => evaluate(x + newStartPoint)

    newStartPoint + b.search(evaluate = newEval, minValue + newStartPoint, maxValue+ newStartPoint, evaluate(newStartPoint), maxIt:Int)
  }
}

object TestRN extends App{


  def f:Int => Int = x => {x*x - 150*x + 5000}


  println("NR: " + new NewtonRaphson(1).search(f,0, 40000, f(0),10))


  println("slide:" + new Slide(step = 10).search(f,0, 40000, f(0),10))


  println("exp:" + new ExponentialStepSlide(10).search(f,0, 40000, f(0),10))


  println("NR,Slide:" + new Composite(
    new NewtonRaphson(1),
    new Slide(step = 1)).search(f,0, 40000, f(0),10))

}