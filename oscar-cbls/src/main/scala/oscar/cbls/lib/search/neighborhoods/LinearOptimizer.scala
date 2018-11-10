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
    *
    * @param obj a function to evaluate a value; it returns an objective (BEWARE this is time consuming!)
    * @param maxValue the maximal value to explore. the min is zero.
    * @param objAtZero the objective value at zero
    * @param shouldStop poll this at each iteration, and stop if it returns true
    */
  def search(startPos: Int,
             startObj: Int,
             minValue: Int,
             maxValue: Int,
             obj: Int => Int,
             maxIt: Int):Int

  def andThen(b:LinearOptimizer) = new Composite(this,b)
}

class Composite(a:LinearOptimizer,b:LinearOptimizer) extends LinearOptimizer{
  override def search(startPos: Int, startObj: Int, minValue: Int, maxValue: Int, obj: Int => Int, maxIt: Int): Int = {

    val newStartPoint = a.search(startPos: Int, startObj: Int, minValue: Int, maxValue: Int, obj: Int => Int, maxIt: Int)

    b.search(newStartPoint, obj(newStartPoint), minValue: Int, maxValue: Int, obj: Int => Int, maxIt: Int)
  }

  override def toString: String = a + " andThen " + b
}


class Exhaustive(step:Int = 1) extends LinearOptimizer{
  override def search(startPos: Int, startObj: Int, minValue: Int, maxValue: Int, obj: Int => Int, maxIt: Int): Int = {

    var it = maxIt
    var bestX = startPos
    var bestF = startObj

    for(value <- minValue to maxValue by step if it > 0 && value != startPos){
      it = it - 1
      val newF = obj(value)
      if(newF < bestF){
        bestF = newF
        bestX = value
      }
    }
    bestX
  }

  override def toString: String = "Exhaustive(step:" + step + ")"
}

class ExponentialStepSlide(dividingRatio:Int)  extends LinearOptimizer{

  override def toString: String = "ExponentialStepSlide(dividingRatio:" + dividingRatio + ")"

  override def search(startPos: Int, startObj: Int, minValue: Int, maxValue: Int, obj: Int => Int, maxIt: Int): Int = {
    new SlideVaryingSteps(generateStepSide(minValue.abs max maxValue.abs).reverse, false).
      search(startPos: Int, startObj: Int, minValue: Int, maxValue: Int, obj: Int => Int, maxIt: Int)
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
  override def search(startPos: Int, startObj: Int, minValue: Int, maxValue: Int, obj: Int => Int, maxIt: Int): Int = {

    var positionOfBestSoFar:Int = startPos
    var bestObjSoFar:Int = startObj

    def myObj(v:Int):Int = {
      val toReturn = obj(v)
      if(toReturn < bestObjSoFar){
        positionOfBestSoFar = v
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
          new Slide(head).search(positionOfBestSoFar, bestObjSoFar: Int, minValue: Int, maxValue: Int, myObj: Int => Int, maxIt: Int)
      }
    }

    def recurExploreGradualIncrease(toExploreSteps:List[Int]):Boolean = {
      toExploreSteps match{
        case Nil => false
        case head::tail =>

          new Slide(head).search(positionOfBestSoFar: Int, bestObjSoFar: Int, minValue: Int, maxValue: Int, myObj: Int => Int, maxIt: Int)

          if(positionOfBestSoFar != startPos) {
            //found something, no need to use bigger step
            return true
          }

          if(!recurExploreGradualIncrease(tail)) {
            //bigger step did not improve, no more try
            return false
          }

          //bigger step did improve, try again this one step

          new Slide(head).search(positionOfBestSoFar: Int, bestObjSoFar: Int, minValue: Int, maxValue: Int, myObj: Int => Int, maxIt: Int)
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

  override def toString: String = "Slide(step:" + step + ")"

  override def search(startPos: Int, startObj: Int, minValue: Int, maxValue: Int, obj: Int => Int, maxIt: Int): Int = {

    var currentPoint = startPos
    var currentValue = startObj

    val pointAbove = maxValue min (currentPoint + step)
    val valueAbove = obj(pointAbove)

    val pointBelow = minValue max (currentPoint - step)
    val valueBelow = obj(pointBelow)

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
      val nextValue = obj(nextPoint)



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

  override def toString: String = "NewtonRaphsonRoot(dXForDetivativeEvalution:" + dXForDetivativeEvalution + ")"

  override def search(startPos: Int, startObj: Int, minValue: Int, maxValue: Int, obj: Int => Int, maxIt: Int): Int = {

    var it = maxIt
    var x = 0
    while(it > 0) {
      it -= 1
      val f = obj(x)
      println("iterate x:" + x + " f:" + f)

      val fPdx = obj(x + dXForDetivativeEvalution)
      val fMdx = obj(x - dXForDetivativeEvalution)
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

  override def toString: String = "NewtonRaphson(dXForDetivativeEvalution:" + dXForDetivativeEvalution + ")"

  override def search(startPos: Int, startObj: Int, minValue: Int, maxValue: Int, obj: Int => Int, maxIt: Int): Int = {

    val evaluate2: Int => Int = x => {
      val f = obj(x)
      val fpdx = obj(x + dXForDetivativeEvalution)

      ((fpdx - f).toDouble / dXForDetivativeEvalution) toInt
    }

    new NewtonRaphsonRoot(dXForDetivativeEvalution: Int).search(startPos: Int, evaluate2(startPos), minValue: Int, maxValue: Int, evaluate2, maxIt: Int)
  }
}



object TestRN extends App{

  def f:Int => Int = x => {x*x - 150*x + 5090}

  val a = new NewtonRaphson(1)
  println(a + " "+ a.search(0, f(0), -1000, 15000, f, 10))

  val b = new Slide(step = 10)
  println(b + " "  + b.search(0, f(0), -1000, 15000, f, 10))

  val c = new ExponentialStepSlide(10)
  println(c + " " + c.search(0, f(0), -1000, 15000, f, 10))

  val d = new Slide(step = 1000) andThen new NewtonRaphson(1) andThen new Slide(step = 1)
  println(d + " " + d.search(0, f(0), -1000, 15000, f, 10))

}