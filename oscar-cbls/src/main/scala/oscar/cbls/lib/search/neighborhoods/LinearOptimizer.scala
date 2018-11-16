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
             obj: Int => Int):Int

  def carryOnTo(b:LinearOptimizer) = new CarryOnTo(this,b)

  def andThen(b:LinearOptimizer) = new AndThen(this,b)
}

class CarryOnTo(a:LinearOptimizer, b:LinearOptimizer) extends LinearOptimizer{
  override def search(startPos: Int, startObj: Int, minValue: Int, maxValue: Int, obj: Int => Int): Int = {
    val newStartPoint = a.search(startPos: Int, startObj: Int, minValue: Int, maxValue: Int, obj: Int => Int)
    //println("CarryOnTo(newStartPoint:" + newStartPoint + ")")
    b.search(newStartPoint, obj(newStartPoint), minValue: Int, maxValue: Int, obj: Int => Int)
  }

  override def toString: String = "(" + a + " carryOnTo " + b + ")"
}

case class AndThen(a:LinearOptimizer, b:LinearOptimizer) extends LinearOptimizer{
  override def search(startPos: Int, startObj: Int, minValue: Int, maxValue: Int, obj: Int => Int): Int = {

    var bestX = startPos
    var bestObj = startObj

    def exploreB(x:Int):Int = {
      val newX = b.search(x, obj(x), minValue: Int, maxValue: Int, obj: Int => Int)
      val o = obj(newX)
      if (o < bestObj){
        bestX = newX
        bestObj = o
      }
      o
    }

    val newStartPoint = a.search(startPos: Int, startObj: Int, minValue: Int, maxValue: Int, exploreB: Int => Int)

    bestX
  }
  override def toString: String = "(" + a + " andThen " + b + ")"
}


class Exhaustive(step:Int = 1,skipInitial:Boolean = false, maxIt: Int) extends LinearOptimizer{
  override def search(startPos: Int, startObj: Int, minValue: Int, maxValue: Int, obj: Int => Int): Int = {

    var it = maxIt
    var bestX = startPos
    var bestF = startObj

    for(value <- minValue to maxValue by step if it > 0 && (!skipInitial || value != startPos)){
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

class NarrowingStepSlide(dividingRatio:Int, maxIt: Int)  extends LinearOptimizer{

  override def toString: String = "NarrowingStepSlide(dividingRatio:" + dividingRatio + ")"

  override def search(startPos: Int, startObj: Int, minValue: Int, maxValue: Int, obj: Int => Int): Int = {
    new SlideVaryingSteps(generateSteps(minValue.abs max maxValue.abs).reverse, false,maxIt).
      search(startPos: Int, startObj: Int, minValue: Int, maxValue: Int, obj: Int => Int)
  }

  def generateSteps(maxStepSize:Int):List[Int] = {
    if(maxStepSize < dividingRatio) List(1)
    else{
      maxStepSize :: generateSteps(maxStepSize/dividingRatio)
    }
  }
}


class NarrowingExhaustive(dividingRatio:Int, maxIt: Int)  extends LinearOptimizer{

  override def toString: String = "NarrowingExhaustive(dividingRatio:" + dividingRatio + ")"

  override def search(startPos: Int, startObj: Int, minValue: Int, maxValue: Int, obj: Int => Int): Int = {
    println("NarrowingExhaustive minValue:" + minValue + " maxValue:" + maxValue)
    val width = maxValue - minValue
    if(width < dividingRatio) {
      val search = new Exhaustive(step = 1, skipInitial = true,maxIt = maxIt)
      search.search(startPos: Int, startObj: Int, minValue: Int, maxValue: Int, obj: Int => Int)
    }else{
      val step = width/dividingRatio
      val search = new Exhaustive(step = step, skipInitial = true,maxIt = maxIt)
      val newVal = search.search(startPos: Int, startObj: Int, minValue: Int, maxValue: Int, obj: Int => Int)
      this.search(newVal: Int, obj(newVal), minValue max (newVal - step), maxValue min (newVal + step), obj: Int => Int)
    }
  }
}


class TryExtremes() extends LinearOptimizer {
  override def search(startPos: Int, startObj: Int, minValue: Int, maxValue: Int, obj: Int => Int): Int = {
    val tries:List[(Int,Int)] = List((startPos,startObj),(minValue,obj(minValue)),(maxValue,obj(maxValue)))
    tries.minBy(_._2)._1
  }

  override def toString: String = "TryExtremes()"
}

class SlideVaryingSteps(stepSequence:List[Int] = List(1), gradualIncrease:Boolean,maxIt:Int)
  extends LinearOptimizer{
  override def search(startPos: Int, startObj: Int, minValue: Int, maxValue: Int, obj: Int => Int): Int = {

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
          new Slide(head,maxIt = maxIt).search(positionOfBestSoFar, bestObjSoFar: Int, minValue: Int, maxValue: Int, myObj: Int => Int)
      }
    }

    def recurExploreGradualIncrease(toExploreSteps:List[Int]):Boolean = {
      toExploreSteps match{
        case Nil => false
        case head::tail =>

          new Slide(head,maxIt = maxIt).search(positionOfBestSoFar: Int, bestObjSoFar: Int, minValue: Int, maxValue: Int, myObj: Int => Int)

          if(positionOfBestSoFar != startPos) {
            //found something, no need to use bigger step
            return true
          }

          if(!recurExploreGradualIncrease(tail)) {
            //bigger step did not improve, no more try
            return false
          }

          //bigger step did improve, try again this one step

          new Slide(head,maxIt = maxIt).search(positionOfBestSoFar: Int, bestObjSoFar: Int, minValue: Int, maxValue: Int, myObj: Int => Int)
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


class Slide(step:Int = 1, maxIt: Int) extends LinearOptimizer{

  override def toString: String = "Slide(step:" + step + ")"

  override def search(startPos: Int, startObj: Int, minValue: Int, maxValue: Int, obj: Int => Int): Int = {

    var currentPoint = startPos
    var currentValue = startObj

    val pointAbove = maxValue min (currentPoint + step)
    val valueAbove = obj(pointAbove)

    val pointBelow = minValue max (currentPoint - step)
    val valueBelow = obj(pointBelow)

    if((valueAbove min valueBelow) >= currentValue) {
      //println("not moving")
      return currentPoint
    }

    val goingUp = valueAbove < valueBelow

    if(goingUp){
      //println("going up")
      currentPoint = pointAbove
      currentValue = valueAbove
    }else{
      //println("going down")
      currentPoint = pointBelow
      currentValue = valueBelow
    }
    var it = maxIt
    while(it > 0){

      //println("nextPoint:" + currentPoint + " nextValue:" + currentValue)

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
        //println("stopped")
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
class NewtonRaphsonRoot(dXForDetivativeEvalution:Int, maxIt: Int) extends LinearOptimizer{

  override def toString: String = "NewtonRaphsonRoot(dXForDetivativeEvalution:" + dXForDetivativeEvalution + ")"

  override def search(startPos: Int, startObj: Int, minValue: Int, maxValue: Int, obj: Int => Int): Int = {

    var it = maxIt
    var x = 0
    while(it > 0) {
      it -= 1
      val f = obj(x)
      //println("iterate x:" + x + " f:" + f)

      val fPdx = obj(x + dXForDetivativeEvalution)
      val fMdx = obj(x - dXForDetivativeEvalution)
      if (fPdx > f && fMdx > f) {
        //best is closer to x than dx, we stop here.
        return x
      }

      val slope:Double = (fPdx - fMdx).toDouble / (2*dXForDetivativeEvalution)
      val newX = (x - (f / slope)).toInt
      if(x == newX) return x
      x = newX
    }
    x
  }
}


//this one finds a min!!!
class NewtonRaphson(dXForDetivativeEvalution:Int, maxIt: Int) extends LinearOptimizer{

  override def toString: String = "NewtonRaphson(dXForDetivativeEvalution:" + dXForDetivativeEvalution + ")"

  override def search(startPos: Int, startObj: Int, minValue: Int, maxValue: Int, obj: Int => Int): Int = {
    var positionOfBestSoFar:Int = startPos
    var bestObjSoFar:Int = startObj

    def myObj(v:Int):Int = {
      val toReturn = obj(v)
      if(toReturn < bestObjSoFar && minValue <= v && v <= maxValue ){
        positionOfBestSoFar = v
        bestObjSoFar = toReturn
      }
      toReturn
    }

    val evaluate2: Int => Int = x => {
      val f = myObj(x)
      val fpdx = myObj(x + dXForDetivativeEvalution)

      ((fpdx - f).toDouble / dXForDetivativeEvalution) toInt
    }

    new NewtonRaphsonRoot(dXForDetivativeEvalution: Int, maxIt: Int).search(startPos: Int, evaluate2(startPos), minValue: Int, maxValue: Int, evaluate2)
    positionOfBestSoFar
  }
}

object TestRN extends App{

  def f1:Int => Int = x => {x*x - 150*x + 5090}
  def f2:Int => Int = x => {-150*x + 5090}
  def f3:Int => Int = x => {(math.cos(x)*500).toInt - 150*x + 5090}

  val f = f3
  val maxIt = 100

  def eval(l:LinearOptimizer): Unit ={
    val aa = l.search(0, f(0), -1000, 15000, f)
    println(l + " " + aa + " obj:" + f(aa))
  }

  //slide should be avoided at all cost; cfr the stop criterion on numerical methods that stop earlier.
  //we should consider numbers as floats even if they are not because the range of value is very large.
  eval(new Exhaustive(step = 50, maxIt = maxIt) carryOnTo new Slide(step = 1, maxIt: Int))
  eval(new NewtonRaphson(1, maxIt: Int) carryOnTo new  TryExtremes())
  eval(new Slide(step = 10, maxIt: Int))
  eval(new NarrowingStepSlide(10, maxIt: Int))
  eval(new NewtonRaphson(1, maxIt: Int) carryOnTo new Slide(step = 1, maxIt: Int))
  eval(new Exhaustive(step = 50, maxIt = maxIt) carryOnTo new NewtonRaphson(1, maxIt: Int) carryOnTo new Slide(step = 1, maxIt: Int))
  eval(new Exhaustive(step = 50, maxIt = maxIt) andThen (new NewtonRaphson(1, maxIt: Int) carryOnTo new Slide(step = 1, maxIt: Int)))
  eval(new TryExtremes() carryOnTo new NewtonRaphson(1, maxIt: Int) carryOnTo new Slide(step=1, maxIt: Int))
  eval(new NarrowingExhaustive(100, maxIt: Int))

}

