package oscar.cbls.business.geometry

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

import java.awt.Color

import org.locationtech.jts.geom.Coordinate
import oscar.cbls.business.geometry
import oscar.cbls.business.geometry.invariants._
import oscar.cbls.business.geometry.model.CBLSGeometryConst
import oscar.cbls.business.geometry.visu.GeometryDrawing
import oscar.cbls.core.computation.IntValue
import oscar.cbls.core.search._
import oscar.cbls.lib.invariant.numeric.{Sqrt, Square, Sum, Sum2}
import oscar.cbls.lib.search.combinators.{Atomic, BestSlopeFirst, Profile}
import oscar.cbls.lib.search.neighborhoods._
import oscar.cbls.visual.{ColorGenerator, SingleFrameWindow}
import oscar.cbls.{CBLSIntVar, Objective, Store}

object TestGeometryPackingMinDistance extends App{

  val store = Store()

  val nbCircle = 10

  val radiusArray = Array.tabulate(nbCircle:Int){
    i => 30*(i+1)
  }

  val maxX = 1100
  val maxY = 1380

  val outerFrame = geometry.factory.createLinearRing(Array(
    new Coordinate(0,0),
    new Coordinate(0,maxY),
    new Coordinate(maxX,maxY),
    new Coordinate(maxX,0),
    new Coordinate(0,0))).convexHull()

  //declaring the optimization model
  val coordArray = Array.tabulate(nbCircle){ i =>
    (new CBLSIntVar(store,radiusArray(i),radiusArray(i) to maxX - radiusArray(i),"circle_" + i + ".x"),
      new CBLSIntVar(store,radiusArray(i),radiusArray(i) to maxY - radiusArray(i),"circle_" + i + ".y"))
  }

  val constantFiguresAt00 = Array.tabulate(nbCircle){i =>
        new CBLSGeometryConst(store,
          if(i%2 ==0) geometry.createRectangle(radiusArray(i)*2,radiusArray(i)*2)
          else geometry.createCircle(radiusArray(i),nbEdges = 30),
          "shape_" + i)
    }

  val placedCirles = Array.tabulate(nbCircle){i =>
    new Apply(store,new Translation(store:Store,coordArray(i)._1,coordArray(i)._2),constantFiguresAt00(i))
  }

  val intersectionAreasHalfMatrix:Array[Array[IntValue]] =
    Array.tabulate(nbCircle)(cirleID1 =>
      Array.tabulate(cirleID1)(cirleID2 =>
        //TODO: this way of doing is very slow.
        Area(store,Intersection(store,placedCirles(cirleID1),placedCirles(cirleID2),true))))

  val intersectionAreaFullMatrix:Array[Array[IntValue]] =
    Array.tabulate(nbCircle)(cirleID1 =>
      Array.tabulate(nbCircle)(cirleID2 =>
        if(cirleID1 == cirleID2) 0
        else intersectionAreasHalfMatrix(cirleID1 max cirleID2)(cirleID1 min cirleID2)))

  val totalIntersectionArea:IntValue = new Sum(intersectionAreasHalfMatrix.flatMap(_.toList)).setName("totalOverlapArea")

  val intersectionPerShape:Array[IntValue] = Array.tabulate(nbCircle)(circleID =>
    Sum(intersectionAreaFullMatrix(circleID)).setName("overlapArea(shape_" + circleID + ")")
  )

  val relevantDistances = List((0,1),(1,2),(1,3),(2,4),(3,5),(4,5),(5,6))

  val distancesArray = relevantDistances.map({case (fromId,toId) => {
    val (x1, y1) = coordArray(fromId)
    val (x2, y2) = coordArray(toId)
    Sqrt(Sum2(Square(x1 - x2),Square(y1 - y2)))
  }})

  //val convexHullOfCircle1And3 = new ConvexHull(store, new Union(store, placedCirles(1),placedCirles(3)))

  val obj:Objective = Sum2(totalIntersectionArea,new Sum(distancesArray))
  store.close()

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  val randomColors = ColorGenerator.generateRandomTransparentColors(nbCircle,175).toList

  val drawing = new GeometryDrawing(relevantDistances)

  def updateDisplay() {
    val colorsIt = randomColors.toIterator
    drawing.drawShapes(shapes =
      (outerFrame,Some(Color.red),None,"")::
        //(convexHullOfCircle1And3.value,Some(Color.blue),None,"convexHullOfCircle1And3")::
        Array.tabulate(nbCircle)(circleID => (placedCirles(circleID).value,None, Some(colorsIt.next), intersectionPerShape(circleID).toString)).toList,
      centers = coordArray.toList.map(xy => (xy._1.value,xy._2.value)))
  }

  updateDisplay()

  SingleFrameWindow.show(drawing,"Packing and minimizing some shape to shape distance",1300,1300)

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  val flattenedCoordArray:Array[CBLSIntVar] = coordArray.flatMap(xy => List(xy._1,xy._2))

  def moveOneCoordNumeric = NumericAssignNeighborhood(flattenedCoordArray,"moveByOneCoord",
    domainExplorer = new NewtonRaphsonMinimize(20, 10) //new NarrowingExhaustive(dividingRatio = 10, maxIt = 10)
    //new Exhaustive(step = 50,skipInitial = true,maxIt = 1000/50)
  )

  def moveXNumeric = NumericAssignNeighborhood(coordArray.map(_._1),"moveX",
    domainExplorer = new NarrowingExhaustive(dividingRatio = 10, minStep = 1)
    //new Exhaustive(step = 50,skipInitial = true,maxIt = 1000/50)
  )

  def smallSlideY(circleID:Int) = NumericAssignNeighborhood(Array(coordArray(circleID)._2),"moveYSlave",
    domainExplorer = new Slide(step=1,maxIt=20) // new NarrowingExhaustive(dividingRatio = 10, maxIt = 10)
    //new Exhaustive(step = 50,skipInitial = true,maxIt = 1000/50)
  )

  def moveYNumeric = NumericAssignNeighborhood(coordArray.map(_._2),"moveY",
    domainExplorer = new NarrowingExhaustive(dividingRatio = 10, minStep = 1)
    //new Exhaustive(step = 50,skipInitial = true,maxIt = 1000/50)
  )

  def smallSlideX(circleID:Int) = NumericAssignNeighborhood(Array(coordArray(circleID)._1),"moveXSlave",
    domainExplorer = new Slide(step=1,maxIt=20) // new NarrowingExhaustive(dividingRatio = 10, maxIt = 10)
    //new Exhaustive(step = 50,skipInitial = true,maxIt = 1000/50)
  )

  def swapX = SwapsNeighborhood(coordArray.map(_._1),symmetryCanBeBrokenOnIndices = false, adjustIfNotInProperDomain = true, name = "swapXCoordinates")

  def swapYSlave(circle1:Int, circle2:Int) = SwapsNeighborhood(Array(coordArray(circle1)._2,coordArray(circle2)._2), symmetryCanBeBrokenOnIndices = false, adjustIfNotInProperDomain = true, name ="swapYCoordinates")

  def swapAndSlide = (swapX dynAndThen(swapMove => {
    swapYSlave(swapMove.idI,swapMove.idJ) andThen Atomic(BestSlopeFirst(List(smallSlideX(swapMove.idI),smallSlideY(swapMove.idI))),_>100)})) name "SwapAndSlide"

  def moveOneCircleXAndThenY = moveXNumeric dynAndThen (assignMode => smallSlideY(assignMode.id)) name "moveXAndThenY"
  def moveOneCircleYAndThenX = moveYNumeric dynAndThen (assignMode => smallSlideX(assignMode.id)) name "moveYAndThenX"

  def moveOneCoordClassic = AssignNeighborhood(flattenedCoordArray,"moveByOneCoord")  //this one is awfully slow!!!


  def moveToHole = ConstantMovesNeighborhood[MoveCircleTo](
    () => {
      val allCircles = placedCirles.map(_.value)

      val holes:Iterable[(Int,Int)] = Overlap.centroidsOfFreeSpacesIn(allCircles,outerFrame)

      allCircles.indices.flatMap(circleID => {

        val oldX = coordArray(circleID)._1.value
        val oldY = coordArray(circleID)._2.value

        holes.map(hole => {
          (newObj:Int) => new MoveCircleTo(circleID,hole._1,hole._2,oldX,oldY,newObj)
        })
      })
    },
    neighborhoodName = "toHole"
  )

  class MoveCircleTo(val circleID:Int,targetX:Int,targetY:Int,oldX:Int,oldY:Int,newObj:Int)
    extends EvaluableCodedMove(() => {
      coordArray(circleID)._1 := (targetX max radiusArray(circleID)) min (maxX - radiusArray(circleID))
      coordArray(circleID)._2 := (targetY max radiusArray(circleID)) min (maxY - radiusArray(circleID))

      () => {
        coordArray(circleID)._1 := oldX
        coordArray(circleID)._2 := oldY
      }
    },
      neighborhoodName = "toHole",
      objAfter = newObj){}


  def moveToHoleAndGradient =
    moveToHole dynAndThen(moveCircleTo => new Atomic(
      gradientOnOneShape(moveCircleTo.circleID),
      _>10,
      stopAsSoonAsAcceptableMoves=true)) name "toHole&Gradient"

  def gradientOnOneShape(shapeID:Int) = new GradientDescent(
    vars = Array(coordArray(shapeID)._1,coordArray(shapeID)._2),
    name= "GradientMove(" + shapeID + ")",
    maxNbVars = 2,
    selectVars = List(0,1),
    variableIndiceToDeltaForGradientDefinition = _ => 20,
    hotRestart = false,
    linearSearch = new NarrowingStepSlide(dividingRatio = 10, minStep = 1)) //new Slide(step=20,maxIt=100)) // new NewtonRaphsonMinimize(5,40))

  def swapAndGradient = swapX dynAndThen(swapMove => (
    swapYSlave(swapMove.idI,swapMove.idJ)
      andThen new Atomic(gradientOnOneShape(swapMove.idI),
      _>10,
      stopAsSoonAsAcceptableMoves=true))) name "swap&Gradient"


  val displayDelay:Long = 1000.toLong * 1000 * 1000 //1 seconds
  var lastDisplay = System.nanoTime()
  val search = (Profile(BestSlopeFirst( //TODO: this is not the best approach for single shot neighborhoods such as gradient
    List(
      Profile(gradientOnOneShape(0)),  //TODO: try gradient on multiple shapes at the same time.
      Profile(gradientOnOneShape(1)),
      Profile(gradientOnOneShape(2)),
      Profile(gradientOnOneShape(3)),
      Profile(gradientOnOneShape(4)),
      Profile(gradientOnOneShape(5)),
      Profile(gradientOnOneShape(6)),
      Profile(gradientOnOneShape(7)),
      Profile(gradientOnOneShape(8)),
      Profile(gradientOnOneShape(9)),
      Profile(moveToHole),
      Profile(moveToHoleAndGradient),
      Profile(moveOneCoordNumeric),
      Profile(moveOneCircleXAndThenY),
      Profile(swapAndSlide),
      Profile(swapAndGradient),
      Profile(moveOneCircleYAndThenX)
    ),
    refresh=nbCircle*10))
    onExhaustRestartAfter (RandomizeNeighborhood(flattenedCoordArray, () => flattenedCoordArray.length/5, name = "smallRandomize"),maxRestartWithoutImprovement = 2, obj)
    onExhaustRestartAfter (RandomizeNeighborhood(flattenedCoordArray, () => flattenedCoordArray.length, name = "fullRandomize"),maxRestartWithoutImprovement = 2, obj)
    afterMove {if(System.nanoTime() > lastDisplay + displayDelay) {
    updateDisplay()
    lastDisplay = System.nanoTime()
  }
  } showObjectiveFunction obj)

  // Thread.sleep(20000)
  // println("start")
  // Thread.sleep(1000)

  search.verbose = 1
  search.doAllMoves(obj=obj, shouldStop = _ => obj.value == 0)

  updateDisplay() //after finish

  println(search.profilingStatistics)
  println
  println(intersectionPerShape.mkString("\n"))
  println(totalIntersectionArea)
}
