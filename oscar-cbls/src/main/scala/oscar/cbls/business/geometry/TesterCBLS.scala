package oscar.cbls.business.geometry

import java.awt.Color

import org.locationtech.jts.geom.Coordinate
import oscar.cbls.business.geometry
import oscar.cbls.business.geometry.invariants._
import oscar.cbls.business.geometry.visu.GeometryDrawing
import oscar.cbls.core.computation.IntValue
import oscar.cbls.core.objective.Objective
import oscar.cbls.core.search._
import oscar.cbls.lib.invariant.numeric.Sum
import oscar.cbls.lib.search.combinators.{Atomic, BestSlopeFirst, Dyn, Profile}
import oscar.cbls.lib.search.neighborhoods._
import oscar.cbls.visual.{ColorGenerator, SingleFrameWindow}
import oscar.cbls.{CBLSIntVar, Objective, Store}

object TesterCBLS extends App{

  val store = Store()

  val nbCircle = 10

  val radiusArray = Array.tabulate(nbCircle:Int){
    i => 30*(i+1)
  }

  val maxX = 1100
  val maxY = 1100

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

  val placedCirles = Array.tabulate(nbCircle){i =>
    new Apply(store,new Translation(store:Store,coordArray(i)._1,coordArray(i)._2),
      new CBLSGeometryConst(store,if(i%2 ==0) geometry.createSquare(radiusArray(i)*2) else geometry.createCircle(radiusArray(i),nbEdges = 40),"circle_" + i))
  }

  val intersectionAreasHalfMatrix:Array[Array[IntValue]] =
    Array.tabulate(nbCircle)(cirleID1 =>
      Array.tabulate(cirleID1)(cirleID2 =>
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

  val convexHullOfCircle1And3 = new ConvexHull(store, new Union(store, placedCirles(1),placedCirles(3)))

  val obj:Objective = totalIntersectionArea
  store.close()

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  val randomColors = ColorGenerator.generateRandomTransparentColors(nbCircle,175).toList

  val drawing = new GeometryDrawing()

  def updateDisplay() {
    val colorsIt = randomColors.toIterator
    drawing.drawShapes(shapes =
      (outerFrame,Some(Color.red),None,"")::
        (convexHullOfCircle1And3.value,Some(Color.blue),None,"convexHullOfCircle1And3")::
        Array.tabulate(nbCircle)(circleID => (placedCirles(circleID).value,None, Some(colorsIt.next), intersectionPerShape(circleID).toString)).toList)
  }

  updateDisplay()

  SingleFrameWindow.show(drawing,"a drawing with the standard oscar drawing console",1300,1300)

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


  def moveToHole = ConstantMovesNeighborhood(
    () => {
      val allCircles = placedCirles.map(_.value).toArray

      val holes:Iterable[(Int,Int)] = Overlap.centroidsOfFreeSpacesIn(allCircles,outerFrame)

      allCircles.indices.flatMap(circleID => {

        val oldX = coordArray(circleID)._1.value
        val oldY = coordArray(circleID)._2.value

        holes.map(hole => {
          new EvaluableCodedMove(() => {
            coordArray(circleID)._1 := (hole._1 max radiusArray(circleID)) min (maxX - radiusArray(circleID))
            coordArray(circleID)._2 := (hole._2 max radiusArray(circleID)) min (maxY - radiusArray(circleID))

            () => {
              coordArray(circleID)._1 := oldX
              coordArray(circleID)._2 := oldY
            }
          })
        })
      })
    },
    neighborhoodName = "moveToHole"
  )


  def gradientOnOneShape(shapeID:Int) = new GradientDescent(
    vars = Array(coordArray(shapeID)._1,coordArray(shapeID)._2),
    name= "GradientMove(" + shapeID + ")",
    maxNbVars = 2,
    selectVars = List(0,1),
    variableIndiceToDeltaForGradientDefinition = _ => 5,
    hotRestart = false,
    linearSearch = new NarrowingStepSlide(dividingRatio = 10, minStep = 1)) //new Slide(step=20,maxIt=100)) // new NewtonRaphsonMinimize(5,40))

  def swapAndGradient = swapX dynAndThen(swapMove => (
    swapYSlave(swapMove.idI,swapMove.idJ)
      andThen new Atomic(gradientOnOneShape(swapMove.idI),
      _>10,
      stopAsSoonAsAcceptableMoves=true))) name "SwapAndGradient"

  val displayDelay:Long = 1000.toLong * 1000 * 500 //.5 seconds
  var lastDisplay = System.nanoTime()
  val search = (Profile(BestSlopeFirst( //TODO: this is not adapted for single shot neighborhoods such as gradient
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

  //Thread.sleep(10000)
  //println("start")
  //Thread.sleep(1000)

  search.verbose = 1
  search.doAllMoves(obj=obj)

  updateDisplay() //after finish

  println(search.profilingStatistics)
  println
  println(intersectionPerShape.mkString("\n"))
  println(totalIntersectionArea)
}
