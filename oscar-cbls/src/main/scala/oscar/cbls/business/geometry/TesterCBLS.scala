package oscar.cbls.business.geometry

import java.awt.Color

import org.locationtech.jts.geom.{Coordinate, Geometry}
import oscar.cbls.business.geometry
import oscar.cbls.core.computation.AtomicValue
import oscar.cbls.core.constraint.ConstraintSystem
import oscar.cbls.lib.search.combinators.{Atomic, BestSlopeFirst, Profile}
import oscar.cbls.lib.search.neighborhoods._
import oscar.cbls.visual.{ColorGenerator, SingleFrameWindow}
import oscar.cbls.{CBLSIntVar, Objective, Store}

object TesterCBLS extends App{

  val store = Store()

  val nbCircle = 10

  val radiusArray = Array.tabulate(nbCircle:Int){
    i => 30*(i+1)
  }

  var circleArray = Array.tabulate(nbCircle:Int){
    i => geometry.createCircle(radiusArray(i),nbEdges = 60)
  }

  val outerFrame = geometry.factory.createLinearRing(Array(new Coordinate(0,0),new Coordinate(0,1100),new Coordinate(1100,1100),new Coordinate(1100,0),new Coordinate(0,0))).convexHull()

  val coordArray = Array.tabulate(nbCircle){ i =>
    (new CBLSIntVar(store,radiusArray(i),radiusArray(i) to 1100 - radiusArray(i),"circle_" + i + ".x"),
      new CBLSIntVar(store,radiusArray(i),radiusArray(i) to 1100 - radiusArray(i),"circle_" + i + ".y"))
  }

  val flattenedCoordArray:Array[CBLSIntVar] = coordArray.map(xy => List(xy._1,xy._2)).flatten.toArray

  val placedCirles = Array.tabulate(nbCircle){i =>
    new Apply(store,new Translation(store:Store,coordArray(i)._1,coordArray(i)._2),new CBLSGeometryConst(store,circleArray(i))).setName("placedCircle" + i)
  }

  val overlapConstraint = new NoOverlap(placedCirles.asInstanceOf[Array[AtomicValue[Geometry]]])

  val c = new ConstraintSystem(store)

  c.add(overlapConstraint)
  c.close()


  val convexHullOfCircle1And3 = new ConvexHull(store, new Union(store, placedCirles(1),placedCirles(3)))

  val obj:Objective = c.violation
  store.close()

  val randomColors = ColorGenerator.generateRandomTransparentColors(nbCircle,175).toList

  val drawing = new GeometryDrawing()

  def updateDisplay() {
    val colorsIt = randomColors.toIterator
    drawing.drawShapes(shapes = (convexHullOfCircle1And3.value,Some(Color.blue),None,"convexHullOfCircle1And3")::(outerFrame,Some(Color.red),None,"")::placedCirles.toList.map(shape => (shape.value, None, Some(colorsIt.next), overlapConstraint.violation(shape).toString)))
  }

  updateDisplay()

  SingleFrameWindow.show(drawing,"a drawing with the standard oscar drawing console",1300,1300)

  def moveOneCoordNumeric = NumericAssignNeighborhood(flattenedCoordArray,"moveByOneCoord",
    domainExplorer = new NewtonRaphsonMinimize(20, 10) //new NarrowingExhaustive(dividingRatio = 10, maxIt = 10)
    //new Exhaustive(step = 50,skipInitial = true,maxIt = 1000/50)
  )

  def moveXNumeric = NumericAssignNeighborhood(coordArray.map(_._1),"moveX",
    domainExplorer = new NarrowingExhaustive(dividingRatio = 10, maxIt = 10)
    //new Exhaustive(step = 50,skipInitial = true,maxIt = 1000/50)
  )

  def smallSlideY(circleID:Int) = NumericAssignNeighborhood(Array(coordArray(circleID)._2),"moveYSlave",
    domainExplorer = new NarrowingStepSlide(10,10) restrictDelta(100) //new Slide(step=1,maxIt=20) // new NarrowingExhaustive(dividingRatio = 10, maxIt = 10)
    //new Exhaustive(step = 50,skipInitial = true,maxIt = 1000/50)
  )

  def moveYNumeric = NumericAssignNeighborhood(coordArray.map(_._2),"moveY",
    domainExplorer = new NarrowingExhaustive(dividingRatio = 10, maxIt = 10)
    //new Exhaustive(step = 50,skipInitial = true,maxIt = 1000/50)
  )

  def smallSlideX(circleID:Int) = NumericAssignNeighborhood(Array(coordArray(circleID)._1),"moveXSlave",
    domainExplorer = new NarrowingStepSlide(10,10) restrictDelta(100)// Slide(step=1,maxIt=20) // new NarrowingExhaustive(dividingRatio = 10, maxIt = 10)
    //new Exhaustive(step = 50,skipInitial = true,maxIt = 1000/50)
  )

  def swapX = SwapsNeighborhood(coordArray.map(_._1),symmetryCanBeBrokenOnIndices = false, adjustIfNotInProperDomain = true, name = "swapXCoordinates")
  def swapY(circle1:Int,circle2:Int) = SwapsNeighborhood(Array(coordArray(circle1)._2,coordArray(circle2)._2), symmetryCanBeBrokenOnIndices = false, adjustIfNotInProperDomain = true, name ="swapYCoordinates")

  def swapAndSlide = (swapX dynAndThen(swapMove => {
    swapY(swapMove.idI,swapMove.idJ) andThen Atomic(BestSlopeFirst(List(smallSlideX(swapMove.idI),smallSlideY(swapMove.idI))),_>100)})) name "SwapAndGlide"

  def moveOneCircleXAndThenY = moveXNumeric dynAndThen (assignMode => smallSlideY(assignMode.id)) name "moveOneCircleXAndThenY"
  def moveOneCircleYAndThenX = moveYNumeric dynAndThen (assignMode => smallSlideX(assignMode.id)) name "moveOneCircleYAndThenX"

  def moveOneCoordClassic = AssignNeighborhood(flattenedCoordArray,"moveByOneCoord")  //this one is awfully slow!!!

  val search = BestSlopeFirst(
    List(
      Profile(moveOneCoordNumeric),
      Profile(moveOneCircleXAndThenY),
      Profile(swapAndSlide),
      Profile(moveOneCircleYAndThenX)),
    refresh=nbCircle).afterMove{updateDisplay()}

  search.verbose = 2
  search.doAllMoves(obj=obj)

  println("finished search" + c)
  println(search.profilingStatistics)
}
