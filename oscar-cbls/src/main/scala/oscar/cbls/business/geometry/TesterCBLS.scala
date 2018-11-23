package oscar.cbls.business.geometry

import java.awt.Color

import org.locationtech.jts.geom.{Coordinate, Geometry}
import oscar.cbls.business.geometry
import oscar.cbls.business.geometry.invariants._
import oscar.cbls.business.geometry.visu.GeometryDrawing
import oscar.cbls.core.computation.AtomicValue
import oscar.cbls.core.constraint.ConstraintSystem
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
    new Apply(store,new Translation(store:Store,coordArray(i)._1,coordArray(i)._2),new CBLSGeometryConst(store,circleArray(i),"circle_" + i))//
  }

  val overlapConstraint = NoOverlap(store,placedCirles.asInstanceOf[Array[AtomicValue[Geometry]]])

  val c = new ConstraintSystem(store)
  c.add(overlapConstraint)
  val circleViolation = c.violations(placedCirles) //to ensure it is generated
  c.close()


  val convexHullOfCircle1And3 = new ConvexHull(store, new Union(store, placedCirles(1),placedCirles(3)))

  val obj:Objective = c.violation
  store.close()

  val randomColors = ColorGenerator.generateRandomTransparentColors(nbCircle,175).toList

  val drawing = new GeometryDrawing()

  def updateDisplay() {
    val colorsIt = randomColors.toIterator
    drawing.drawShapes(shapes = (convexHullOfCircle1And3.value,Some(Color.blue),None,"convexHullOfCircle1And3")::(outerFrame,Some(Color.red),None,"")::placedCirles.toList.map(shape => (shape.value, None, Some(colorsIt.next), c.violation(shape).toString)))
  }

  updateDisplay()

  SingleFrameWindow.show(drawing,"a drawing with the standard oscar drawing console",1300,1300)

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
  def swapY(circle1:Int,circle2:Int) = SwapsNeighborhood(Array(coordArray(circle1)._2,coordArray(circle2)._2), symmetryCanBeBrokenOnIndices = false, adjustIfNotInProperDomain = true, name ="swapYCoordinates")

  def swapAndSlide = (swapX dynAndThen(swapMove => {
    swapY(swapMove.idI,swapMove.idJ) andThen Atomic(BestSlopeFirst(List(smallSlideX(swapMove.idI),smallSlideY(swapMove.idI))),_>100)})) name "SwapAndAdjustOne"

  def moveOneCircleXAndThenY = moveXNumeric dynAndThen (assignMode => smallSlideY(assignMode.id)) name "moveOneCircleXAndThenY"
  def moveOneCircleYAndThenX = moveYNumeric dynAndThen (assignMode => smallSlideX(assignMode.id)) name "moveOneCircleYAndThenX"

  def moveOneCoordClassic = AssignNeighborhood(flattenedCoordArray,"moveByOneCoord")  //this one is awfully slow!!!

  def gradientOnOneShape(shapeID:Int) = new GradientDescent(
    vars = Array(coordArray(shapeID)._1,coordArray(shapeID)._2),
    name= "GradientMove(" + shapeID + ")",
    maxNbVars = 2,
    selectVars = List(0,1),
    variableIndiceToDeltaForGradientDefinition = _ => 5,
    hotRestart = false,
    linearSearch = new NarrowingStepSlide(dividingRatio = 10, minStep = 1)) //new Slide(step=20,maxIt=100)) // new NewtonRaphsonMinimize(5,40))

  def swapAndGradient = (swapX dynAndThen(swapMove => {
    swapY(swapMove.idI,swapMove.idJ) andThen new Atomic(gradientOnOneShape(swapMove.idI),_>10)})) name "SwapAndGradientOne"

  val displayDelay:Long = 1000.toLong * 1000 * 100 //.1 seconds
  var lastDisplay = System.nanoTime()
  val search = Profile(BestSlopeFirst( //TODO: this is not adapted for single shot neighborhoods such as gradient
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
      Profile(moveOneCoordNumeric),
      Profile(moveOneCircleXAndThenY),
      Profile(swapAndSlide),
      Profile(swapAndGradient),
      Profile(moveOneCircleYAndThenX)),
    refresh=nbCircle)).afterMove{
    if(System.nanoTime() > lastDisplay + displayDelay) {
      updateDisplay()
      lastDisplay = System.nanoTime()
    }
  } //showObjectiveFunction(obj)

  updateDisplay() //before start

  search.verbose = 2
  search.doAllMoves(obj=obj)

  updateDisplay() //after finish
  println("finished search" + c)
  println(search.profilingStatistics)
  println("\t" + circleViolation.mkString("\n\t"))
}
