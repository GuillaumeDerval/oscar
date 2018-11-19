package oscar.cbls.business.geometry

import org.locationtech.jts.geom.Geometry
import oscar.cbls.business.geometry
import oscar.cbls.core.computation.AtomicValue
import oscar.cbls.core.constraint.ConstraintSystem
import oscar.cbls.lib.search.neighborhoods._
import oscar.cbls.visual.{ColorGenerator, SingleFrameWindow}
import oscar.cbls.{CBLSIntVar, Objective, Store}

object TesterCBLS extends App{

  val store = Store()

  val nbCircle = 10
  var circleArray = Array.tabulate(nbCircle:Int){
    i => geometry.createCircle(30*(i+1))
  }

  val coordArray = Array.tabulate(nbCircle){ i =>
    (new CBLSIntVar(store,0,0 to 1000,"circle_" + i + ".x"),
      new CBLSIntVar(store,0,0 to 1000,"circle_" + i + ".y"))
  }

  val flattenedCoordArray:Array[CBLSIntVar] = coordArray.map(xy => List(xy._1,xy._2)).flatten.toArray

  val placedCirles = Array.tabulate(nbCircle){i =>
    new Apply(store,new Translation(store:Store,coordArray(i)._1,coordArray(i)._2),new CBLSGeometryConst(store,circleArray(i)))
  }

  val overlapConstraint = new NoOverlap(placedCirles.asInstanceOf[Array[AtomicValue[Geometry]]])

  val c = new ConstraintSystem(store)

  c.add(overlapConstraint)
  c.close()

  val obj:Objective = c.violation
  store.close()

  val randomColors = ColorGenerator.generateRandomTransparentColors(nbCircle,175).toList

  val drawing = new GeometryDrawing()

  def updateDisplay() {
    val colorsIt = randomColors.toIterator
    drawing.drawShapes(shapes = placedCirles.toList.map(shape => (shape.value, None, Some(colorsIt.next), "")))
  }

  updateDisplay()

  SingleFrameWindow.show(drawing,"a drawing with the standard oscar drawing console")

  val search1 = NumericAssignNeighborhood(flattenedCoordArray,"moveByOneCoord",
    domainExplorer = new TryExtremes() //new Exhaustive(step = 100,skipInitial = true,maxIt = 10) //// carryOnTo new NarrowingExhaustive(dividingRatio=10, maxIt=20),
  ).afterMove{updateDisplay()}

  val search2 = AssignNeighborhood(flattenedCoordArray,"moveByOneCoord").afterMove{updateDisplay()}

  val search = search1
  search.verbose = 2
  search.doAllMoves(obj=obj)

  println("finished search" + c)
}
