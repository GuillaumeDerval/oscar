package oscar.cbls.lib.invariant.graph

import oscar.visual.VisualDrawing
import oscar.visual.shapes.VisualShape

import scala.collection.immutable.{SortedMap, SortedSet}

class ViewConditionalGraph(graph:ConditionalGraphWithIntegerNodeCoordinates)
  extends VisualDrawing(false,false){

    val maxX = graph.nodeswithCoordinates.map(_.x).max
    val maxY = graph.nodeswithCoordinates.map(_.y).max

    var xMultiplier:Double = this.getWidth.toDouble / maxX.toDouble
    var yMultiplier:Double = this.getHeight.toDouble / maxY.toDouble

    override def addShape(shape: VisualShape, repaintAfter: Boolean = true){
      super.addShape(shape,false)
    }

  def redrawGraph(openConditions:SortedSet[Int],
                  centroids:SortedSet[Int],
                  nodeToCentroid:SortedMap[Int,Int],
                  nodeInfo:Int => String) {
    super.clear(false)

    xMultiplier = this.getWidth.toDouble / maxX.toDouble
    yMultiplier = this.getHeight.toDouble / maxY.toDouble



  }
}
