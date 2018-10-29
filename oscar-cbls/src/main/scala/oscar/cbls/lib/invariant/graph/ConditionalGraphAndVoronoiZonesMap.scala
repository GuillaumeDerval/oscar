package oscar.cbls.lib.invariant.graph

import java.awt.geom.Line2D.Double
import java.awt.geom.Rectangle2D
import java.awt.{BorderLayout, Color, Dimension}

import javax.swing.JFrame
import oscar.visual.VisualDrawing
import oscar.visual.shapes.{VisualCircle, VisualLine, VisualRectangle, VisualShape}

import scala.collection.immutable.{SortedMap, SortedSet}
import scala.swing.Color



class ConditionalGraphAndVoronoiZonesMapWindow(graph:ConditionalGraphWithIntegerNodeCoordinates,
                                               centroidColor:SortedMap[Int,Color],
                                               colorForUnreachableNodes:Color = Color.black,
                                               colorForPermanentEdges:Color =Color.black,
                                               colorForOpenEdges:Color = Color.green,
                                               colorForClosedEdges:Color = Color.red){

  val visual = new ConditionalGraphAndVoronoiZonesMap(
    graph:ConditionalGraphWithIntegerNodeCoordinates,
    centroidColor:SortedMap[Int,Color],
    colorForUnreachableNodes:Color,
    colorForPermanentEdges:Color,
    colorForOpenEdges:Color,
    colorForClosedEdges:Color)

  def redraw(openConditions:SortedSet[Int],
             centroids:SortedSet[Int],
             nodeToCentroid:SortedMap[Int,Int]){
    visual.redraw(openConditions:SortedSet[Int],
      centroids:SortedSet[Int],
      nodeToCentroid:SortedMap[Int,Int])
  }
  val frame = new JFrame()
  frame.setTitle("ConditionalGraphAndVoronoiZonesMap")
  frame.setLayout(new BorderLayout())
  frame.setPreferredSize(new Dimension(960,960))
  frame.add(visual, BorderLayout.CENTER)
  frame.pack()
  frame.revalidate()
  frame.setVisible(true)
}

class ConditionalGraphAndVoronoiZonesMap(graph:ConditionalGraphWithIntegerNodeCoordinates,
                                         centroidColor:SortedMap[Int,Color],
                                         colorForUnreachableNodes:Color,
                                         colorForPermanentEdges:Color,
                                         colorForOpenEdges:Color,
                                         colorForClosedEdges:Color)
  extends VisualDrawing(false,false){

  val maxX = graph.nodeswithCoordinates.map(_.x).max
  val maxY = graph.nodeswithCoordinates.map(_.y).max

  var xMultiplier = this.getWidth.toDouble / maxX.toDouble
  var yMultiplier = this.getHeight.toDouble / maxY.toDouble

  override def addShape(shape: VisualShape, repaintAfter: Boolean = true){
    super.addShape(shape,false)
  }

  def redraw(openConditions:SortedSet[Int],
                  centroids:SortedSet[Int],
                  nodeToCentroid:SortedMap[Int,Int]) {
    super.clear(false)

    xMultiplier = this.getWidth.toDouble / maxX.toDouble
    yMultiplier = this.getHeight.toDouble / maxY.toDouble

    drawEdges(openConditions:SortedSet[Int])

    drawNodes(centroids:SortedSet[Int],
      nodeToCentroid:SortedMap[Int,Int])
  }

  def drawNodes(centroids:SortedSet[Int],
                nodeToCentroid:SortedMap[Int,Int]): Unit ={

    for(node <- graph.nodeswithCoordinates){
      if(centroids contains node.nodeId){
        //this is a centroid
        drawNode(node:NodeWithIntegerCoordinates,centroidColor(node.nodeId),true)
      }else{
        //this is not a centroid, check for a marked node
        nodeToCentroid.get(node.nodeId) match{
          case Some(centroidID) =>
            //a folowed node
            if(centroidID == -1){
              //not reacheable
              drawNode(node:NodeWithIntegerCoordinates,colorForUnreachableNodes,false)
            }else{
              //reachable by centroidID
              drawNode(node:NodeWithIntegerCoordinates,centroidColor(centroidID),false)
            }
          case None =>
            //not a marked node, set default black color, small dot
            //drawNode(node:NodeWithIntegerCoordinates,colorForUnreachableNodes,false)
        }
      }
    }
  }

  def drawNode(node:NodeWithIntegerCoordinates,color:Color,isCentroid:Boolean) = {
    if(isCentroid){
      //rectangle
      val side = 8
      val tempPoint = new VisualRectangle(this, new Rectangle2D.Double(
        node.x * xMultiplier - side/2,
        node.y * yMultiplier - side/2,
        side,
        side))
      tempPoint.innerCol_$eq(color)
    }else {
      //circle
      val radius = 3
      val tempPoint = new VisualCircle(this,
        node.x * xMultiplier,
        node.y * yMultiplier,
        radius)
      tempPoint.innerCol_$eq(color)
    }
  }

  def drawEdges(openConditions:SortedSet[Int]): Unit = {
    for (edge <- graph.edges){
      edge.conditionID match{
        case None => //permanent edge
          drawEdge(edge:Edge,0)
        case Some(conditionID) =>
          if (openConditions contains conditionID){
            drawEdge(edge:Edge,1)
          }else{
            drawEdge(edge:Edge,2)
          }
      }
    }
  }

  def drawEdge(edge:Edge,style:Int): Unit ={
    val fromCoord = graph.nodeswithCoordinates(edge.nodeA.nodeId)
    val toCoord = graph.nodeswithCoordinates(edge.nodeB.nodeId)

    val line = new VisualLine(this, new Double(
      fromCoord.x * xMultiplier,
      fromCoord.y * yMultiplier,
      toCoord.x * xMultiplier,
      toCoord.y * yMultiplier))

    style match{
      case 0 => //permanent edge
        line.dashed = true
        line.outerCol = colorForPermanentEdges
      case 1 => //open edge
        line.dashed = true
        line.outerCol = colorForOpenEdges
      case 2 => //closed edge
        line.dashed = true
        line.outerCol = colorForClosedEdges
    }
  }
}
