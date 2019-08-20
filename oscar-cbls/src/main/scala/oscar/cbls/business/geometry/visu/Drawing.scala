package oscar.cbls.business.geometry.visu

import java.awt.Color
import java.io.File

import org.locationtech.jts.geom.Geometry
import oscar.cbls.visual.SingleFrameWindow
import oscar.cbls.visual.geometry.{GeometryDrawing, GeometryDrawingOnRealMap, GeometryDrawingTypes}

class Drawing (title: String,
               relevantDistances: List[(Int,Int)],
               geometryDrawingType: GeometryDrawingTypes.Value,
               area: Option[List[(Int,Int)]] = None,
               pointOfOrigin: Option[(Double, Double)] = None,
               savingFile: Option[File] = None) {
  val (drawing, background) = geometryDrawingType match {
    case GeometryDrawingTypes.Simple =>
      (GeometryDrawing(relevantDistances,geometryDrawingType,area,pointOfOrigin, savingFile = savingFile), None)
    case GeometryDrawingTypes.OnRealMap =>
      val background = GeometryDrawing(relevantDistances,GeometryDrawingTypes.OnRealMap,area,pointOfOrigin).asInstanceOf[GeometryDrawingOnRealMap]
      (GeometryDrawing(relevantDistances,GeometryDrawingTypes.Simple,area,pointOfOrigin,pointShift = Some(() => background.topLeftPointShiftInPixel), savingFile = savingFile), Some(background))
  }
  val singleFrameWindow: SingleFrameWindow = SingleFrameWindow.show(drawing, title, backgroundPanel = background)

  /**
    * This method save the current drawing as a png in the specified savingFile.
    */
  def saveDrawingAsPNG(): Unit = {
    require(savingFile.nonEmpty, "Unable to save the current drawing - No file specified")
    singleFrameWindow.saveWindowAsPng(savingFile.get)
  }

  def drawShapes(boundingBoxOn:Option[Geometry] = None,
                 shapes:List[(Geometry,Option[Color],Option[Color],String)],
                 centers:List[(Long,Long)]): Unit ={
    drawing.drawShapes(boundingBoxOn,shapes,centers)
  }
}
