package oscar.cbls.business.geometric

import java.awt.geom.Rectangle2D

import oscar.visual.VisualDrawing
import oscar.visual.shapes.{VisualRectangle, VisualShape}

class Visu()
  extends VisualDrawing(false,false) {

  override def addShape(shape: VisualShape, repaintAfter: Boolean): Unit ={
    super.addShape(shape,false)
  }



}