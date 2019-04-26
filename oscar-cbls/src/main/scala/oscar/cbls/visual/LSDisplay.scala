package oscar.cbls.visual

import javax.swing.JPanel
import oscar.cbls.Solution
import oscar.visual.{VisualDrawing, VisualFrame}
import oscar.visual.shapes.VisualShape

abstract class LSDisplay(title:String,width:Int = 960,height:Int = 960){

  def redraw(s:Solution,drawing:VisualDrawing)

  def update(s:Solution,obj:Int): Unit ={

  }

  def createDrawing() = new VisualDrawing(false,false){
    override def addShape(shape: VisualShape, repaintAfter: Boolean): Unit ={
      super.addShape(shape,false)
    }
  }

  val bestDisplay = createDrawing()
  val currentDisplay = createDrawing()

  SingleFrameWindow.show(bestDisplay,"best" + title,width,height)
  SingleFrameWindow.show(bestDisplay,"current" + title,width,height)


}
