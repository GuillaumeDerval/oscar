package oscar.cbls.visual

import java.awt.{BorderLayout, Color, Dimension}

import javax.swing.{JFrame, JLayeredPane, JPanel}
import oscar.cbls.visual.geometry.GeometryDrawingOnRealMap
import oscar.visual.VisualDrawing
import oscar.visual.shapes.{VisualLine, VisualRectangle}

object SingleFrameWindow{
  def show(pannel:JPanel,title:String,width:Int = 960,height:Int = 960,backgroundPanel: Option[JPanel] = None):SingleFrameWindow = {
    new SingleFrameWindow(pannel:JPanel,title:String,width,height,backgroundPanel)
  }

  def showFrame(frame:JFrame,title:String,width:Int = 960,height:Int = 960) = {
    frame.setTitle(title)
    frame.setLayout(new BorderLayout())
    frame.setPreferredSize(new java.awt.Dimension(width,height))
    frame.pack()
    frame.revalidate()
    frame.setVisible(true)
  }
}

class SingleFrameWindow(val pannel:JPanel,title:String,width:Int,height:Int,backgroundPanel: Option[JPanel] = None){
  pannel.setPreferredSize(new Dimension(width,height))

  val frame = new JFrame()
  frame.setTitle(title)
  frame.setPreferredSize(new java.awt.Dimension(width,height))
  if(backgroundPanel.isDefined) {
    frame.setContentPane(backgroundPanel.get)
    frame.setGlassPane(pannel)
    pannel.setOpaque(false)
    pannel.setVisible(true)
  }
  else frame.add(pannel)
  frame.setResizable(true)
  frame.pack()
  frame.revalidate()
  frame.setVisible(true)
}
