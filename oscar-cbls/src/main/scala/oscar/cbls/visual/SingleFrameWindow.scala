package oscar.cbls.visual

import java.awt.BorderLayout

import javax.swing.{JFrame, JPanel}

object SingleFrameWindow{
  def show(pannel:JPanel,title:String,width:Int = 960,height:Int = 960){
    new SingleFrameWindow(pannel:JPanel,title:String,width,height)
  }
}

class SingleFrameWindow(val pannel:JPanel,title:String,width:Int,height:Int){
  val frame = new JFrame()
  frame.setTitle(title)
  frame.setLayout(new BorderLayout())
  frame.setPreferredSize(new java.awt.Dimension(width,height))
  frame.add(pannel, BorderLayout.CENTER)
  frame.pack()
  frame.revalidate()
  frame.setVisible(true)
}
