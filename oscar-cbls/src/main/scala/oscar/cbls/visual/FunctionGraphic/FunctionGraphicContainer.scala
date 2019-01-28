package oscar.cbls.visual.FunctionGraphic

/**
  * *****************************************************************************
  * OscaR is free software: you can redistribute it and/or modify
  * it under the terms of the GNU Lesser General Public License as published by
  * the Free Software Foundation, either version 2.1 of the License, or
  * (at your option) any later version.
  *
  * OscaR is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU Lesser General Public License  for more details.
  *
  * You should have received a copy of the GNU Lesser General Public License along with OscaR.
  * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
  * ****************************************************************************
  */

import java.awt.{BorderLayout, Dimension}
import javax.swing._
import oscar.cbls._

/**
  * This abstract class represent the JInternalFrame that will contain
  * a FunctionGraphic and all the add-on related to it.
  *
  * @param title The title of the JInternalFrame
  * @author fabian.germeau@student.vinci.be
  */

abstract class FunctionGraphicContainer(title:String,dimension: Dimension) extends JPanel{

  setSize(dimension)
  setLayout(new BorderLayout())
  setVisible(true)

  var graphic: FunctionGraphic = null

  validate()

  def drawGlobalCurve()

  def clear(): Unit ={
    graphic.clear()
    validate()
  }
}


/**
  * This class create the JInternalFrame that will contain
  * the ObjFunctionGraphic and all the add-on related to it.
  *
  * @param title The title of the JInternalFrame
  * @param dimension The dimension of the JInternalFrame
  * @author fabian.germeau@student.vinci.be
  */
class ObjFunctionGraphicContainer(title:String = "Evolution of the objective function", dimension: Dimension) extends FunctionGraphicContainer(title, dimension) with Runnable{

  graphic = new ObjFunctionGraphic
  add(graphic, BorderLayout.CENTER)

  var objCurveDatas:List[(Long,Long,String)] = Nil

  def run(): Unit ={
    var temp:List[(Long,Long,String)] = Nil

    while(true){
      try {
        Thread.sleep(100L)
        objCurveDatas.synchronized{
          temp = objCurveDatas.take(objCurveDatas.size)
          objCurveDatas = Nil
        }
        if (temp.nonEmpty) {
          for(i <- temp.indices){
            val j = temp.size - 1L - i
            notifyNewObjectiveValue(temp(j)._1,temp(j)._2,temp(j)._3)
          }
          drawGlobalCurve()
          temp = Nil
        }
      }catch{
        case ie:InterruptedException => return
        case e:Exception => e.printStackTrace()
      }
    }
  }

  /**
    * This method init the drawing of the curve and add a legend for the neighborhood present in the graphic
    */
  def drawGlobalCurve(): Unit ={
    graphic.drawGlobalCurve()
  }

  def notifyNewObjectiveValue(objValue:Long, objTime:Long, neighBorhood:String): Unit ={
    graphic.notifyNewObjectiveValue(objValue,objTime)
  }

  override def clear(): Unit ={
    super.clear()
  }
}
