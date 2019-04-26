package oscar.cbls.visual.obj

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
import javax.swing.event.{ChangeEvent, ChangeListener}
import oscar.cbls.algo.search.LazyQuicksort

/**
  * This class create the JInternalFrame that will contain
  * the ObjFunctionGraphic and all the add-on related to it.
  *
  * @param dimension The dimension of the JInternalFrame
  * @author fabian.germeau@student.vinci.be
  */
class ObjFunctionGraphicContainer(dimension: Dimension)
  extends JPanel with Runnable{


  setSize(dimension)
  setLayout(new BorderLayout())
  setVisible(true)

  val graphic: FunctionGraphic = new ObjFunctionGraphic()

  validate()

  add(graphic, BorderLayout.CENTER)

  def appendData(newObj:Int,time:Long): Unit ={
    objCurveDatas.synchronized{
      objCurveDatas = (newObj,time) :: objCurveDatas
    }
  }


  private var objCurveDatas:List[(Int,Long)] = Nil

  def run(): Unit ={
    var temp:List[(Int,Long)] = Nil

    while(true){
      try {
        Thread.sleep(100)
        objCurveDatas.synchronized{
          temp = objCurveDatas.take(objCurveDatas.size)
          objCurveDatas = Nil
        }
        if (temp.nonEmpty) {
          for(i <- temp.indices){
            val j = temp.size - 1 - i
            graphic.notifyNewObjectiveValue(temp(j)._1,temp(j)._2)
          }
          graphic.drawGlobalCurve()
          temp = Nil
        }
      }catch{
        case ie:InterruptedException => return
        case e:Exception => e.printStackTrace()
      }
    }
  }

  //This is for the zoom scrollbar
  var sortedYValues:LazyQuicksort = null

  val adjustMaxValueSlider = new JSlider(SwingConstants.VERTICAL,1,100,100)
  adjustMaxValueSlider.setMajorTickSpacing(10)
  adjustMaxValueSlider.setMinorTickSpacing(1)
  adjustMaxValueSlider.setPaintTicks(true)
  adjustMaxValueSlider.addChangeListener(new ChangeListener {
    /*
      This method starts by sorting a part of the yValues stocked in the graphic object using the LazyQuicksort method.
      Then it selects the last value sorted and set it as the maxYValueDisplayed
      so that only the wanted values will be displayed.
     */
    override def stateChanged(e: ChangeEvent): Unit = {
      sortedYValues = new LazyQuicksort(graphic.yValues.toArray,+_)
      sortedYValues.sortUntil(Math.max(1,(adjustMaxValueSlider.getValue.toDouble/100 * sortedYValues.size).toInt))
      val iteratorXValues = sortedYValues.iterator
      for(i <- 0 until Math.max(1, (adjustMaxValueSlider.getValue.toDouble / 100 * sortedYValues.size).toInt)){
        val temp = iteratorXValues.next()
        if(i == (adjustMaxValueSlider.getValue.toDouble/100 * sortedYValues.size).toInt-1) {
          graphic.maxYValueDisplayed = temp
        }
      }
      graphic.drawGlobalCurve()
    }
  })
  add(adjustMaxValueSlider, BorderLayout.EAST)

  //finally, we start the thread that performs the display operations.
  new Thread(this,"Graphic Thread").start()
}
