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

import java.awt.BorderLayout
import javax.swing.event.{ChangeEvent, ChangeListener}
import javax.swing.{JSlider, SwingConstants}

import oscar.cbls.algo.search.LazyQuicksort


/**
  * This file contains all the add-on modules that you could add to your FunctionGraphicContainer object.
  * This way you can build any FunctionGraphicContainer object you want just by adding the desired trait on the construction.
  *
  * @author fabian.germeau@student.vinci.be
  */


/**
  * This trait, attached to a ObjFunctionGraphicContainer add three scrollbar to the graphic.
  * The scrollbar on the right allow the user to zoom in the graphic
  * (quite useful if you want to look closer on specifics parts of the graphic).
  * The scrollbar on the left allow the user to move up and down in the graphic (except when all the graphic is displayed)
  * Similarly the scrollbar on the bottom allow the user to move left and right
  *
  * @author fabian.germeau@student.vinci.be
  */
trait Zoom extends ObjFunctionGraphicContainer{
  val zoomSlider = new JSlider(SwingConstants.VERTICAL,1,100,1)
  zoomSlider.setMajorTickSpacing(10)
  zoomSlider.setMinorTickSpacing(1)
  zoomSlider.setPaintTicks(true)
  zoomSlider.addChangeListener(new ChangeListener{
    override def stateChanged(e: ChangeEvent): Unit = {
      adjustScrollBar()
      graphic.minXValueDisplayed = Math.max(Math.floor((graphic.maxXValue()/rightLeftSlider.getMaximum)*(rightLeftSlider.getValue-1)).toLong,graphic.minXValue())
      graphic.maxXValueDisplayed = Math.max(Math.ceil((graphic.maxXValue()/rightLeftSlider.getMaximum)*rightLeftSlider.getValue).toLong,graphic.diffWidth()+graphic.minXValue())
      graphic.minYValueDisplayed = Math.max(Math.floor((graphic.maxYValue()/upDownSlider.getMaximum)*(upDownSlider.getValue-1)).toLong,graphic.minYValue())
      graphic.maxYValueDisplayed = Math.min(Math.ceil((graphic.maxYValue()/upDownSlider.getMaximum)*upDownSlider.getValue).toLong,graphic.maxYValue())
    }
  })
  add(zoomSlider, BorderLayout.EAST)

  val rightLeftSlider = new JSlider(SwingConstants.HORIZONTAL,1,1,1)
  rightLeftSlider.setPaintTicks(true)
  rightLeftSlider.addChangeListener(new ChangeListener {
    override def stateChanged(e: ChangeEvent): Unit = {
      graphic.minXValueDisplayed = Math.max(Math.floor((graphic.maxXValue()/rightLeftSlider.getMaximum)*(rightLeftSlider.getValue-1)).toLong,graphic.minXValue())
      graphic.maxXValueDisplayed = Math.max(Math.ceil((graphic.maxXValue()/rightLeftSlider.getMaximum)*rightLeftSlider.getValue).toLong,graphic.diffWidth()+graphic.minXValue())
      graphic.drawGlobalCurve()
    }
  })
  add(rightLeftSlider, BorderLayout.SOUTH)

  val upDownSlider = new JSlider(SwingConstants.VERTICAL,1,1,1)
  upDownSlider.setPaintTicks(true)
  upDownSlider.addChangeListener(new ChangeListener{
    override def stateChanged(e: ChangeEvent): Unit = {
      graphic.minYValueDisplayed = Math.max(Math.floor((graphic.maxYValue()/upDownSlider.getMaximum)*(upDownSlider.getValue-1)).toLong,graphic.minYValue())
      graphic.maxYValueDisplayed = Math.min(Math.ceil((graphic.maxYValue()/upDownSlider.getMaximum)*upDownSlider.getValue).toLong,graphic.maxYValue())
      graphic.drawGlobalCurve()
    }
  })
  add(upDownSlider, BorderLayout.WEST)

  /*
    This method, called by the zoomScrollBar adjust the values of the rightLeftScrollBar and the upDownScrollBar
    so that their values are adjusted to the new zoom level.
    After adjusting the values, it sets the value of the scrollbar to 0 (top left of the graphic)
   */
  def adjustScrollBar(): Unit ={
    rightLeftSlider.setValue(1)
    upDownSlider.setValue(1)
    rightLeftSlider.setMaximum(Math.min(Math.ceil(graphic.diffXValue()/graphic.diffWidth()), getLogZoom(zoomSlider.getValue)).toInt)
    rightLeftSlider.setMajorTickSpacing(rightLeftSlider.getMaximum/100)
    rightLeftSlider.setMinorTickSpacing(rightLeftSlider.getMaximum/10)
    upDownSlider.setMaximum(Math.min(graphic.diffYValue(),getLogZoom(zoomSlider.getValue)).toInt)
    upDownSlider.setMajorTickSpacing(upDownSlider.getMaximum/100)
    upDownSlider.setMinorTickSpacing(upDownSlider.getMaximum/10)
  }

  /*
    This method set the maximum value of the moving scrollbar to the maximum value of their related axis.
    This way at each position we can easily have the minimum X/Y value to display and, with the extent value
    of the scrollbar, the maximum X/Y value.
   */
  override def drawGlobalCurve(): Unit ={
    super.drawGlobalCurve()
  }

  /**
    * This method calculates the new zoom level based on a logarithmic calculation
    *
    * @param i the position of the zoomScrollBar
    * @return the new zoom level
    */
  def getLogZoom(i:Double): Double ={
    if(i == 0)1
    else Math.max(1.0,100*Math.pow(Math.log(i), 2) / Math.pow(Math.log(100), 2))
  }
}

/**
  * This trait, attached to a ObjFunctionGraphicContainer add a scrollbar on the right side of the graphic.
  * This scrollbar allow the user to increase or decrease the number of value displayed on the graphic.
  * The number of displayed value is represented in percentages and always begin with the lower value.
  * The minimum value is 1% and the maximum value 100%.
  *
  * @author fabian.germeau@student.vinci.be
  */
trait AdjustMaxValue extends ObjFunctionGraphicContainer{
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

  override def drawGlobalCurve(): Unit ={
    super.drawGlobalCurve()
  }
}