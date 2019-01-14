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

import java.awt._
import java.awt.event.{MouseEvent, MouseListener}
import java.awt.geom.Line2D.Double
import java.awt.geom.Rectangle2D

import oscar.cbls.util.StopWatch
import oscar.visual.VisualDrawing
import oscar.visual.shapes.{VisualLine, VisualRectangle, VisualShape, VisualText}

import scala.collection.mutable.ListBuffer

/** This abstract class represent the base structure for
  * the classes who'll have the purpose of drawing the curve of a function.
  * It has many variables :
  * maxWidth : it represents the right horizontal drawing limit of the curve
  * minWidth : it represents the left horizontal drawing limit of the curve
  * diffWidth : the difference between the maxWidth and the minWidth
  * maxHeight : it represents the bottom vertical drawing limit of the curve
  * minHeight : it represents the top vertical drawing limit of the curve
  * diffHeight : the difference between the maxHeight and the minHeight
  * xValues : a ListBuffer that contains all the X values encountered so far
  * yValues : a ListBuffer that contains all the Y values encountered so far
  * maxXValueDisplayed : the maximum X value that will be displayed on the graphic
  * minXValueDisplayed : the minimum X value that will be displayed on the graphic
  * diffXValueDisplayed : the difference between the maxXValueDisplayed and the minXValueDisplayed
  * maxYValueDisplayed : the maximum Y value that will be displayed on the graphic
  * minYValueDisplayed : the minimum Y value that will be displayed on the graphic
  * diffYValueDisplayed : the difference between the maxYValueDisplayed and the minYValueDisplayed
  * maxXValue :  the maximum X value registered so far
  * minXValue : the minimum X value registered so far
  * diffXValue : the difference between the maxXValue and the minXValue
  * maxYValue : the maximum Y value registered so far
  * minYValue : the minimum Y value registered so far
  * diffYValue : the difference between the maxYValue and the minYValue
  * widthAdapter : an anonymous function that return the X position of a point in pixel
  *                based on the minXValueDisplayed and the maxXValueDisplayed values
  * heightAdapter : an anonymous function that return the Y position of a point in pixel
  *                based on the minYValueDisplayed and the maxYValueDisplayed
  *
  * @author fabian.germeau@student.vinci.be
  */

abstract class FunctionGraphic() extends VisualDrawing(false,false) with StopWatch{

  val maxWidth = () => getWidth -10L
  val minWidth = 70L
  val diffWidth = () => maxWidth() - minWidth
  val maxHeight = () => getHeight - 30L
  val minHeight = 10L
  val diffHeight = () => maxHeight() - minHeight


  val xValues:ListBuffer[Long] = new ListBuffer[Long]
  val yValues:ListBuffer[Long] = new ListBuffer[Long]

  var maxXValueDisplayed:Long = 0L
  var minXValueDisplayed:Long = 0L
  val diffXValueDisplayed = () => maxXValueDisplayed - minXValueDisplayed
  var maxYValueDisplayed:Long = 0L
  var minYValueDisplayed:Long = 0L
  val diffYValueDisplayed = () => maxYValueDisplayed - minYValueDisplayed

  var minAbs:Long = 0L
  var maxAbs:Long = 0L
  val diffAbs = () => maxAbs - minAbs
  var minOrd:Long = 0L
  var maxOrd:Long = 0L
  val diffOrd = () => maxOrd - minOrd

  val maxXValue = () => if(xValues.isEmpty)0L else xValues.max
  val minXValue = () => if(xValues.isEmpty)0L else xValues.min
  val diffXValue = () => maxXValue() - minXValue()
  val maxYValue = () => if(yValues.isEmpty)0L else yValues.max
  val minYValue = () => if(yValues.isEmpty)0L else yValues.min
  val diffYValue = () => maxYValue() - minYValue()

  val widthAdapter = (value:Long) => {
    ((value - minXValueDisplayed)*diffWidth().toDouble/Math.max(diffWidth(),diffXValueDisplayed())).toInt
  }
  val heightAdapter = (value:Long) => {
    (value - minYValueDisplayed)*diffHeight().toDouble/Math.max(diffHeight(),diffYValueDisplayed())
  }

  //A list buffer that contains the color of the neighborhood that has found the current move
  val xColorValues:ListBuffer[Color] = new ListBuffer[Color]

  setLayout(new BorderLayout())

  //We remove the unwanted listener inherited from VisualDrawing
  removeMouseListener(getMouseListeners.head)
  removeMouseMotionListener(getMouseMotionListeners.head)

  def notifyNewObjectiveValue(objValue:Long, objTime:Long)

  def clear(): Unit ={
    super.clear()
  }

  override def addShape(shape: VisualShape, repaintAfter: Boolean = true): Unit ={
    super.addShape(shape,false)
  }

  def drawGlobalCurve(): Unit ={
    //repaint()
  }

  def setTimeBorders(position:Long){}

  def setMaxNumberOfObject(percentage:scala.Double){}
}

/** This class has the purpose to draw the objective function curve.
  * It firstly collect all the objective value.
  * To do so, the search procedure has to call the notifyNewObjectiveValue with the objective value and the current time.
  * After that it draws the entire curve with the drawObjectiveCurve method
  *
  * @author fabian.germeau@student.vinci.be
  */

class ObjFunctionGraphic() extends FunctionGraphic(){

  //A list buffer that contains all the bests objective values encountered during the search
  val bestValues:ListBuffer[Long] = new ListBuffer[Long]

  //The current best objective value of the search
  val best = () => {
    if(bestValues.nonEmpty)
      bestValues.min
    else
      Long.MaxValue
  }


  //A variable and a MouseListener used to allow the user to display or not the best curve
  var displayBest = true
  addMouseListener(new MouseListener{
    override def mouseExited(e: MouseEvent): Unit = {}

    override def mouseClicked(e: MouseEvent): Unit = {
      if(e.getX <= minWidth && e.getY >= maxHeight()){
        displayBest = !displayBest
        drawGlobalCurve()
      }
    }

    override def mouseEntered(e: MouseEvent): Unit = {}

    override def mousePressed(e: MouseEvent): Unit = {}

    override def mouseReleased(e: MouseEvent): Unit = {}
  })

  /**
    * Clear the graphic and reset the different ListBuffers in order to begin another research
    */
  override def clear(): Unit ={
    super.clear()
    yValues.clear()
    xValues.clear()
    bestValues.clear()
    xColorValues.clear()
  }

  /**
    * Save the objective value, the best value encountered so far, the time value of the current state
    * and the color of the neighborhood encountered (also in the xColorMap if it is not already registered)
    */
  def notifyNewObjectiveValue(objValue:Long, time:Long): Unit ={
    xValues.append(time)
    yValues.append(objValue)
    bestValues.append(Math.min(best(),objValue))
    maxXValueDisplayed = time
    minYValueDisplayed = best()
    maxYValueDisplayed = yValues.max
  }

  /**
    * Prepare the different values needed to draw the global curve.
    * It's similar to the previous method but this time the goal is to draw the entire curve.
    */
  override def drawGlobalCurve() = {
    super.clear()

    val (bottom,top) = getOrdFloorValues(minYValueDisplayed,maxYValueDisplayed)
    val (left,right) = getAbsFloorValues(minXValueDisplayed,maxXValueDisplayed)

    minAbs = left
    maxAbs = right
    minOrd = bottom
    maxOrd = top

    drawStatistics()
    drawCurve()
    drawAxis()
    super.drawGlobalCurve()
  }

  /**
    * Draw the global curve, including all the values
    */
  def drawCurve(): Unit = {
    var currentTimeUnit:scala.Double = widthAdapter(0L)
    var currentTimeUnitValue:scala.Double = 0.0
    var currentTimeUnitValuesNumber:scala.Double = 0.0
    var currentTimeUnitBestValue:scala.Double = 0.0
    var previousTimeUnitValue:scala.Double = 0.0
    var previousTimeUnitBestValue:scala.Double = 0.0
    var previousTimeUnit:scala.Double = 0.0
    for(i <- yValues.indices){
      if(widthAdapter(xValues(i)) != currentTimeUnit && currentTimeUnitValuesNumber != 0L) {
        currentTimeUnitValue = maxHeight() - (currentTimeUnitValue/currentTimeUnitValuesNumber) + heightAdapter(minYValueDisplayed)
        currentTimeUnitBestValue = maxHeight() - (currentTimeUnitBestValue/currentTimeUnitValuesNumber) + heightAdapter(minYValueDisplayed)
        if(previousTimeUnitValue == 0.0) {
          previousTimeUnitValue = currentTimeUnitValue
          previousTimeUnitBestValue = currentTimeUnitBestValue
          previousTimeUnit = currentTimeUnit
        }

        if(displayBest){
          val bestLine = new VisualLine(this,new Double(previousTimeUnit+70L, previousTimeUnitBestValue, currentTimeUnit+70L, currentTimeUnitBestValue))
          bestLine.outerCol_$eq(Color.green)
        }
        val line = new VisualLine(this, new Double(previousTimeUnit+70L, previousTimeUnitValue, currentTimeUnit+70L, currentTimeUnitValue))
        //line.outerCol_$eq(xColorValues(i))
        line.borderWidth = 3L

        previousTimeUnit = currentTimeUnit
        previousTimeUnitValue = currentTimeUnitValue
        previousTimeUnitBestValue = currentTimeUnitBestValue
        currentTimeUnitValue = 0.0
        currentTimeUnitValuesNumber = 0.0
        currentTimeUnitBestValue = 0.0
      }
      while(currentTimeUnit < widthAdapter(xValues(i)))
        currentTimeUnit += 1L
      currentTimeUnitValue += heightAdapter(yValues(i))
      currentTimeUnitValuesNumber += 1L
      currentTimeUnitBestValue += heightAdapter(bestValues(i))
    }

    //We draw the last position
    if(displayBest){
      val bestLine = new VisualLine(this,new Double(previousTimeUnit+70L, previousTimeUnitBestValue, currentTimeUnit+70L, maxHeight() - (currentTimeUnitBestValue/currentTimeUnitValuesNumber) + heightAdapter(minYValueDisplayed)))
      bestLine.outerCol_$eq(Color.green)
    }
    val line = new VisualLine(this, new Double(previousTimeUnit+70L, previousTimeUnitValue, currentTimeUnit+70L, maxHeight() - (currentTimeUnitValue/currentTimeUnitValuesNumber) + heightAdapter(minYValueDisplayed)))
    //line.outerCol_$eq(xColorValues.last)
    line.borderWidth = 3L
  }

  /**
    * Return the values of the Y axis's border
    *
    * @param minObjValue The minimum objective value that will be drawn
    * @param maxObjValue The maximum objective value that will be drawn
    * @return The bottom and top border of the Y axis
    */
  def getOrdFloorValues(minObjValue:Long, maxObjValue:Long): (Long,Long) ={
    if(diffYValueDisplayed() == 0L){
      (minObjValue - 5L, minObjValue + 5L)
    }else {
      var diffFloor:Long = 1L
      while (diffFloor <= diffYValueDisplayed()){
        diffFloor *= 10L
      }
      var minFloor = 1L
      //to prevent the case when the min and the max value are too different (like max = 15000L and min 100L)
      while (minObjValue / minFloor > 0L) {
        minFloor *= 10L
      }
      val df = Math.max(diffFloor / 10L, 10L)
      (Math.max((minObjValue / df) * df, (minObjValue / minFloor) * minFloor), ((maxObjValue / df) * df) + df)
    }
  }

  /**
    * Return the value of the X axis's border
    *
    * @param minTimeValue The minimum objective's time of occurrence that will be drawn
    * @param maxTimeValue The maximum objective's time of occurrence that will be drawn
    * @return The left and right border of the X axis
    */
  def getAbsFloorValues(minTimeValue:scala.Long, maxTimeValue:scala.Long): (scala.Long, scala.Long) ={
    var diffFloor = 1L
    while(diffFloor <= diffXValueDisplayed())
      diffFloor *= 10L
    var minFloor = 1L
    while(minTimeValue/minFloor > 0.0){
      minFloor *= 10L
    }
    val df = Math.max(diffFloor / 10L, 1L)
    (Math.max((minTimeValue / df) * df, (minTimeValue / minFloor) * minFloor), ((maxTimeValue / df) * df) + df)
  }

  /**
    * Draw the axis. We draw blank rectangle to erase the parts of the curve that overstep the axes.
    */
  def drawAxis(): Unit ={
    val rectLeft = new VisualRectangle(this, new Rectangle2D.Double(0L,0L,minWidth,getHeight))
    rectLeft.innerCol_=(Color.white)
    rectLeft.outerCol_=(Color.white)
    val rectBottom = new VisualRectangle(this, new Rectangle2D.Double(0L,maxHeight(),getWidth,getHeight))
    rectBottom.innerCol_=(Color.white)
    rectBottom.outerCol_=(Color.white)
    val ordLine = new VisualLine(this,new Double(minWidth,minHeight,minWidth,maxHeight()))
    ordLine.outerCol_$eq(Color.black)
    val absLine = new VisualLine(this,new Double(minWidth,maxHeight(),maxWidth(),maxHeight()))
    absLine.outerCol_$eq(Color.black)

    //We determine the step of the scale
    val objStep = diffOrd()/10L
    for(i <- 1L to 10L){
      val scaleHeight = maxHeight() - heightAdapter((i*objStep) + minOrd)
      new VisualText(this,5L,scaleHeight,(minOrd+(objStep*i)).toString,false,new Rectangle2D.Double(0L, 0L, 1L, 1L))
      new VisualLine(this,new Double(minWidth-10L,scaleHeight,minWidth,scaleHeight))
      val smallObjStep = objStep/10L
      for(j <- 1L to 9L){
        val scaleHeight = maxHeight() - heightAdapter(((i-1L)*objStep) + j*smallObjStep + minOrd)
        new VisualLine(this,new Double(minWidth-5L,scaleHeight,minWidth,scaleHeight))
      }
    }
    val timeStep = diffAbs()/10L
    for(i <- 1L to 10L){
      val scaleWidth = widthAdapter((i*timeStep) + minAbs)
      new VisualText(this,scaleWidth,maxHeight()+20L,((timeStep*i) + minAbs).toString,true,new Rectangle2D.Double(0L, 0L, 1L, 1L))
      new VisualLine(this,new Double(scaleWidth,maxHeight(),scaleWidth,maxHeight()+10L))
      val smallTimeStep = timeStep/10L
      for(j <- 1L to 9L){
        val scaleWidth = widthAdapter(((i-1L)*timeStep) + (j*smallTimeStep) + minAbs)
        new VisualLine(this,new Double(scaleWidth,maxHeight(),scaleWidth,maxHeight()+5L))
      }
    }
    val rectTop = new VisualRectangle(this, new Rectangle2D.Double(minWidth,0L,getWidth,8L))
    rectTop.innerCol_=(Color.white)
    rectTop.outerCol_=(Color.white)
    val rectRight = new VisualRectangle(this, new Rectangle2D.Double(maxWidth(),0L,getWidth,getHeight))
    rectRight.innerCol_=(Color.white)
    rectRight.outerCol_=(Color.white)
    val rectBottomLeft = new VisualRectangle(this, new Rectangle2D.Double(0L,maxHeight(),minWidth,getHeight))
    val bestText = new VisualText(this,minWidth/2L,maxHeight()+20L,"Best",true,new Rectangle2D.Double(0L,0L,1L,1L))
    bestText.fontColor= if (displayBest) Color.green else Color.red
  }

  /**
    * Draw the neighborhood statistics. For each time step, the method count the number of occurences of each neighborhood encountered
    * And whit this information it draws small rectangles in the background representing the proportion of each neighborhood.
    */
  def drawStatistics(): Unit ={
    val timeStep = diffAbs()/10L
    val colorSums:Array[Map[Color,Long]] = Array.tabulate(10L)(n => Map[Color,Long]())
    var i = 0L

    while(i < xColorValues.size && Math.floor((xValues(i) - minAbs)/timeStep).toInt < 10L){
      val columnNumber = Math.floor((xValues(i) - minAbs)/timeStep).toInt
      if(columnNumber >= 0L){
        colorSums(columnNumber).get(xColorValues(i)) match{
          case Some(s:Long) => colorSums(columnNumber) += xColorValues(i) -> (1L + s)
          case None => colorSums(columnNumber) += xColorValues(i) -> 1L
        }
      }
      i += 1L
    }
    val totalColors = colorSums.map(_.foldLeft(0L)((c,d) => c + d._2))
    val maxColor  = totalColors.max
    for(i <- 1L to 10L){
      val leftBorder = Math.max(widthAdapter((i*timeStep) + minAbs),minWidth)
      val rightBorder = widthAdapter(((i+1L)*timeStep) + minAbs)-leftBorder
      var previousHeight:Float = 0L
      for(color <- colorSums(i-1L)){
        val colorHeight:Float = color._2/maxColor.toFloat
        previousHeight += colorHeight
        val coloredHistogram = new VisualRectangle(this,new Rectangle2D.Double(
            leftBorder, maxHeight() - previousHeight*diffHeight(),
            rightBorder, colorHeight*diffHeight()))
        coloredHistogram.innerCol = color._1
        coloredHistogram.outerCol = color._1
      }
      new VisualLine(this,new Double(leftBorder,maxHeight(),leftBorder,minHeight)).outerCol = Color.white
    }
  }
}
