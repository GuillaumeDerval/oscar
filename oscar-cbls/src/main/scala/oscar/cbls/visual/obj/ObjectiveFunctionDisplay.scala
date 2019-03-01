package oscar.cbls.visual.obj

import org.jfree.chart.plot.PlotOrientation
import org.jfree.chart.ChartFactory
import oscar.visual.plot.Plot
import oscar.cbls._
import oscar.cbls.util.StopWatch

class ObjectiveFunctionDisplay(title: String) extends Plot(title,"Time","Objective function value", 2) with StopWatch {

  xDom = 0 to 1
  yDom = 0 to 100

  private val startinAt = getWatch
  private var best = Long.MaxValue

  def createChart =
    ChartFactory.createXYLineChart(title,"Time (s)","Objective function value",xyDataset,PlotOrientation.VERTICAL,true,false, false);

  def drawFunction(value: Long) ={
    if(value < best) best = value
    val at = (getWatch - startinAt).toDouble/1000
    if(yDom.getUpperBound < value.toDouble)
      yDom = Range.inclusive(0L,upper(value))
    if(xDom.getUpperBound < at)
      xDom = Range.inclusive(0L,upper(at))
    addPoint(at,value.toDouble)
    addPoint(at,best.toDouble,1)
  }

  private def upper(value: Double): Long ={
    var resExp = 10L
    while(value/resExp > 1) {
      resExp = resExp * 10
    }
    resExp = resExp/10
    println(resExp)
    var res = resExp
    while(value/res > 1){
      res += resExp
    }
    println(value)
    println(res)
    res
  }
}

object ObjectiveFunctionDisplay{
  def apply(title: String): ObjectiveFunctionDisplay = new ObjectiveFunctionDisplay(title)
}
