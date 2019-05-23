package oscar.cp.heuristics

import oscar.cp.core.CPSolver
import oscar.cp.core.variables.{CPIntVar, CPVar}

class WeightedDegree(cp:CPSolver, variables:Array[CPIntVar], decay:Double) {

  // WDEG
  private[this] val nVariables = variables.length
  private[this] val degree:Array[Double] = Array.tabulate(nVariables)(i => variables(i).constraintDegree)
  private[this] val map = variables.zipWithIndex.map(y => (y._1.asInstanceOf[CPVar], y._2)).toMap
  private[this] var min = Double.MaxValue
  private[this] var max = Double.MinValue

  for(i <- variables.indices) {
    degree(i) = variables(i).constraintDegree
    if(degree(i) < min) min = degree(i)
    else if(degree(i) > max) max = degree(i)
  }


  cp.onFailure(onFailure())


  private def onFailure():Unit = {
    min = Double.MaxValue
    max = Double.MinValue
    for(i <- variables.indices) {
      degree(i) = degree(i)*decay
      if(cp.lastConstraintCalled != null) {
        for(v <- cp.lastConstraintCalled.associatedVars()) {
          val op = map.get(v)
          val index = op.getOrElse(-1)
          if (index >= 0) degree(index) += 1
        }
      }
      if(degree(i) < min) min = degree(i)
      else if(degree(i) > max) max = degree(i)
    }
  }

  def getDomWeightedDegree(i:Int):Double = {
    variables(i).getSize.toDouble / degree(i)
  }

  def setDegree(values: Array[Double]):Unit = {
    min = Double.MaxValue
    max = Double.MinValue
    for(i <- values.indices) {
      degree(i) = values(i)
      if(degree(i) < min) min = degree(i)
      else if(degree(i) > max) max = degree(i)
    }
  }

  def getScaledDegree(i:Int):Double = {
    (degree(i)) - min / (max - min)
  }

}
