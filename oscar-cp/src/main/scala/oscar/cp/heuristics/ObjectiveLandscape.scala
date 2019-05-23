package oscar.cp.heuristics

import oscar.algo.Inconsistency
import oscar.cp.constraints.LeEq
import oscar.cp.core.CPSolver
import oscar.cp.core.variables.CPIntVar

import util.control.Breaks._
import scala.collection.mutable.ListBuffer



class ObjectiveLandscape(solver:CPSolver, variables: Array[CPIntVar]) {

  private[this] val isDecision = Array.fill(variables.length)(false)

  private[this] val size = variables.length
  private[this] val isMin = solver.objective.objs.head.isMin
  private[this] val objVar = solver.objective.objs.head.objVar
  // lower bound of the objective function
  private[this] var Zl = if(isMin) solver.objective.objs.head.domBest else solver.objective.objs.head.domWorst
  // upper bound of the objective function
  private[this] val Zu = if(isMin)  solver.objective.objs.head.domWorst else solver.objective.objs.head.domBest

  var sum = 0
  for(i <- variables.indices) {
    sum += variables(i).getSize
  }

  var k = Zl
  breakable {
    while(k < Zu) {
      try {
        solver.pushState()
        solver.post(new LeEq(objVar, k))
        solver.pop()
        break
      }
      catch {
        case _: Inconsistency =>
          solver.pop()
          k += 1
      }
    }
  }
  Zl = k

  private[this] var Zj = getZValues()


  // number of points computed for the landscape
  private[this] var nPoints = Zj.length
  // lower bounds of variables for a given propagation
  private[this] val Xl = Array.ofDim[Int](size, nPoints)
  // upper bounds of the variables for a given propagation
  private[this] val Xu = Array.ofDim[Int](size, nPoints)

  // objective landscape
  private[this] val L = Array.ofDim[Map[Int, Int]](size)
  for(i <- variables.indices) {
    L(i) = Map.empty[Int, Int]
  }


  // objective landscape basic computation
  var j = 0
  while(j < Zj.length) {
    solver.pushState()
    for (i <- variables.indices) {
      solver.post(new LeEq(objVar, Zj(j)))
      Xl(i)(j) = variables(i).getMin
      Xu(i)(j) = variables(i).getMax
    }
    j += 1
  }

  // objective landscape final computation
  for(i <- variables.indices) {
    val values = Array.ofDim[Int](variables(i).getSize)
    variables(i).fillArray(values)
    for(v <- values) {
      if(v >= Xl(i)(Zj.length-1) && v <= Xu(i)(nPoints - 1)) {
        L(i) += (v -> Zl)
      }
      else if( v < Xl(i)(nPoints-1)) {
        println("XXXXXX")
        isDecision(i) = true
        breakable {
          for (j <- nPoints - 1 to 0 by -1) {
            if (Xl(i)(j) <= v) {
              L(i) += (v -> Zj(j))
              break
            }
          }
        }
      }
      else if(v > Xu(i)(nPoints -1)) {
        println("XXXXXX")
        isDecision(i) = true
        breakable {
          for (j <- nPoints - 1 to 0 by -1) {
            if (Xu(i)(j) >= v) {
              L(i) += (v -> Zj(j))
              break
            }
          }
        }
      }
    }
  }

  private def getZValues(): List[Int] = {
    var ret = ListBuffer[Int]()
    var j = 1
    var zj = ((Zu - Zl) / scala.math.pow(2,j-1)).toInt + Zl
    while(zj != Zl) {
      ret += zj
      j += 1
      zj = ((Zu - Zl) / scala.math.pow(2,j-1)).toInt + Zl
    }
    ret += Zl
    ret.toList
  }

  def selectValue(i:Int):Int = {
    val m = L(i)
    val tmp = Array.ofDim[Int](variables(i).getSize)
    var lowest = Int.MaxValue
    var bestV = 0
    for(v <- tmp) {
      val g = m.getOrElse(v, Int.MaxValue)
      if(g < lowest) {
        bestV = v
        lowest = g
      }
    }
    bestV
  }

  def getDecisionVars:Array[Boolean] = {
    isDecision
  }

}
