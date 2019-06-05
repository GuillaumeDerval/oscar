package oscar.cp.heuristics
import oscar.algo.Inconsistency
import oscar.cp.constraints.{GrEq, LeEq}
import oscar.cp.core.CPSolver
import oscar.cp.core.variables.CPIntVar
import util.control.Breaks._
import scala.collection.mutable.ArrayBuffer

// Objective Landscapes from "Objective Landscapes for Constraint Programming"
class ObjectiveLandscape(solver:CPSolver, variables: Array[CPIntVar]) {

  private[this] val nPoints = 100
  private[this] val isMin = solver.objective.objs.head.isMin
  private[this] val objVar = solver.objective.objs.head.objVar
  private[this] val context = variables(0).context
  private[this] val size = variables.length
  // lower bound of the objective function
  private[this] var Zl = if(isMin) solver.objective.objs.head.domBest else solver.objective.objs.head.domWorst
  // upper bound of the objective function
  private[this] val Zu = if(isMin)  solver.objective.objs.head.domWorst else solver.objective.objs.head.domBest
  // sequence of zj values
  private[this] val points:Array[Int] = getPoints
  // lower and upper points or variables when f(x) <= zj is propagated
  private[this] val Xl = Array.ofDim[Int](size, points.length)
  private[this] val Xu = Array.ofDim[Int](size, points.length)
  // objective landscape function
  private[this] val L = Array.ofDim[Map[Int, Int]](size)
  for(i <- variables.indices) {
    L(i) = Map.empty[Int, Int]
  }
  private[this] var empty = false

  if(Zu == Zl || points.isEmpty) {
    empty = true
  }
  else{

    // objective landscape preliminary computation
    // compute Xl and Xu for all values  zj such that f(x) <= z does not fail
    if(isMin) {
      context.pushState()
      for(j <- points.indices) {

        solver.post(new LeEq(objVar, points(j)))
        for(i <- variables.indices) {
          Xl(i)(j) = variables(i).getMin
          Xu(i)(j) = variables(i).getMax
        }
      }
      context.pop()
    }
    else {
      context.pushState()
      for(j <- points.indices) {

        solver.post(new GrEq(objVar, points(j)))
        for(i <- variables.indices) {
          Xl(i)(j) = variables(i).getMin
          Xu(i)(j) = variables(i).getMax
        }
      }
      context.pop()
    }






    // objective landscape final computation
    for(i <- variables.indices) {
      val values = Array.ofDim[Int](variables(i).getSize)
      variables(i).fillArray(values)
      for(v <- values) {
        if(v >= Xl(i)(points.length-1) && v <= Xu(i)(points.length-1)) {
          L(i) += (v -> points(points.length-1))
        }
        else if( v < Xl(i)(points.length-1)) {
          breakable {
            for (j <- points.length-1 to 0 by -1) {
              if (Xl(i)(j) <= v) {
                L(i) += (v -> points(j))
                break
              }
            }
          }
        }
        else if(v > Xu(i)(points.length-1)) {
          breakable {
            for (j <- points.length-1 to 0 by -1) {
              if (Xu(i)(j) >= v) {
                L(i) += (v -> points(j))
                break
              }
            }
          }
        }
      }
    }



  }


  def isEmpty:Boolean = {
    empty
  }


  // computes the sequence of points zj
  private def getPoints: Array[Int] = {
    var j = 2
    val values = ArrayBuffer[Int]()
    context.pushState()

    if(isMin) {
      var zj = Zu

      while(zj > Zl && !context.isFailed) {

        zj = ((Zu - Zl) / scala.math.pow(2, j-1)).toInt + Zl
        try {
          solver.post(new LeEq(objVar, zj))
          values += zj
        }
        catch {
          case _: Inconsistency =>
            context.pop()
            return values.toArray
        }
        j += 1
      }
    }
    else {
      var zj = Zl
      j = 2
      while(zj < Zu && !context.isFailed) {

        zj = Math.abs(((Zu - Zl) / scala.math.pow(2, j-1)).toInt - Zu)

        try {
          solver.post(new GrEq(objVar, zj))
          values += zj
        }
        catch {
          case _: Inconsistency =>
            context.pop()
            return values.toArray
        }
        j += 1
      }
    }
    context.pop()
    values.toArray
  }


  // selects the value that optimizes the landscape value
  def selectValue(i:Int): Int = {
    val m = L(i)
    var bestValue = variables(i).getMin
    var bestScore = Integer.MAX_VALUE
    for((key, value) <- m) {
      val tmp = if(isMin) value else -value
      if(tmp < bestScore) {
        bestValue = key
        bestScore = tmp
      }
    }
    bestValue
  }

  // returns the landscape score which has to be minimized
  def getLandscapeScore(i:Int, v:Int): Int = {
    val ret = L(i).getOrElse(v, Integer.MAX_VALUE)
    if(isMin) ret else -ret
  }
}
