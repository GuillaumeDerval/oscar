package solvers.cp.branchings

import models.CPModel
import solvers.cp.branchings.Branching.Alternative
import vars.IntVar

/**
  * Abstract Binary Branching:
  * You can specify your variable heuristics
  *
  * @author Pierre Schaus pschaus@gmail.com
  * @author Renaud Hartert ren.hartert@gmail.com
  * @author Guillaume Derval guillaume@guillaumederval.be
  * @param varHeuris is a variable heuristic, it will select preferably first the unbound
  *        variables(i) such that varHeuris(i) is the smallest
  */
class BinaryBranching(cp: CPModel, variables: Array[IntVar], var varHeuris: (Int => Int), valHeuris: (Int => Int)) extends Branching {
  private[this] val nVariables = variables.length
  private[this] val indexes = Array.tabulate(nVariables)(i => i)
  private[this] val nBounds = cp.getReversibleInt(0)

  @inline private def bound(i: Int): Unit = {
    val id = nBounds.incr() - 1
    val tmp = indexes(id)
    indexes(id) = indexes(i)
    indexes(i) = tmp
  }

  protected def allBounds(): Boolean = {
    var i = nBounds.value
    while (i < nVariables) {
      val varId = indexes(i)
      val variable = variables(varId)
      if (variable.isBound) bound(i)
      else return false
      i += 1
    }
    true
  }

  protected def nextVar(): Int = {
    var i = nBounds.value
    var bestId = indexes(i)
    var bestVariable = variables(bestId)
    var bestH = varHeuris(bestId)
    i += 1
    while (i < nVariables) {
      val varId = indexes(i)
      val variable = variables(varId)
      if (variable.isBound) bound(i)
      else {
        val h = varHeuris(varId)
        if (h < bestH || (h == bestH && varId < bestId)) {
          bestVariable = variable
          bestId = varId
          bestH = h
        }
      }
      i += 1
    }
    bestId
  }

  def alternatives(): Seq[Alternative] = {
    if (allBounds()) Branching.noAlternative
    else {
      val i = nextVar()
      val variable = variables(i)
      val value = valHeuris(i)
      Branching.branch(cp.post(variable === value))(cp.post(variable != value))
    }
  }
}