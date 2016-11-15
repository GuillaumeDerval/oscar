package oscar.algo.branchings

import oscar.algo.search._
import oscar.algo.vars.IntVarLike

/**
 * Binary search on the decision variables vars, splitting the domain at the selected value (left : <= value, right : > value)
 */
class BinaryDomainSplitBranching(variables: Array[IntVarLike], varHeuris: (Int => Int), valHeuris: (Int => Int)) extends BinaryBranching(variables, varHeuris, valHeuris) {

  def this(x: Array[IntVarLike], varHeuris: (Int => Int)) = this(x, varHeuris, i => (x(i).min + x(i).max) / 2)

  final override def alternatives(): Seq[Alternative] = {
    if (allBounds()) noAlternative
    else {
      val i = nextVar()
      val variable = variables(i)
      val value = valHeuris(i)
      List(Decision.lowerEq(variable, value), Decision.greaterEq(variable, value + 1))
    }
  }
}