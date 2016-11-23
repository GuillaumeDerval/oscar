package oscar.modeling.solvers.cp

import oscar.modeling.models.ModelDeclaration
import oscar.modeling.solvers.cp.decompositions.DecompositionStrategy
import oscar.modeling.solvers.cp.distributed.DecomposedCPSolve

/**
  * Stores a CP Decomposition, or redirect to the ModelDeclaration if it implements DecomposedCPSolve
  */
trait CPDecompositionHolder {
  val md: ModelDeclaration
  private var _cpDecomp: DecompositionStrategy = null
  def setCPDecompositionStrategy(d: DecompositionStrategy): Unit = _cpDecomp = d
  def getCPDecompositionStrategy: DecompositionStrategy = {
    if(_cpDecomp == null && md.isInstanceOf[DecomposedCPSolve[_]])
      md.asInstanceOf[DecomposedCPSolve[_]].getDecompositionStrategy
    else
      _cpDecomp
  }
}
