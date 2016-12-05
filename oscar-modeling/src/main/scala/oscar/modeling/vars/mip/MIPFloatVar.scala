package oscar.modeling.vars.mip

import oscar.algebra.{Solution, VarNumerical}
import oscar.algo.search.FloatConstrainableContext
import oscar.linprog.MPModel
import oscar.modeling.vars.FloatVarImplem
import oscar.modeling.vars.mip.MIPFloatVar.MIPSolutionHolder

/**
  * A float ("continuous") variable for the MIP Solver
  */
class MIPFloatVar(val realMipVar: VarNumerical, solutionHolder: MIPSolutionHolder) extends MIPVar with FloatVarImplem {
  override def context: FloatConstrainableContext = null

  /**
    * @return true if the domain of the variable has exactly one value,
    *         false if the domain has more than one value
    */
  override def isBound: Boolean = solutionHolder.getSolution.isDefined

  /**
    * Return a *lower bound* for this expression
    */
  override def min: Double = if(isBound) value() else realMipVar.lowerBound

  /**
    * Return a *higher bound* for this expression
    */
  override def max: Double = if(isBound) value() else realMipVar.upperBound

  /**
    * Test if a value is in the domain
    *
    * @param value : value to test
    * @return true if the domain contains the value val, false otherwise
    */
  override def hasValue(value: Double): Boolean = value >= min && value <= max

  /**
    * @return returns the set this variable represents, if it is bound
    */
  override def value(): Double = solutionHolder.getSolution.get.apply(realMipVar)

  /**
    * Return a representative name for this var(-like), if one was given
    */
  override def name: String = realMipVar.name
}

object MIPFloatVar {
  def apply(min: Double, max: Double, name: String, solver: MPModel[_], solutionHolder: MIPSolutionHolder): MIPFloatVar = {
    new MIPFloatVar(VarNumerical(name, min, max)(solver), solutionHolder)
  }

  trait MIPSolutionHolder {
    def getSolution: Option[Solution[Double]]
  }
}