package oscar.modeling.vars.mip

import oscar.algo.search.FloatConstrainableContext
import oscar.linprog.interface.MPSolverInterface
import oscar.linprog.modeling.{MPFloatVar, MPSolver}
import oscar.modeling.vars.FloatVarImplem

/**
  * A float ("continuous") variable for the MIP Solver
  */
class MIPFloatVar[I <: MPSolverInterface](val realMipVar: oscar.linprog.modeling.MPVar[I]) extends MIPVar with FloatVarImplem {
  override def context: FloatConstrainableContext = null

  /**
    * @return true if the domain of the variable has exactly one value,
    *         false if the domain has more than one value
    */
  override def isBound: Boolean = realMipVar.value.isDefined

  /**
    * Return a *lower bound* for this expression
    */
  override def min: Double = if(isBound) value() else realMipVar.bounds._1

  /**
    * Return a *higher bound* for this expression
    */
  override def max: Double = if(isBound) value() else realMipVar.bounds._2

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
  override def value(): Double = realMipVar.value.get

  /**
    * Return a representative name for this var(-like), if one was given
    */
  override def name: String = realMipVar.name
}

object MIPFloatVar {
  def apply[I <: MPSolverInterface](min: Double, max: Double, name: String, solver: MPSolver[I]): MIPFloatVar[I] = {
    new MIPFloatVar(MPFloatVar(name, min, max)(solver))
  }
}