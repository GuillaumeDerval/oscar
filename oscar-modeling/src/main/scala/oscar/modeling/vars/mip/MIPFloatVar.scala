package oscar.modeling.vars.mip

import de.xypron.linopt.Problem
import oscar.algo.search.FloatConstrainableContext
import oscar.modeling.models.mip.MIPModel
import oscar.modeling.vars.FloatVarImplem

/**
  * A float ("continuous") variable for the MIP Solver
  */
class MIPFloatVar(baseMin: Double, baseMax: Double, override val name: String, mIPModel: MIPModel) extends MIPVar with FloatVarImplem {
  val realMipVar: Problem#Column = mIPModel.linProblem.column(name).`type`(Problem.ColumnType.FLOAT).bounds(baseMin, baseMax)
  override def context: FloatConstrainableContext = null

  /**
    * @return true if the domain of the variable has exactly one value,
    *         false if the domain has more than one value
    */
  override def isBound: Boolean = mIPModel.hasSolution

  /**
    * Return a *lower bound* for this expression
    */
  override def min: Double = if(isBound) value() else realMipVar.getLowerBound

  /**
    * Return a *higher bound* for this expression
    */
  override def max: Double = if(isBound) value() else realMipVar.getUpperBound

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
  override def value(): Double = realMipVar.getValue
}

object MIPFloatVar {
  def apply(baseMin: Double, baseMax: Double, name: String)(implicit mIPModel: MIPModel): MIPFloatVar = {
    new MIPFloatVar(baseMin, baseMax, name, mIPModel)
  }
}