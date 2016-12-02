package oscar.modeling.models

import oscar.modeling.algebra.Expression
import oscar.modeling.constraints.Constraint
import oscar.modeling.models.operators.ModelOperator

/**
  * Created by dervalguillaume on 17/11/16.
  */
trait ModelDeclarationInterface {

  /**
    * Get the current model
    */
  def getCurrentModel: Model

  /**
    * Apply the function func, which uses Var declared in this ModelDeclaration,
    * on the model (inheriting for this object too), temporarily changing the current model.
    *
    * @param model : model on which to apply the function
    * @param func  : function to apply
    */
  def apply[RetVal](model: Model)(func: => RetVal): RetVal

  /**
    * Post a new constraint
    *
    * @param constraint the constraint to post
    */
  def post(constraint: Constraint): Unit

  /**
    * Add a new constraint to the model
    *
    * @param constraint the constraint to add
    */
  def add(constraint: Constraint): Unit

  /**
    * Minimize on variable v
    *
    * @param v variable to minimize
    */
  def minimize(v: Expression)

  /**
    * Maximize on variable v
    *
    * @param v variable to maximize
    */
  def maximize(v: Expression)

  /**
    * Remove the optimisation method
    */
  def removeOptimization()

  /**
    * Apply a model operator
    *
    * @param operator operator to apply
    */
  def apply[OutputType <: Model](operator: ModelOperator[OutputType]): Unit
}
