package oscar.modeling.models

import oscar.modeling.algebra.Expression
import oscar.modeling.constraints.Constraint
import oscar.modeling.models.operators.ModelOperator

/**
  * Proxy to a ModelDeclaration
  */
trait ModelDeclarationProxy {
  val md: ModelDeclarationInterface
  def getCurrentModel = md.getCurrentModel
  def apply[RetVal](model: Model)(func: => RetVal): RetVal = md.apply[RetVal](model)(func)
  def post(constraint: Constraint): Unit = md.post(constraint)
  def post(constraints: Seq[Constraint]): Unit = constraints.foreach(md.post)
  def add(constraint: Constraint): Unit = md.add(constraint)
  def add(constraints: Seq[Constraint]): Unit = constraints.foreach(md.add)
  def minimize(v: Expression) = md.minimize(v)
  def maximize(v: Expression) = md.maximize(v)
  def removeOptimization() = md.removeOptimization()
  def apply[OutputType <: Model](operator: ModelOperator[OutputType]): Unit = md.apply[OutputType](operator)
  def fork[T](func: => T): T =  md.fork(func)
}