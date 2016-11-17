package oscar.modeling.models

import oscar.modeling.algebra.floating.FloatExpression
import oscar.modeling.algebra.integer.IntExpression

trait OptimisationMethod

case class Maximisation(objective: IntExpression) extends OptimisationMethod
case class Minimisation(objective: IntExpression) extends OptimisationMethod
case class MaximisationFloat(objective: FloatExpression) extends OptimisationMethod
case class MinimisationFloat(objective: FloatExpression) extends OptimisationMethod
case class NoOptimisation() extends  OptimisationMethod