package oscar.modeling.models

import oscar.modeling.vars.IntVar

trait OptimisationMethod

case class Maximisation(objective: IntVar) extends OptimisationMethod
case class Minimisation(objective: IntVar) extends OptimisationMethod
case class NoOptimisation() extends  OptimisationMethod