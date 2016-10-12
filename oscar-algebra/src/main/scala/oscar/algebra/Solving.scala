package oscar.algebra

import java.nio.file.Path


/**
  * Factory class for creating [[SolverRun]]s that solves [[Model]]s with a specific solver.
  *
  * @tparam O the degree of objective of the [[Model]] to solve
  * @tparam C the degree of the constraints of the [[Model]]
  * @tparam V the type of values contained by the variables
  */
abstract class SolverInterface[O  >: Constant <: ExpressionDegree, C <: ExpressionDegree, V: Numeric] {

  /**
    * Create a new [[SolverRun]] to solve `model`. Please note that the solver does not actually solve the [[Model]]. The method `solve` of the returned [[SolverRun]] should be called to actually solve the [[Model]].
    * @param model the [[Model]] to be solved
    * @return A [[SolverRun]] that will solve `model`
    */
  def run(model: Model[O, C, V], config: Option[Path] = None): SolverRun[O, C, V]

  /**
    * Solve the specified [[Model]]
    * @param model the model to be solved
    * @return the status of the solving
    */
  def solve(model: Model[O, C, V], config: Option[Path] = None) = run(model).solve

}

/**
  * Represents the solving of a mathematical model by a specific solver. A [[SolverRun]] can be modified to change the type of variables and the objective.
  * @tparam O the degree of objective of the [[Model]] to solve
  * @tparam C the degree of the constraints of the [[Model]]
  * @tparam V the type of values contained by the variables
  */
abstract class SolverRun[O  >: Constant <: ExpressionDegree, C <: ExpressionDegree, V: Numeric] {

  /**
    * Solves the [[Model]] using a specific solver
    * @return the status of the solving process
    */
  def solve: ModelStatus[O, C, V]

  /**
    * Cleanly terminate the optimization process
    */
  def abort: Unit

  /**
    * Update the lower bound a variable. The [[Model]] passed to initialize the [[SolverRun]] will ```not``` be modified.
    * @param v the variable whose lower bound is updated
    * @param d the new lower bound
    */
  def setLowerBound(v: Var[V], d: V): Unit


  /**
    * Update the upper bound a variable. The [[Model]] passed to initialize the [[SolverRun]] will ```not``` be modified.
    * @param v the variable whose upper bound is updated
    * @param d the new upper bound
    */
  def setUpperBound(v: Var[V], d: V): Unit

  /**
    * Update the objective of the [[Model]]. The [[Model]] passed to initialize the [[SolverRun]] will ```not``` be modified.
    * @param obj the [[Objective]] to use for the next optimization.
    */
  def setObjective(obj: Objective[O,V]): Unit

  /**
    * Relax a variable to be continuous. The [[Model]] passed to initialize the [[SolverRun]] will ```not``` be modified.
    * @param v the variable to make continuous
    */
  def setContinuous(v: Var[Double])

  /**
    * Constrain a variable to be integral. The [[Model]] passed to initialize the [[SolverRun]] will ```not``` be modified.
    * @param v the variable to make integral
    */
  def setInteger(v: Var[Double])

  /**
    * DSL to write `run.lowerBound(v) = newValue`
    */
  object lowerBound{
    def update(v: Var[V], d: V) = setLowerBound(v,d)
  }

  /**
    * DSL to write `run.upperBound(v) = newValue`
    */
  object upperBound{
    def update(v: Var[V], d: V) = setUpperBound(v,d)
  }
}

/**
  * Solution for this model, respecting all the constraints
  *
  * @param values the values of all the variables, indexed by their respective index
  */
class Solution[V: Numeric](values: IndexedSeq[V]) extends Function[Var[V], V] {

  /**
    * @param v the variable whose value in this [[Solution]] should be returned
    * @return the value of `v` in this [[Solution]]
    */
  def apply(v: Var[V]) = values(v.id)

  /**
    *
    * @return the value of `expr` using the values of the variables represented by `this`
    */
  def apply(expr: NormalizedExpression[_,V]) = expr.eval(this)
}

/**
  * Status of the solving of a [[Model]] by a specific solver
  * @tparam O the degree of the objective of the [[Model]] solved
  * @tparam C the degree of the constraints of the [[Model]]
  * @tparam V type of values contained by the variables of the [[Model]] solved
  */
abstract class ModelStatus[O <: ExpressionDegree, C <: ExpressionDegree, V: Numeric]

/**
  * The solving has been successful, but optimality is not proven
  * @param solution the [[Solution]] found by the solver
  * @tparam O the degree of the objective of the [[Model]] solved
  * @tparam C the degree of the constraints of the [[Model]]
  * @tparam V type of values contained by the variables of the [[Model]] solved
  */
case class AFeasible[O <: ExpressionDegree, C <: ExpressionDegree, V: Numeric](solution: Solution[V]) extends ModelStatus[O, C, V]

/**
  * The solving has been successful and optimality is proven
  * @param solution the optimal solution found
  * @tparam O the degree of the objective of the [[Model]] solved
  * @tparam C the degree of the constraints of the [[Model]]
  * @tparam V type of values contained by the variables of the [[Model]] solved
  */
case class AOptimal[O <: ExpressionDegree, C <: ExpressionDegree, V: Numeric](solution: Solution[V]) extends ModelStatus[O, C, V]

/**
  * The [[Model]] is unbounded.
  * @tparam O the degree of the objective of the [[Model]] solved
  * @tparam C the degree of the constraints of the [[Model]]
  * @tparam V type of values contained by the variables of the [[Model]] solved
  */
case class AUnbounded[O <: ExpressionDegree, C <: ExpressionDegree, V: Numeric]() extends ModelStatus[O, C, V]

/**
  * The [[Model]] is infeasible given the bounds on the variables and the constraints
  * @tparam O the degree of the objective of the [[Model]] solved
  * @tparam C the degree of the constraints of the [[Model]]
  * @tparam V type of values contained by the variables of the [[Model]] solved
  */
case class AInfeasible[O <: ExpressionDegree, C <: ExpressionDegree, V: Numeric]() extends ModelStatus[O, C, V]

/**
  * The solver has returned an unknown status
  * @tparam O the degree of the objective of the [[Model]] solved
  * @tparam C the degree of the constraints of the [[Model]]
  * @tparam V type of values contained by the variables of the [[Model]] solved
  */
case class AWarning[O <: ExpressionDegree, C <: ExpressionDegree, V: Numeric]() extends ModelStatus[O, C, V]

case class ANoSolutionFoundException[O <: ExpressionDegree, C <: ExpressionDegree, V: Numeric](ModelStatus: ModelStatus[O,C,V]) extends Exception(s"No solution found to the problem, end status is $ModelStatus")


