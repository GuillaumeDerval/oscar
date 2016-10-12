package oscar.algebra

/**
  * Represents an objective for a mathematical model.
  * @tparam O the degree of the objective
  * @tparam V the type of values contained by the variables in the objective
  */
abstract class Objective[O <: ExpressionDegree, V: Numeric] {
  /**
    * Expression to be either minimized or maximized
    * @return
    */
  def expression: NormalizedExpression[O, V]
}

object Maximize{
  def apply[O <: ExpressionDegree, V: Numeric](expression: NormalizedExpression[O, V]) = new Maximize[O,V](expression)
}

object Minimize{
  def apply[O <: ExpressionDegree, V: Numeric](expression: NormalizedExpression[O, V]) = new Minimize[O,V](expression)
}

/**
  * Describe a maximization objective.
  * @param expression expression to be maximized
  * @tparam O the degree of the objective
  * @tparam V the type of values contained by the variables in the objective
  */
class Maximize[O <: ExpressionDegree, V: Numeric](val expression: NormalizedExpression[O, V]) extends Objective[O, V]

/**
  * Describe a minimization objective.
  * @param expression expression to be minimized
  * @tparam O the degree of the objective
  * @tparam V the type of values contained by the variables in the objective
  */
class Minimize[O <: ExpressionDegree, V: Numeric](val expression: NormalizedExpression[O, V]) extends Objective[O, V]

/**
  * A [[Model]] represents a mathematical model (X,C,O) where X is a set of numerical variables, C a set of constraints
  * and O a set of objectives to be optimized.
  *
  * Please note that such [[Model]] is mutable and there is no check to ensure that it is not modified after a solver
  * has begun solving it.
  *
  * @tparam O the maximum degree of the objectives to be added to this model
  * @tparam C the maximum degree of the constraints to be added to this model
  * @tparam V the type value stored in the variables (Double, Int, ...). For now, mainly Double is used here.
  */
class Model[O >: Constant <: ExpressionDegree , C <: ExpressionDegree, V: Numeric] {

  /**
    * Set of variables of the [[Model]]
    */
  private val _variables: scala.collection.mutable.ArrayBuffer[Var[V]] = new scala.collection.mutable.ArrayBuffer[Var[V]]()

  def variables: scala.collection.IndexedSeq[Var[V]] = _variables

  /**
    * @return the maximum index of all the variables
    */
  def maxIndex = _variables.size

  /**
    * Objective to be optimized
    */
  private var _objective: Objective[O, V] = new Minimize[O,V](implicitly[Numeric[V]].zero.normalized[V])
  def objective = _objective

  /**
    * List of constraints for this [[Model]]
    */
  private val _constraints = scala.collection.mutable.ListBuffer[EquationSystem[C, V]]()
  def constraints: Seq[EquationSystem[C,V]] = _constraints

  /**
    * Add a variable in this model
    *
    * @param v the variable to be added
    * @return the index of the added variable in this model
    */
  def addVariable(v: Var[V]) = {
    _variables += v
    maxIndex - 1
  }

  /**
    * Add a constraint to the model
    *
    * @param eq the equation to be added
    */
  def subjectTo(eq: Equation[C, V]*): Unit = {
    _constraints += new EquationSystem[C, V](eq.toIterable)
  }

  /**
    * Add a set of constraint to the model
    *
    * @param eqs the set of constraints to be added
    */
  def subjectTo(eqs: EquationSystem[C, V]): Unit = {
    _constraints += eqs
  }

  /**
    * Add an objective to be minimized
    */
  def minimize(eq: NormalizedExpression[O, V]): Unit = {
    _objective = new Minimize[O, V](eq)
  }


  /**
    * Add an objective to be minimized
    */
  def maximize(eq: NormalizedExpression[O, V]): Unit = {
    _objective = new Maximize[O, V](eq)
  }

  /**
    * Add an objective to be minimized
    */
  def withObjective(obj: Objective[O, V]): Unit = {
    _objective = obj
  }

  override def toString = _constraints.mkString("\n")



}

