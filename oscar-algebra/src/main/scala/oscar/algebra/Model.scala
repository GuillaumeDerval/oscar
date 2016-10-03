package oscar.algebra

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

class Model[O <: ExpressionDegree, C <: ExpressionDegree,V]{

  /**
    * Set of variables of the [[Model]]
    */
  private val _variables: scala.collection.mutable.Map[Var[V],Int] = new scala.collection.mutable.HashMap[Var[V],Int]()
  def variables: scala.collection.Map[Var[V],Int] = _variables

  /**
    * @return the maximum index of all the variables
    */
  def maxIndex = _variables.size - 1

  /**
    * List of objectives to be optimized
    */
  val objectives = scala.collection.mutable.ListBuffer[NormalizedExpression[O,V]]()

  /**
    * List of constraints for this [[Model]]
    */
  val constraints = scala.collection.mutable.ListBuffer[EquationSystem[C,V]]()

  /**
    * Add a variable in this model
    * @param v the variable to be added
    * @return the index of the added variable in this model
    */
  def addVariable(v: Var[V]) = {
    _variables += v -> maxIndex
    maxIndex - 1
  }

  /**
    * Add a constraint to the model
    * @param eq the equation to be added
    */
  def subjectTo(eq: Equation[C,V]): Unit ={
    constraints += new EquationSystem[C,V](Iterable(eq))
  }

  /**
    * Add a set of constraint to the model
    * @param eqs the set of constraints to be added
    */
  def subjectTo(eqs: EquationSystem[C,V]): Unit ={
    constraints += eqs
  }

  /**
    * Add an objective to be minimized
    */
  def minimize(eq: NormalizedExpression[O,V]): Unit ={
    objectives += eq
  }

  /**
    * Add an objective to be minimized
    */
  def withObjective(obj: NormalizedExpression[O,V]): Unit ={
    objectives += obj
  }

  override def toString = constraints.mkString("\n")

  /**
    * Buffer the solution found by a solver for this [[Model]]
    */
  @deprecated
  class SolutionBuffer {

    var objectiveValue: Double = 0.0

    val values = new Array[Double](maxIndex)

    def immutable = new Solution(values.toIndexedSeq, objectiveValue)

    def update[T](v: Var[V], value: Double): Unit ={
      values(v.id) = value
    }

  }

  /**
    * Solution for this model, respecting all the constraints
    * @param values the values of all the variables, indexed by their respective index
    * @param objectiveValue value of the objective for this solution
    */
  class Solution(values: IndexedSeq[Double], objectiveValue: Double) {

    def apply(v: Var[V]) = values(v.id)
    def cost = objectiveValue
  }
}

