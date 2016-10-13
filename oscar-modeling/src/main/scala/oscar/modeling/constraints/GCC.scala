package oscar.modeling.constraints

import oscar.modeling.algebra.IntExpression
import oscar.modeling.models.ModelDeclaration
import oscar.modeling.vars.IntVar

case class GCC(x: Array[IntExpression], minval: Int, low: Array[Int], up: Array[Int]) extends Constraint {}

/**
  * Global Cardinality Constraint with variable counters
  * @param x a non empty collection of variables
  * @param valueOccurrence is a collection of pairs (v, o)
  *        where o is variable representing the number of occurrences of value v
  * @return a constraint such that for each (o,v) in valueOccurrence, o is the number of times the value v appears in x
  */
case class GCCVar(x: Array[IntExpression], valueOccurrence: Iterable[(Int, IntExpression)]) extends Constraint {}

object GCC {
  /**
    * Global Cardinality Constraint: every value occurs at least min and at most max
    * @param x an non empty array of variables
    * @param values is the range of constrained values
    * @param min is the minimum number of occurrences for each value in the range values
    * @param max is the maximum number of occurrences for each value in the range values
    * @return a constraint such that each value in the range values occurs at least min and at most max times.
    */
  def apply(x: Array[IntExpression], values: Range, min: Int, max: Int): Constraint = {
    new GCC(x, values.min, Array.fill(values.size)(min), Array.fill(values.size)(max))
  }

  /**
    * Global Cardinality Constraint: every value v occurs at least min(v) and at most max(v)
    * @param x an non empty array of variables
    * @param values is the range of constrained values
    * @param min is the minimum number of occurrences for each value in the range values
    * @param max is the maximum number of occurrences for each value in the range values
    * @return a constraint such that each value in the range values occurs at least min and at most max times.
    */
  def apply(x: Array[IntExpression], values: Range, min: Array[Int], max: Array[Int]): Constraint = {
    new GCC(x, values.min, min, max)
  }

  /**
    * Global Cardinality Constraint: every value v occurs at least min(v) and at most max(v)
    * @param x an non empty array of variables
    * @param values is the array of constrained values
    * @param min is the minimum number of occurrences for each value in the array values
    * @param max is the maximum number of occurrences for each value in the array values
    * @return a constraint such that each value in the array values occurs at least min and at most max times.
    */
  def apply(x: Array[IntExpression], values: Array[Int], min: Array[Int], max: Array[Int])(implicit modelDeclaration: ModelDeclaration): Constraint = {
    //TODO get the version of Pierre which should be faster
    //check if values is a range
    val sorted = values.sorted
    if(sorted.zipWithIndex.forall(v => v._1 == sorted(0) + v._2)) {
      // use simple GCC
      new GCC(x, sorted(0), min, max)
    }
    else {
      // use more complex GCC
      new GCCVar(x, values.zip(min.zip(max).map(i => IntVar(i._1, i._2))))
    }
  }

  /**
    * Global Cardinality Constraint: every value v occurs at least min(v) and at most max(v)
    * @param x an non empty array of variables
    * @param values is the array of constrained values
    * @param occurs occurs(i) contains the number of occurences of values(i)
    * @return a constraint such that each value in the array values occurs at least min and at most max times.
    */
  def apply(x: Array[IntExpression], values: Array[Int], occurs: Array[IntExpression]): Constraint = {
    new GCCVar(x, values.zip(occurs))
  }
}