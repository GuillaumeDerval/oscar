package oscar.modeling.constraints

import oscar.modeling.algebra.IntExpression
import oscar.modeling.models.ModelDeclaration
import oscar.modeling.vars.IntVar

/**
  * N variables of X take a values in set S
  */
case class Among(N: IntExpression, X: Array[IntExpression], S: Set[Int]) extends Constraint {}

object Among
{
  /**
    * N variables of X take value v
    */
  def apply(N: IntExpression, X: Array[IntExpression], v: Int): Among = Among(N, X, Set(v))
}

object AtLeast
{
  /**
    * AtLeast Constraint: at least n variables take their value in s
    *
    * @param n counter variable
    * @param x array of variables
    * @param s set of values
    * @return a constraint enforcing that  #{ i | x(i) in s } >= n
    */
  def apply(n: Int, x: Array[IntExpression], s: Set[Int])(implicit modelDeclaration: ModelDeclaration) = {
    Among(IntVar(n, x.size), x, s)
  }

  /**
    * AtLeast Constraint: at least n variables equal to v
    *
    * @param n counter variable
    * @param x array of variables
    * @param v a value
    * @return a constraint enforcing that  #{ i | x(i) = v } >= n
    */
  def apply(n: Int, x: Array[IntExpression], v: Int)(implicit modelDeclaration: ModelDeclaration) = {
    Among(IntVar(n, x.size), x, Set(v))
  }
}

object AtMost
{
  /**
    * AtMost Constraint: at most n variables take their value in s
    *
    * @param n counter variable
    * @param x array of variables
    * @param s set of values
    * @return a constraint enforcing that  #{ i | x(i) in s } <= n
    */
  def apply(n: Int, x: Array[IntExpression], s: Set[Int])(implicit modelDeclaration: ModelDeclaration) = {
    Among(IntVar(0, n), x, s)
  }

  /**
    * AtMost Constraint: at least n variables equal to v
    *
    * @param n counter variable
    * @param x array of variables
    * @param v a value
    * @return a constraint enforcing that  #{ i | x(i) = v } <= n
    */
  def apply(n: Int, x: Array[IntExpression], v: Int)(implicit modelDeclaration: ModelDeclaration)= {
    Among(IntVar(0, n), x, Set(v))
  }
}