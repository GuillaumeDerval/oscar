package oscar.algebra

/**
 * Type representing polynomial degree of [[Expression]]s.
 */
class ExpressionDegree

/**
 * Quadratic type for [[Expression]]s
 */
class Quadratic extends ExpressionDegree

/**
 * Linear type for [[Expression]]s
 */
class Linear extends Quadratic

/**
 * Constant type for [[Expression]]s
 */
class Constant extends Linear


/**
 * Represents the product of several [[Product]]s. Used only for DSL for multiplication and compile-time check of degree
 * of [[Expression]]s.
 *
 * @param products products to be multiplied
 * @param name name of the product
 * @tparam T the degree of the [[Expression]]
 * @tparam V the type of values stored by the variables of the [[Expression]]. For now, mainly Double is used.
 */
protected class ProdExpression[+T <: ExpressionDegree, +V: Numeric](products: Iterable[Product[T, V]], name: String = "ND") extends NormalizedExpression[T, V](products, name)

protected object ProdExpression {
  def apply[T <: ExpressionDegree, V: Numeric](a: NormalizedExpression[T, V], b: NormalizedExpression[T, V]): ProdExpression[T, V] = {
    val num = implicitly[Numeric[V]]
    new ProdExpression[T, V](
      for (va <- a.terms; vb <- b.terms) yield Product(num.times(va.coef.d, vb.coef.d), va.vars ++ vb.vars)
    )
  }
}

/**
 * Represents the division of several [[Product]]s. Used only for DSL for division and compile-time check of degree
 * of [[Expression]]s.
 *
 * @param products products to be divided
 * @param name name of the division
 * @tparam T the degree of the [[Expression]]
 * @tparam V the type of values stored by the variables of the [[Expression]]. For now, mainly Double is used.
 */
protected class DivExpression[+T <: ExpressionDegree, +V: Fractional](products: Iterable[Product[T, V]], name: String = "ND") extends NormalizedExpression[T, V](products, name)

protected object DivExpression {
  def apply[V: Fractional](a: NormalizedExpression[Constant, V], b: NormalizedExpression[Constant, V]): DivExpression[Constant, V] = {
    val num = implicitly[Fractional[V]]
    new DivExpression[Constant, V](
      for (va <- a.terms; vb <- b.terms) yield Product(Const(num.div(va.coef.d, vb.coef.d)))
    )
  }
}


object ExpressionDegree {

  implicit def times[V](implicit numeric: Numeric[V]): (NormalizedExpression[ExpressionDegree, V], NormalizedExpression[ExpressionDegree, V]) => ProdExpression[ExpressionDegree, V] = {
    ProdExpression(_, _)
  }
}

object Quadratic {

  implicit def times02[V](implicit numeric: Numeric[V]): (NormalizedExpression[Constant, V], NormalizedExpression[Quadratic, V]) => ProdExpression[Quadratic, V] = {
    ProdExpression(_, _)
  }

  implicit def times11[V](implicit numeric: Numeric[V]): (NormalizedExpression[Linear, V], NormalizedExpression[Linear, V]) => ProdExpression[Quadratic, V] = {
    ProdExpression(_, _)
  }

  implicit def times20[V](implicit numeric: Numeric[V]): (NormalizedExpression[Quadratic, V], NormalizedExpression[Constant, V]) => ProdExpression[Quadratic, V] = {
    ProdExpression(_, _)
  }

}

object Linear {

  implicit def times01[V](implicit numeric: Numeric[V]): (NormalizedExpression[Constant, V], NormalizedExpression[Linear, V]) => ProdExpression[Linear, V] = {
    ProdExpression(_, _)
  }


  implicit def times10[V](implicit numeric: Numeric[V]): (NormalizedExpression[Linear, V], NormalizedExpression[Constant, V]) => ProdExpression[Linear, V] = {
    ProdExpression(_, _)
  }
}

object Constant {

  implicit def times00[V](implicit numeric: Numeric[V]): (NormalizedExpression[Constant, V], NormalizedExpression[Constant, V]) => ProdExpression[Constant, V] = {
    ProdExpression[Constant, V](_, _)
  }

  implicit def div00[V](implicit numeric: Fractional[V]): (NormalizedExpression[Constant, V], NormalizedExpression[Constant, V]) => DivExpression[Constant, V] = {
    DivExpression(_, _)
  }
}