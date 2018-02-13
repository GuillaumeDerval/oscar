/*******************************************************************************
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License  for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
 ******************************************************************************/

package oscar.modeling.models.operators

import oscar.modeling.algebra._
import oscar.modeling.algebra.bool.{BoolExpression, Eq}
import oscar.modeling.algebra.integer._
import oscar.modeling.constraints.ExpressionConstraint
import oscar.modeling.models.UninstantiatedModel

import scala.collection.mutable

/**
  * Simplify sums, using weightedsums notably.
  */
object SimplifySum extends ModelOperator[UninstantiatedModel] {
  def apply(model: UninstantiatedModel): UninstantiatedModel = {
    val newConstraints = model.constraints.map {
      case ExpressionConstraint(expr) => new ExpressionConstraint(SimplifySum(expr))
      case constraint@default => constraint
    }
    model.copy(constraints = newConstraints)
  }

  def apply(expr: BoolExpression): BoolExpression = {
    SimplifySum(expr.asInstanceOf[IntExpression]).asInstanceOf[BoolExpression]
  }

  def apply(expr: IntExpression): IntExpression = {
    val s1 = convertToWeightedSum(expr)
    val s2 = updateWeightedSumCoefficient(s1)
    val s3 = mergeWeightedSums(s2)
    val s4 = simplifyEq(s3)
    transformBackWeightedSum(s4)
  }

  private def simplifyEq(expr: Expression): IntExpression = {
    val nexpr = expr.mapSubexpressions(convertToWeightedSum)
    nexpr match {
      case Eq(Array(WeightedSum(x, wx), WeightedSum(y, wy))) => Eq(mergeWeightedSums(WeightedSum(x++y, wx ++ wy.map(i => -i))), 0)
      case default: IntExpression => default
      case _ => throw new RuntimeException("Only Bool/IntExpressions are supported in CP")
    }
  }

  private def convertToWeightedSum(expr: Expression): IntExpression = {
    val nexpr = expr.mapSubexpressions(convertToWeightedSum)
    nexpr match {
      case Sum(a) => WeightedSum(a, Array.tabulate(a.length)(_ => 1))
      case default: IntExpression => default
      case _ => throw new RuntimeException("Only Bool/IntExpressions are supported in CP")
    }
  }

  private def updateWeightedSumCoefficient(expr: Expression): IntExpression = {
    val nexpr = expr.mapSubexpressions(updateWeightedSumCoefficient)
    nexpr match {
      case WeightedSum(x, w) =>
        val nxw : Array[(IntExpression, Int)] = x.zip(w).map(updateWeightedSumCoefficientRecur)
        val (nx, nw) = nxw.unzip
        WeightedSum(nx, nw)
      case default: IntExpression => default
      case _ => throw new RuntimeException("Only Bool/IntExpressions are supported in CP")
    }
  }

  private def updateWeightedSumCoefficientRecur(ixw: (IntExpression, Int)): (IntExpression, Int) = {
    val ix = ixw._1
    val iw = ixw._2
    ix match {
      case Prod(Array(sub, Constant(subw))) => updateWeightedSumCoefficientRecur((sub, subw*iw))
      case Prod(Array(Constant(subw), sub)) => updateWeightedSumCoefficientRecur((sub, subw*iw))
      case default => ixw
    }
  }

  private def mergeWeightedSums(expr: Expression): IntExpression = {
    val nexpr = expr.mapSubexpressions(mergeWeightedSums)
    nexpr match {
      case WeightedSum(x, w) => {
        val (nx, nw) = x.zip(w).flatMap(updateWeightedSum).unzip
        val (fnx, fnw) = findRedundancy(nx, nw)
        WeightedSum(fnx, fnw)
      }
      case default: IntExpression => default
      case _ => throw new RuntimeException("Only Bool/IntExpressions are supported in CP")
    }
  }

  private def findRedundancy(exprs: Array[IntExpression], weights: Array[Int]): (Array[IntExpression], Array[Int]) = {
    val m = mutable.HashMap[IntExpression, Int]()
    for((expr, w) <- exprs.zip(weights)) {
      m.update(expr, m.getOrElse(expr, 0)+w)
    }
    m.toArray.unzip
  }

  private def updateWeightedSum(ixw: (IntExpression, Int)): Array[(IntExpression, Int)] = {
    ixw._1 match {
      case WeightedSum(ix2, iw2) => ix2.zip(iw2.map(_*ixw._2))
      case default => Array(ixw)
    }
  }

  private def transformBackWeightedSum(expr: Expression): IntExpression = {
    val nexpr = expr.mapSubexpressions(transformBackWeightedSum)
    nexpr match {
      case WeightedSum(x, w) => {
        if(w.forall(v => v == 1)) {
          new Sum(x)
        }
        else
          nexpr.asInstanceOf[WeightedSum]
      }
      case default: IntExpression => default
      case _ => throw new RuntimeException("Only Bool/IntExpressions are supported in CP")
    }
  }
}
