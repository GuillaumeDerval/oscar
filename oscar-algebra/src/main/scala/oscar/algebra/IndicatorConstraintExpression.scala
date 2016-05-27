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
package oscar.algebra

/**
 * An indicator constraint is a linear constraint that is active or relaxed
 * based on the value of a combination of binary variables (the indicators).
 *
 * For this, the given linear constraint is transformed into a set of linear constraints formulated using the
 * so-called big M method.
 *
 * Example: f(x) <= c is active when binary variable x is 1 yields the indicator constraint
 *    f(x) <= c + M * (1 - x)   with M (big M) a big constant value.
 *  In this case, the indicator is (1- x).
 *
 * The usual boolean relations between binary variables (NOT, AND, OR) are accepted as indicators.
 *
 * Example: f(x) <= c is active when binary variable x is NOT 1 yields the indicator constraint
 *    f(x) <= c + M * x
 *  In this case, the indicator is x.
 *
 * Example: f(x) <= c is active when binary variable x is 1 AND binary variable y is 1 yields the indicator constraint
 *    f(x) <= c + M * (2 - x - y)
 *  In this case, the indicator is (2 - x - y).
 *
 * Example: f(x) <= c is active when binary variable x is 1 OR binary variable y is 1 yields the indicator constraint
 *    f(x) <= c + M * (1 - x)
 *    f(x) <= c + M * (1 - y)
 *  In this case, the indicator constraint is actually represented as a set of constraints.
 *  In this case, the indicators are (1 - x) and (1 - y).
 *
 * Of course any combination of these relations are accepted.
 *
 * @author acrucifix acr@n-side.com
 */
class IndicatorConstraintExpression(
  linExpr: LinearExpression,
  sense: ConstraintSense,
  val indicators: Seq[LinearExpression],
  val bigM: Double) extends LinearConstraintExpression(linExpr, sense) {

  require(indicators.length > 0, s"An IndicatorConstraint should declare at least one indicator.")

  val constraintExpressions: Seq[LinearConstraintExpression] =
    indicators.map { ind =>
      val actualBound = bigM * ind

      lazy val constraintLQ = linExpr <:= actualBound
      lazy val constraintGQ = linExpr >:= -actualBound

      sense match {
        case LQ => Seq(constraintLQ)
        case GQ => Seq(constraintGQ)
        case EQ => Seq(constraintLQ, constraintGQ)
      }
    }.reduce(_ ++ _)

  override def toString: String = constraintExpressions.mkString("\n")
}
