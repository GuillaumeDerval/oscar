package oscar.linprog.test

import org.scalatest.matchers.{MatchResult, Matcher}

/**
 * Custom matchers for tests.
 */
trait OscarLinprogMatchers {

  case class DoubleOptionMatcher(right: Option[Double], tol: Double) extends Matcher[Option[Double]] {
    def apply(left: Option[Double]): MatchResult = {
      val res = (left, right) match {
        case (Some(dLeft), Some(dRight)) => dLeft >= dRight - tol && dLeft <= dRight + tol
        case (None, None)                => true
        case _                           => false
      }

      MatchResult(
        res,
        s"$left did not equal $right",
        s"$left did equal $right but it shouldn't")
    }
  }
  def equalWithTolerance(right: Option[Double], tol: Double = 1e-6) = DoubleOptionMatcher(right, tol)

}
