package oscar.linprog.enums

sealed abstract class SolutionQuality(val name: String) {
  override def toString: String = name
}

case object Optimal extends SolutionQuality("OPTIMAL")
case object Suboptimal extends SolutionQuality("SUBOPTIMAL")

object SolutionQuality {
  val values = Seq(Optimal, Suboptimal)

  def fromString(str: String): SolutionQuality = values.find(q => q.name == str.toUpperCase) match {
    case Some(q) => q
    case None    => throw new IllegalArgumentException(s"Unrecognized solution quality: $str")
  }
}