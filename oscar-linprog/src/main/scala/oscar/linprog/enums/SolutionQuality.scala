package oscar.linprog.enums

sealed abstract class SolutionQuality(val name: String) {
  override def toString: String = name
}

case object Optimal extends SolutionQuality("OPTIMAL")
case object Suboptimal extends SolutionQuality("SUBOPTIMAL")
