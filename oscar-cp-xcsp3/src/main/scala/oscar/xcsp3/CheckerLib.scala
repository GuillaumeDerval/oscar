package oscar.xcsp3

import java.io.ByteArrayInputStream

import org.xcsp.checker.SolutionChecker
import org.xcsp.parser.entries.XConstraints.XCtr
import org.xcsp.parser.entries.XObjectives.XObj

import scala.collection.JavaConversions._

/**
  * A "library-friendly" version of SolutionChecker
  * @param filename the filename of the XCSP3 instance on which the solution will be checked
  * @param solution the solution, as an XCSP3-valid XML string
  */
class CheckerLib(filename: String, solution: String)
  extends SolutionChecker(false, filename, new ByteArrayInputStream(solution.getBytes)) {
  def getViolatedCtrs: List[String] = violatedCtrs.toList
  def getInvalidObjs: List[String] = invalidObjs.toList
  def valid: Boolean = violatedCtrs.isEmpty && invalidObjs.isEmpty
}
