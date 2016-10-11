import java.io.ByteArrayInputStream

import org.xcsp.checker.SolutionChecker
import org.xcsp.parser.XConstraints.XCtr
import org.xcsp.parser.XObjectives.XObj
import scala.collection.JavaConversions._

/**
  * A "library-friendly" version of SolutionChecker
  * @param filename the filename of the XCSP3 instance on which the solution will be checked
  * @param solution the solution, as an XCSP3-valid XML string
  */
class CheckerLib(filename: String, solution: String)
  extends SolutionChecker(filename, new ByteArrayInputStream(solution.getBytes)) {
  def getViolatedCtrs: List[XCtr] = violatedCtrs.toList
  def getInvalidObjs: List[XObj] = invalidObjs.toList
}
