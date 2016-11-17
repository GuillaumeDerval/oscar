package oscar.modeling.solvers.cp.decompositions

import oscar.algo.search.Branching
import oscar.modeling.models.UninstantiatedModel
import oscar.modeling.models.cp.MemoCPModel
import oscar.modeling.solvers.cp.Branchings._
import oscar.modeling.solvers.cp.distributed.{SubProblem, SubProblemDiscrepancy}

import scala.collection.mutable


/**
  * An iterative deepening decomposition strategy that can be expanded to do a lot of different strategies
  * @param searchInstantiator the search to use
  * @tparam Threshold an object that will be given to the function nextThreshold and shouldStopSearch
  */
abstract class IterativeDeepeningStrategy[Threshold](searchInstantiator: BranchingInstantiator)
  extends DecompositionStrategy {

  def initThreshold(model: MemoCPModel, wantedSubProblems: Int): Threshold
  def nextThreshold(oldThreshold: Threshold, currentSubproblems: List[SubProblem], wantedSubProblems: Int): Threshold
  def shouldStopSearch(model: MemoCPModel, threshold: Threshold, depth: Int, discrepancy: Int): Boolean

  // The idea here is to store the "path" taken in the tree at each iteration of the search
  var currentDepth = -1
  var currentDiscrepancy = -1
  var currentPath: Array[Int] = null

  /**
    * A custom search that stops going further in the tree over a given Threshold
    * @param a the model
    * @param search the search
    * @param threshold the threshold
    * @return Oscar-CP alternatives, that are originally from ``search``, if below the Threshold, else, oscar.cp.noAlternative.
    */
  private def customSearch(a: MemoCPModel, search: Branching, threshold: Threshold): Seq[oscar.cp.Alternative] = {
    val base : Seq[Alternative] = search.alternatives().toList
    val trueDepth = currentDepth+1
    val trueDiscrepancy = currentDiscrepancy
    if(shouldStopSearch(a, threshold, trueDepth, trueDiscrepancy)) {
      oscar.cp.noAlternative
    }
    else {
      if(trueDepth >= currentPath.length)
        currentPath = Array.tabulate(currentPath.length*2)(i => if(i < currentPath.length) currentPath(i) else -1)

      base.zipWithIndex.map((tuple) => {
        () => {
          currentDepth = trueDepth
          currentDiscrepancy = trueDiscrepancy + tuple._2
          currentPath(currentDepth) = tuple._2
          tuple._1()
        }
      })
    }
  }

  private def tryDecomposition(model: MemoCPModel, threshold: Threshold): List[SubProblem] = {
    // TODO: this can be improved a lot once MemoCPModel has been rewritten to integrate it in the oscar search
    currentDepth = -1
    currentPath = Array.tabulate(32)(_ => -1) //32 should be enough for decomposition
    currentDiscrepancy = 0

    // To store the path
    val path_list = new mutable.MutableList[(Array[Int], Int)]

    // Search all the possibles paths
    model.apply {
      val baseSearch = searchInstantiator(model)
      model.pushState()

      model.cpSolver.search(customSearch(model, baseSearch, threshold))
      model.cpSolver.onSolution {
        path_list += ((currentPath.clone().slice(0, currentDepth+1), currentDiscrepancy))
      }
      model.cpSolver.start()

      model.popState()

      // For each path, generate the list of constraints added
      path_list.toList.map(path_with_data => {
        val newSearch = searchInstantiator(model)
        //re-apply the path
        model.pushState()
        var currentAlternatives = newSearch.alternatives().toList
        for(i <- path_with_data._1) {
          currentAlternatives(i)()
          currentAlternatives = newSearch.alternatives().toList
        }
        //get the constraints
        val constraints = model.getAddedConstraints
        model.popState()
        //generate subproblem object
        new SubProblem(constraints).addData(SubProblemDiscrepancy, path_with_data._2)
      })
    }
  }

  /**
    * Decompose the problem
    *
    * @param baseModel the model to decompose
    * @param count the (minimum) number of subproblems wanted
    * @return A list of subproblems
    */
  override def decompose(baseModel: UninstantiatedModel, count: Int): List[SubProblem] = {
    // Instantiate model and search
    val model = new MemoCPModel(baseModel.removeOptimisation())

    // Threshold to stop going further in the research
    var threshold = initThreshold(model, count)

    // Base decomposition
    var decomp: List[SubProblem] = List(new SubProblem(List()))

    // Retry until we have enough subproblems
    while(decomp.size < count) {
      decomp = tryDecomposition(model, threshold)
      threshold = nextThreshold(threshold, decomp, count)
    }

    decomp
  }


}