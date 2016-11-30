package oscar.modeling.solvers.cp.decompositions

import oscar.modeling.constraints.Constraint
import oscar.modeling.models.UninstantiatedModel
import oscar.cp.core.NoSolutionException
import oscar.modeling.models.cp.MemoCPModel
import oscar.modeling.solvers.cp.Branchings.BranchingInstantiator
import oscar.modeling.solvers.cp.distributed.{SubProblem, SubProblemDiscrepancy, SubProblemMinBound}

import scala.collection.mutable

/**
  * A refinement strategy for decomposing a CP problem into subproblems. The idea is to put all current subproblems into a priority queue.
  * While there is not enough subproblems, take the first subproblem on the queue and divide it
  * @param searchInstantiator the search to be used
  * @tparam SubproblemOrdering an object that allows to order the subproblems
  */
abstract class RefinementStrategy[SubproblemOrdering](searchInstantiator: BranchingInstantiator)(implicit ordering: Ordering[SubproblemOrdering])
  extends DecompositionStrategy {

  protected def generate(memoCPModel: MemoCPModel, assignment: List[Constraint], path: List[Int]): SubproblemOrdering

  private case class SubproblemInfo(assignment: List[Constraint], path: List[Int], orderInfo: SubproblemOrdering) extends Ordered[SubproblemInfo] {
    override def compare(that: SubproblemInfo): Int = ordering.compare(orderInfo, that.orderInfo)
  }

  /**
    * Decompose the problem
    *
    * @param baseModel the model to decompose
    * @param count the (minimum) number of subproblems wanted
    * @return A list of assignation to variable that makes the subproblem, along with the associated SubproblemData
    */
  override def decompose(baseModel: UninstantiatedModel, count: Int): List[SubProblem] = {
    if(count == 0) //no decomposition
      return List[SubProblem](new SubProblem(List()))

    //Initialise a CP Model
    val model = new MemoCPModel(baseModel.removeOptimisation())

    //Initialise the Priority Queue
    val q = mutable.PriorityQueue[SubproblemInfo]()
    val solutions = mutable.ArrayBuffer[SubproblemInfo]()

    model.declaration.apply(model) {
      q += SubproblemInfo(List(), List(), generate(model, List(), List()))

      val search = searchInstantiator(model)

      while(q.size < count) {
        //Dequeue the largest subproblem, and compute its domain
        val sp = q.dequeue()
        model.pushState()
        for(c <- sp.assignment)
          model.post(c)

        //Get the children for this state
        val alternatives = search.alternatives()
        if(alternatives.isEmpty) {
          solutions += sp
        }
        else {
          for((alternative, idx) <- alternatives.zipWithIndex) {
            model.pushState()
            try
            {
              alternative()
              val addedConstraints = model.getAddedConstraints
              val newPath = sp.path ++ List(idx)
              q += SubproblemInfo(addedConstraints, newPath, generate(model, addedConstraints, newPath))
            }
            catch { case _: NoSolutionException => }
            model.popState()
          }
        }
        //Do not forget to pop the state
        model.popState()
      }
    }

    //Sort by appearance in the tree (restore search order)
    val r = q.toList.sortWith((a, b) => {
      var ok = false
      var result = false
      for((i,j) <- a.path.zip(b.path); if !ok) {
        if(i < j){
          ok = true
          result = true
        }
        else if(j < i) {
          ok = true
          result = false
        }
      }
      result
    })

    r.map(sp => extendSubProblem(
      new SubProblem(sp.assignment)
        .addData(SubProblemDiscrepancy, sp.path.sum)
        .addData(SubProblemMinBound, SubProblemMinBound.compute(model.optimisationMethod)),
      sp.orderInfo)
    )
  }

  def extendSubProblem(subproblem: SubProblem, orderInfo: SubproblemOrdering): SubProblem = subproblem
}
