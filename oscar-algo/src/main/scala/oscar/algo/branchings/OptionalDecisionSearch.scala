package oscar.algo.branchings

import oscar.algo.reversible.{ReversibleInt, ReversibleSparseSet}
import oscar.algo.search._
import oscar.algo.vars.{BoolVarLike, IntVarLike}

/**
  * This search branches on boolean decision variables linked to optional variables:
  * When branching on a decision variable, it creates two child nodes, left=true, right=false.
  * On the true (left) branch, all the optional variables must be assigned before attempting a next decision.
  * On the false (right) branch, the optional variables are not branched on at all.
  *
  * @param decisions The OptionalDecision objects linking the boolean decision variables with their optional variables
  * @param decisionHeuristic The branching to be used on the decision variables
  *
  * @author Pierre Schaus  pschaus@gmail.com
  * @author Charles Thomas cftmthomas@gmail.com
  */
class OptionalDecisionSearch[B <: BoolVarLike, I <: IntVarLike](decisions: Seq[OptionalDecision[B, I]], decisionHeuristic: Branching) extends Branching {

  require(decisions.nonEmpty)

  private[this] val context = decisions.head.decisionVar.context

  private[this] val currentDecision = new ReversibleInt(context, -1) //Index of current branching var (-1 if none)
  private[this] val unbound = new ReversibleSparseSet(context, 0, decisions.length-1) //Unbound variables

  //Detect last var bound to true if any
  private def findNextI: Int = {
    var next = -1
    val unboundVars = unbound.toArray
    var j = unboundVars.length-1

    while(next < 0 && j >= 0){
      val decision = decisions(unboundVars(j)).decisionVar
      if(decision.isBound){
        if(decision.isTrue) next = unboundVars(j)
        else unbound.removeValue(unboundVars(j))
      }
      j-=1
    }

    next
  }

  override def alternatives(): Seq[Alternative] = {

    var selectNextDecision = false

    while(!selectNextDecision){
      //If decision var already selected:
      if(currentDecision.value  >= 0){
        val toBranchOn = decisions(currentDecision.value).optVarsHeuristic() //Getting branching in sub-tree
        if(toBranchOn.nonEmpty) return toBranchOn
        else currentDecision.setValue(-1)
      }
      //Else:
      else{
        val next = findNextI
        if(next >= 0){
          unbound.removeValue(next)
          currentDecision.setValue(next)
        }
        else selectNextDecision = true
      }
    }

    decisionHeuristic.alternatives()
  }
}

object OptionalDecisionSearch{
  def apply[B <: BoolVarLike, I <: IntVarLike](decisions: Seq[OptionalDecision[B, I]], decisionHeuristic: Branching) =
    new OptionalDecisionSearch(decisions, decisionHeuristic)
}
