package oscar.algo.search

import oscar.algo.vars.{BoolVarLike, IntVarLike}

/**
  * Represents a dependency in the search between a boolean decision variable and
  * other optional variables: we want to branch on the optional variables only if the decision variable is set to true.
  *
  * @param decisionVar The boolean decision variable
  * @param optionalVars The optional variables
  * @param optVarsBranching The branching to be used on the optional variables
  *
  * @author Pierre Schaus  pschaus@gmail.com
  * @author Charles Thomas cftmthomas@gmail.com
  */
class OptionalDecision[B <: BoolVarLike, I <: IntVarLike](
                                                           val decisionVar: B,
                                                           val optionalVars: Seq[I],
                                                           val optVarsBranching: Branching
                                                         ) {

  def optVarsHeuristic(): Seq[Alternative] = optVarsBranching.alternatives()
}

object OptionalDecision{
  def apply[B <: BoolVarLike, I <: IntVarLike](decisionVar: B, optionalVars: Seq[I], optVarsBranching: Branching) =
    new OptionalDecision(decisionVar, optionalVars, optVarsBranching)
}