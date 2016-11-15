package oscar.cp.constraints

import oscar.algo.search.Outcome
import oscar.cp.core.variables.CPGraphVar
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.Constraint

/**
 * @author Andrew Lambert andrew.lambert@student.uclouvain.be
 * Create Graph Basic Constraints to modify domains and call propagate when domain change
 */

class RequiresNode(val G: CPGraphVar, n: Int) extends Constraint(G.s, "Node required") {

  override def setup(l: CPPropagStrength): Outcome = {
    G.addNodeToGraph(n)
  }

}

class ExcludesNode(val G: CPGraphVar, n: Int) extends Constraint(G.s, "Node excluded") {

  override def setup(l: CPPropagStrength): Outcome = {
    G.removeNodeFromGraph(n)
  }

}

class RequiresEdge(val G: CPGraphVar, src: Int, dest: Int) extends Constraint(G.s, "Edge required") {

  override def setup(l: CPPropagStrength): Outcome = {
    G.addEdgeToGraph(src,dest)
  }

}

class ExcludesEdge(val G: CPGraphVar, src: Int, dest: Int) extends Constraint(G.s, "Edge excluded") {

  override def setup(l: CPPropagStrength): Outcome = {
    G.removeEdgeFromGraph(src,dest)
  }

}
