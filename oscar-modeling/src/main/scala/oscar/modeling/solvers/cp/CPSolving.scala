package oscar.modeling.solvers.cp

/**
  * Indicates that the model of a SolverApp can be solved by any CP solver
  */
trait CPSolving extends SequentialCPSolving with ParallelCPSolving
