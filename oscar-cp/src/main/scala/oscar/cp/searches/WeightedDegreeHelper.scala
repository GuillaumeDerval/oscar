package oscar.cp.searches

import oscar.cp.{CPIntVar, CPStore}
import oscar.cp.core.variables.CPVar

import scala.collection.mutable

/**
 * Helps to compute the weighted degree of each CPIntVar given.
 * The weight of a constraint is the number of times it has been involved in a failure.
 * The weighted degree of a variable is the sum of the weights of all the constraints associated with this variable
 */
class WeightedDegreeHelper(cpSolver: CPStore, vars: Array[CPIntVar], decreaseRatio: Double) {
  val degree: mutable.HashMap[CPVar, Double] = new mutable.HashMap[CPVar, Double]()
  for(v <- vars)
    degree(v) = v.constraintDegree

  //Add our callback to the solver
  cpSolver.onFailure(onFailure())

  /**
   * Get the domain of the variable on the weighted degree of the variable.
   */
  def getDomOnWeightedDegree(v: CPIntVar): Double = v.size.toDouble / degree(v)

  /**
   * Get the weighted degree of a variable
   */
  def getWeightedDegree(v: CPIntVar): Double = degree(v)

  def onFailure(): Unit = {
    degree.transform((v, d) => d*decreaseRatio)
    if(cpSolver.lastConstraintCalled != null)
      for(v <- cpSolver.lastConstraintCalled.associatedVars())
        if(degree.contains(v))
          degree(v) += 1.0
  }
}
