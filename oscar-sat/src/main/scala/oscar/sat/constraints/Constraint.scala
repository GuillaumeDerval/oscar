package oscar.sat.constraints

import oscar.algo.array.ArrayStackInt

abstract class Constraint {

  def simplify(): Boolean
  
  def explain(litId: Int, outReason: ArrayStackInt): Unit
  
  def explainAll(outReason: ArrayStackInt): Unit

  def setup(): Boolean
  
  def propagate(literal: Int): Boolean
  
  def isDeleted = false
}