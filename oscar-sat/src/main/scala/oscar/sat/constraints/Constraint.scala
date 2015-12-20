package oscar.sat.constraints

import oscar.algo.array.ArrayStackInt

abstract class Constraint {

  def setup(): Boolean
  
  def propagate(literal: Int): Boolean
  
  def explain(litId: Int, outReason: ArrayStackInt): Unit
  
  def isDeleted: Boolean = false
}