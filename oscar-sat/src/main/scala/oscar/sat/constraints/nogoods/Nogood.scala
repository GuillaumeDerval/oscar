package oscar.sat.constraints.nogoods

import oscar.algo.array.ArrayStackInt
import oscar.sat.constraints.clauses.Clause
import oscar.sat.core.CDCLStore

abstract class Nogood extends Clause {
  
  def activity: Double
  
  def activity_=(d: Double): Unit
  
  def locked: Boolean
}

object Nogood {
  
  def apply(solver: CDCLStore, literals: ArrayStackInt): Nogood = {
    if (literals.length == 2) binary(solver, literals(0), literals(1))
    else watchLiterals(solver, literals.toArray)
  }
  
  def watchLiterals(solver: CDCLStore, literals: Array[Int]): Nogood = {
    new WLNogood(solver, literals)
  }
  
  def binary(solver: CDCLStore, lit1: Int, lit2: Int): Nogood = {
    new BinaryNogood(solver, lit1, lit2)
  }
}