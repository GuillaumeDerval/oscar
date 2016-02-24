package oscar.cp.constraints.tables

import oscar.cp.core.Constraint
import oscar.cp.core.variables.CPIntVar
import oscar.cp.core.variables.CPBoolVar
import oscar.cp.core.CPOutcome
import oscar.cp.core.CPOutcome._
import oscar.cp.core.CPPropagStrength
import oscar.algo.reversible.ReversibleInt
import oscar.cp.core.CPStore
import oscar.cp.constraints.TableSTR2

/**
 * @author ThanhKM thanhkhongminh@gmail.com
 */
class TableSTR2Reif(val variables: Array[CPIntVar], table: Array[Array[Int]], val b: CPBoolVar) extends Constraint(variables(0).store, "TableSTR2Reif") {
  idempotent = true
  private[this] val arity = variables.length
  
  // Sparse set for current table
  private[this] val position = Array.tabulate(table.length)(i => i)
  private[this] val revLimit = new ReversibleInt(s, table.length - 1)
  // SVal
  private[this] val sVal = Array.tabulate(arity)(i => i)
  private[this] var sValLimit = -1

  private[this] val lastSizes = Array.tabulate(variables.length)(i => new ReversibleInt(s, -1))
  
  override def setup(l: CPPropagStrength): CPOutcome = {
    val outcome = propagate()
    if (outcome == Failure) return Failure
    else if (outcome == Success) return Success
    
    variables.filter(!_.isBound).foreach(_.callPropagateWhenDomainChanges(this))
    if (!b.isBound) b.callPropagateWhenDomainChanges(this)
    Suspend
  }
  
  override def propagate(): CPOutcome = {
    // check if b is bound
    if (b.isBound) {
      if (b.min == 1) {
        if (s.post(new TableSTR2(variables, table)) == Failure)
          return Failure
      } else if (b.min == 0) {
        if (s.post(new TableSTRNe(variables, table)) == Failure)
          return Failure
      }
      return Success
    }
    
    var limit = revLimit.getValue // save limit

    //--------------------------- setup SVal ------------------------------------/
		// Reset SVal
		sValLimit = - 1
    var varId = 0
    while (varId < arity) {      
      // SVal
      if (variables(varId).size != lastSizes(varId).getValue) {
        //add variable's index to SVal
        sValLimit += 1
        sVal(sValLimit) = varId

        lastSizes(varId).setValue(variables(varId).size)
      }
      varId += 1
    }
    
    //----------------------- iterate global table ------------------------------/
    var i = limit    
    while (i >= 0){
      val tau = table(position(i))
      if (!isValidTupleWithSVal(tau)) {
        swap(position, i, limit)
        limit -= 1
      }      
      i -= 1
    }
    revLimit.setValue(limit)
    
    //------------------------ check entailed -----------------------------------/
    // Check disentailed
    if (revLimit.value == -1) {
      if (b.assign(0) == Failure) return Failure
      return Success
    }
    
    // Compute number of valid tuples
    var nbValidTuples = 1 
    varId = 0
    while (varId < arity) {
      nbValidTuples = nbValidTuples * variables(varId).size
      varId += 1
    }
    // variables.foreach {x => (nbValidTuples *= x.size)}
    
    // Check entailed
    if (nbValidTuples == revLimit.getValue + 1) {
      if (b.assign(1) == Failure) return Failure
      return Success
    }
    
    Suspend
  }
  
  /*****************************************************************************/
  /************************** Helper functions *********************************/
  
  /**
   *  Check tuple validity with SVal
   */
  @inline private def isValidTupleWithSVal(tau: Array[Int]): Boolean = {
      var i = 0
      while (i <= sValLimit) {
        val varId = sVal(i)
        if (!variables(varId).hasValue(tau(varId)))
          return false
        
        i += 1
      }
      true
  } 
  
  @inline private def swap(arrays: Array[Int], i1: Int, i2: Int) = {
    val tmp = arrays(i1)
    arrays(i1) = arrays(i2)
    arrays(i2) = tmp
  }
    
}