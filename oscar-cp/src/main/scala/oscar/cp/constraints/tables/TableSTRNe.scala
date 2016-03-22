package oscar.cp.constraints.tables

import oscar.cp.core.variables.CPIntVar
import oscar.cp.core.Constraint
import oscar.cp.core.CPOutcome
import oscar.cp.core.CPOutcome._
import oscar.cp.core.CPPropagStrength
import oscar.algo.reversible.ReversibleInt
import java.util.Arrays

/**
 * @author ThanhKM thanhkhongminh@gmail.com
 * 
 * Class for Negative Table Constraint
 * 
 * Implementation follows https://www.aaai.org/ocs/index.php/AAAI/AAAI13/paper/viewFile/6141/6825 
 * and improves with ideas of STR2 (Sval, Ssup)
 * 
 */
class TableSTRNe(val variables: Array[CPIntVar], table: Array[Array[Int]]) extends Constraint(variables(0).store, "TableSTRNe") {
  idempotent = true
  
  private[this] val arity = variables.length
  
  // Sparse set for current table
  private[this] val position = Array.tabulate(table.length)(i => i)
  private[this] val revLimit = new ReversibleInt(s, table.length - 1)

  // Dense + sparse set for (variable, value) (unsupported set)
  private[this] val unsDense = Array.tabulate(arity)(i => new Array[Int](variables(i).size)) // store values
  private[this] val unsSparse = Array.tabulate(arity)(i => new Array[Int](variables(i).max - variables(i).min + 1)) // sparse
  private[this] val minValues = Array.tabulate(arity)(i => variables(i).min) // min values of each variables 
  private[this] val unsSizes = Array.tabulate(arity)(i => variables(i).size) // size of sparse set

  // Ssup, Sval
  private[this] var sSupLimit = -1
  private[this] val sSup = Array.tabulate(arity)(i => i)  
  private[this] var sValLimit = -1
  private[this] val sVal = Array.tabulate(arity)(i => i)
  
  // variable's domain size since last invocation 
  private[this] val lastSizes = Array.tabulate(variables.length)(i => new ReversibleInt(s, -1))
  
  // count(x,a): number of support possibles for each literal (x,a)
  private[this] val count = Array.tabulate(arity)(i => new Array[Int](variables(i).size))
  
  override def setup(l: CPPropagStrength): CPOutcome = {
    if (propagate() == Failure) return Failure
    variables.filter(!_.isBound).foreach(_.callPropagateWhenDomainChanges(this))
    CPOutcome.Suspend
  }

  override def propagate(): CPOutcome = {

    var limit = revLimit.getValue()
    //---------------------- initialize -------------------------------------/
    sValLimit = -1 // reset Sval
    sSupLimit = -1 // reset Ssup
    var i = 0
    while (i < arity) {
    	copyDomainsToUns(i)
      sSupLimit += 1
      sSup(sSupLimit) = i

      // SVal
      if (variables(i).size != lastSizes(i).getValue) {
        sValLimit += 1
        sVal(sValLimit) = i
        lastSizes(i).setValue(variables(i).size)
      }
      i += 1
    }
    
    // calculate the number of tuple possibles for this constraint 
    i = 0
    var nTuplePossibles = 1
    while (i < arity) {
      nTuplePossibles *= variables(i).size
      i += 1
    }
    // calculate count(x,a) for each variable. count(x,a) = nTuplePossibles / dom(x)
    // if count(x,a) > tableSize, it means (x,a) is GAC
    i = arity - 1
    while (i >= 0) {
      val nTuples = nTuplePossibles / variables(i).size
      if (nTuples > limit + 1) {
        sSup(i) = sSup(sSupLimit)
        sSupLimit -= 1
      } else
        //count(i) = Array.fill(variables(i).size)(nTuples)
        Arrays.fill(count(i), 0, variables(i).size, nTuples)
      i -= 1
    }
    
    //-------------- iterate global table ------------------------------/
    i = 0
    while (i <= limit) {
      val tau = table(position(i))
      
      /*
      if (isValidTuple(tau) != isValidTupleWithSVal(tau)) {
        println("------------\ntuple:")
        printArray(tau)
        println("variables: " + variables.toSeq)
        println("table:")
        for (j <- 0 to limit) {
          print(position(j) + ": ")
          printArray(table(position(j)))
        }
      }
      */
        
      // if the tuple is valid
      if (isValidTupleWithSVal(tau)) {//if (isValidTuple(tau)) {
        unsupport(tau, limit+1)
        i += 1
      } else { // remove tuple
        swap(position, i, limit)
        limit -= 1
      }
    }
    revLimit.setValue(limit)
    
    //---------------- update domains ---------------------------------/
    if (limit == -1) return Success
		updateDomains()
  }
  
  /*****************************************************************************/
  /************************** Helper functions *********************************/
  /**
   * Copy domain of variables to sparse set
   */
  @inline private def copyDomainsToUns(varId: Int): Unit = {
    val size = variables(varId).fillArray(unsDense(varId))
    unsSizes(varId) = size
    var i = 0
    while (i < size) {
      val value = unsDense(varId)(i)
      unsSparse(varId)(value - minValues(varId)) = i
      i += 1
    }
  }
  
  /**
   * Remove (varId, value) from Uns.
   * @return new size of Uns(varId)
   */
  @inline private def removeFromUns(varId: Int, value: Int): Int = {
    val valIndex = unsSparse(varId)(value - minValues(varId))
    // value
    val size = unsSizes(varId)
    if (valIndex < size) {
      val value2 = unsDense(varId)(size - 1)
      val valIndex2 = size - 1
      swap(unsDense(varId), valIndex, valIndex2)
      swap(unsSparse(varId), value - minValues(varId), value2 - minValues(varId))
      unsSizes(varId) = size - 1
    }
    unsSizes(varId)
  }
  
  /**
   *  Reduce count for each value of variable in this tuple
   */
  @inline private def unsupport(tau: Array[Int], tbSize: Int): Unit = {
    var i = sSupLimit
    while (i >= 0) {
      val varId = sSup(i)
      val value = tau(varId)
      val valIndex = unsSparse(varId)(value - minValues(varId))
      
      count(varId)(valIndex) -= 1
      
      // when count(x,a) > tbSize, it means (x,a) is GAC, we have to remove it
      if (count(varId)(valIndex) > tbSize) {
        // Remove from Uns
        val unsSize = removeFromUns(varId, value)
        if (unsSize == 0) {
          sSup(i) = sSup(sSupLimit)
          sSupLimit -= 1
        }
      }
      
      i -= 1
    }
  }
  
  /**
   * Update variables' domain and return CPOutcome i.e. Suspend, Failure,... 
   */
  @inline private def updateDomains(): CPOutcome = {
    var i = 0
    while (i <= sSupLimit) {
      val varId = sSup(i)

      val size = unsSizes(varId)
      var j = 0
      while (j < size) {
        val value = unsDense(varId)(j)
        val valIndex = unsSparse(varId)(value - minValues(varId))
        
        if (count(varId)(valIndex) == 0) {
          if (variables(varId).size == 1)
            return Failure
          variables(varId).removeValue(value)
          // println("variables: " + (varId) + " removeValue " +value)
        }
        j += 1
      }
      // Can not do like STR2 because after remove some values, the global table is not GAC
      // i.e. not all tuple are GAC.
      // lastSizes(varId).setValue(variables(varId).size)
      i += 1
    }
    
    Suspend
  }
  
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

  /**
   *  Check valid tuple by presence of all values in variables' domain
   */
  @inline private def isValidTuple(tau: Array[Int]): Boolean = {
      var varId = 0
      while (varId < arity) {
        if (!variables(varId).hasValue(tau(varId)))
          return false        
        varId += 1
      }
      true
  }
  
  @inline private def swap(arrays: Array[Int], i1: Int, i2: Int) = {
    val tmp = arrays(i1)
    arrays(i1) = arrays(i2)
    arrays(i2) = tmp
  }
  
  @inline private def printArray(arrays: Array[Int]) = {
    for (a <- arrays) print(a + " ")
    println
  }
}
