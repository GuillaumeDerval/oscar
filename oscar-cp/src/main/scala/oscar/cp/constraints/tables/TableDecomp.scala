package oscar.cp.constraints.tables

import oscar.algo.search.Outcome
import oscar.cp.core.variables.CPIntVar
import oscar.cp.core.Constraint
import oscar.cp.core.CPPropagStrength
import oscar.algo.search.Outcome._

class TableDecomp(val X: Array[CPIntVar], table: Array[Array[Int]]) extends Constraint(X(0).store, "TableDecomp"){
  
  override def setup(l: CPPropagStrength): Outcome = {
    idempotent = true
    if (propagate() == Outcome.Failure) return Outcome.Failure
    X.filter(!_.isBound).foreach(_.callPropagateWhenDomainChanges(this))
    Suspend
  }

  override def propagate(): Outcome = {
    for (i <- 0 until X.size) {
      for (v <- X(i).min to X(i).max if X(i).hasValue(v)) {
        var valueIsSupported = false
        for (tuple <- table if (!valueIsSupported && tuple(i) == v)) {
          var allValueVariableSupported = true
          for (j <- 0 until X.size if (j != i)) {
            if (allValueVariableSupported && !X(j).hasValue(tuple(j)))
              allValueVariableSupported = false
          }
          valueIsSupported = allValueVariableSupported
        }
        if (!valueIsSupported) {
          if (X(i).removeValue(v) == Failure)
            return Failure
        }

      }
    }
    Suspend    
  }
  
}

    /*
    for (variable <-X; value <- variable) {
      val varIndex = X.indexOf(variable)
      var valueIsSupported = false
      for (tuple <- table if (!valueIsSupported && tuple(varIndex) == value)) {
        var allValueVariableSupported = true
        for (otherVariable <- X if otherVariable != variable) {
          if (allValueVariableSupported && !otherVariable.hasValue(tuple(X.indexOf(otherVariable)))) 
              allValueVariableSupported = false
        }
        valueIsSupported = allValueVariableSupported
      }
      if (!valueIsSupported){
        if(variable.removeValue(value) == Failure)
          return Failure
      }
        
    }
    Suspend
    */