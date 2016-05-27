package oscar.examples.cbls

import oscar.cbls.constraints.core.ConstraintSystem
import oscar.cbls.constraints.lib.global.{MultiKnapsack, MultiKnapsackLoad}
import oscar.cbls.invariants.core.computation.{CBLSIntVar, IntValue, Store}
import oscar.cbls.invariants.lib.logic.BinPackingLoad
import oscar.cbls.invariants.lib.numeric.Sum2
import oscar.cbls.search.{SearchEngine, StopWatch}

/**
  * Created by gustavbjordal on 27/05/16.
  */
object MultiKnapsackTest  extends SearchEngine with StopWatch {

    def main(args: Array[String]) {


      // model
      val m: Store = new Store(false, None, true)

      val binArray = Array(1,1,2,2,3,3)
      val sizeArray = Array(1,2,1,2,1,2)
      val binSizes = Array(3,3,3)

      val inBinVar:Array[IntValue] = binArray.map(CBLSIntVar(m,_,1 to 3, "item"))
      val sizeVar:Array[IntValue] = sizeArray.map(CBLSIntVar(m,_,1 to 5, "size"))
      val binSizeVar:Array[CBLSIntVar] = binSizes.map(CBLSIntVar(m,_,1 to 20, "bin_size"))


      val c = ConstraintSystem(m)

      val bin_pack = BinPackingLoad(inBinVar.map(Sum2(_,-1)),sizeArray)
      bin_pack.PostInvariants(binSizeVar)

      for(v <- inBinVar)
        println(c.violation(v))
      for(b <- binSizeVar)
        println(c.violation(b))

      c.close()
      // closing model
      m.close()

      def printViolation() = {
        println(c.violation)
        for(v <- inBinVar)
          println(v + ": " + c.violation(v))
        for(b <- binSizeVar)
          println(b + ": " + c.violation(b))
      }
      printViolation()
      inBinVar(0).asInstanceOf[CBLSIntVar].setValue(2)
      printViolation()
    }
  }
