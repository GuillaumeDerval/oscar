package oscar.cbls.lib.search.neighborhoods

import oscar.cbls._
import oscar.cbls.algo.search.{HotRestart, IdenticalAggregator}
import oscar.cbls.core.computation.{CBLSIntVar, Variable}
import oscar.cbls.core.search.{EasyNeighborhoodMultiLevel, First, LoopBehavior, Move}


case class DeltaTransfer(vars:Array[CBLSIntVar],
                         name:String = "SwapsNeighborhood",
                         searchZone1:()=>Iterable[Int] = null,
                         searchZone2:() => (Int,Int)=>Iterable[Int] = null,
                         searchZoneForDelta:() => (Int,Int) => (Int,Int) => Iterable[Int], //donne des delta à essayer (TOTO: faire un enwton raphson ou regula falsi ou dichotomoe ici!!!
                         symmetryCanBeBrokenOnIndices:Boolean = true,
                         selectFirstVariableBehavior:LoopBehavior = First(),
                         selectSecondVariableBehavior:LoopBehavior = First(),
                         selectDeltaBehavior:LoopBehavior = First(),
                         hotRestart:Boolean = true)
  extends EasyNeighborhoodMultiLevel[TransferMove](name){

  //the indice to start with for the exploration
  var firstVarIndice:Int = 0
  var firstVar:CBLSIntVar = null

  var secondVarIndice:Int = -1
  var secondVar:CBLSIntVar = null

  var delta:Int = 0

  override def exploreNeighborhood() {

    val firstIterationSchemeZone =
      if (searchZone1 == null) {
        if (hotRestart) {
          if (firstVarIndice >= vars.length) firstVarIndice = 0
          vars.indices startBy firstVarIndice
        } else vars.indices
      } else if (hotRestart) HotRestart(searchZone1(), firstVarIndice) else searchZone1()

    val searchZone2ForThisSearch = if (searchZone2 == null) null else searchZone2()
    val searchZoneForDeltaL1 = if(searchZoneForDelta == null) null else searchZoneForDelta()

    val (iIterator,notifyFound1) = selectFirstVariableBehavior.toIterator(firstIterationSchemeZone)
    while (iIterator.hasNext) {
      firstVarIndice = iIterator.next()

      firstVar = vars(firstVarIndice)
      val oldValOfFirstVar = firstVar.newValue

      val secondIterationSchemeZone = if (searchZone2ForThisSearch == null) vars.indices else searchZone2ForThisSearch(firstVarIndice,oldValOfFirstVar)
      val searchZoneForDeltaL2 = if(searchZoneForDeltaL1 == null) null else searchZoneForDeltaL1(firstVarIndice,oldValOfFirstVar)

      val (jIterator,notifyFound2) = selectSecondVariableBehavior.toIterator(secondIterationSchemeZone)
      while (jIterator.hasNext) {
        secondVarIndice = jIterator.next()
        secondVar = vars(secondVarIndice)
        val oldValOfSecondVar = secondVar.newValue

        if ((!symmetryCanBeBrokenOnIndices || firstVarIndice < secondVarIndice) //we break symmetry on variables
          && firstVarIndice != secondVarIndice
          && secondVar.domain.contains(oldValOfFirstVar)
          && firstVar.domain.contains(oldValOfSecondVar)) {

          this.secondVar = secondVar

          val searchZoneForDeltaL3 = if(searchZoneForDeltaL2 == null) null else searchZoneForDeltaL2(secondVarIndice,oldValOfSecondVar)

          val iterationOnDelta = if (searchZoneForDeltaL3 == null) ??? else searchZoneForDeltaL3
          //TODO: il faut cadrer dans le domaine des variables!!

          val (deltaIterator,notifyFound3) = selectDeltaBehavior.toIterator(iterationOnDelta)
          while (deltaIterator.hasNext) {
            val delta = deltaIterator.next()

            this.delta = delta

            if (evaluateCurrentMoveObjTrueIfSomethingFound(obj.swapVal(firstVar, secondVar))) {
              notifyFound1()
              notifyFound2()
              notifyFound3()
            }
          }
        }
      }
    }
    firstVarIndice = firstVarIndice +1
    secondVarIndice = -1
  }


  override def instantiateCurrentMove(newObj: Int) =
    TransferMove(firstVar, secondVar, delta:Int, firstVarIndice,secondVarIndice,newObj, name)

  //this resets the internal state of the Neighborhood
  override def reset(): Unit = {
    firstVarIndice = 0
  }
}


/** standard move that swaps the value of two CBLSIntVar
  *
  * @param i the variable
  * @param j the other variable
  * @param objAfter the objective after this assignation will be performed
  * @param neighborhoodName a string describing the neighborhood hat found the move (for debug purposes)
  * @author renaud.delandtsheer@cetic.be
  */
case class TransferMove(i:CBLSIntVar,j:CBLSIntVar, delta:Int, idI:Int, idJ:Int, override val objAfter:Int, override val neighborhoodName:String = null)
  extends Move(objAfter, neighborhoodName){

  override def commit() {
    i :+= delta
    j:-= delta
  }

  override def toString: String  = {
    neighborhoodNameToString + "Transfer(" + i + " :+= " + delta + " "  + j + ":-=" + delta + objToString + ")"
  }

  override def touchedVariables: List[Variable] = List(i,j)
}
