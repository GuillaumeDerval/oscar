package oscar.cp.searches.lns.operators

import oscar.algo.search.SearchStatistics
import oscar.cp.searches.lns.CPIntSol
import oscar.cp.searches.lns.search.ALNSStatistics
import oscar.cp.searches.lns.selection.AdaptiveStore

import scala.collection.mutable

/**
  * Adaptive large neighbourhood search operator with two parameters of type T1 and T2.
  *
  * @param function the function that the operator applies.
  * @param param1Store: an AdaptiveStore containing the possible values for the 1st parameter.
  * @param param2Store: an AdaptiveStore containing the possible values for the 2nd parameter.
  * @param metric the metric function used when adapting the operator parameters values.
  */
class ALNSTwoParamsOperator[T1, T2](
                                     name: String,
                                     failThreshold: Int,
                                     val function: (CPIntSol, T1, T2) => Unit,
                                     val param1Store: AdaptiveStore[ALNSParameter[T1]],
                                     val param2Store: AdaptiveStore[ALNSParameter[T2]],
                                     val metric: (ALNSElement, Int, SearchStatistics) => Double
                                   ) extends ALNSOperator(name, failThreshold){

  private var selected1: Option[ALNSParameter[T1]] = None
  private var selected2: Option[ALNSParameter[T2]] = None

  /**
    * returns a reified operator representing this operator with the given parameter value.
    * @param param1 The index of the 1st parameter value in the internal adaptive store.
    * @param param2 The index of the 2nd parameter value in the internal adaptive store.
    */
  private def getReified(param1: ALNSParameter[T1], param2: ALNSParameter[T2]): ALNSReifiedOperator = {
    new ALNSReifiedOperator(
      name + "(" + param1.value + ", " + param2.value + ")",
      Math.min(param1.failThreshold, param2.failThreshold),
      function(_:CPIntSol, param1.value, param2.value),
      (improvement, stats, fail) => updateParam(param1, param2, improvement, stats, fail),
      (state) => {
        if(!param1.isActive) {
          param1Store.deactivate(param1)
          if (param1Store.isActiveEmpty) setActive(false)
        }
        if(!param2.isActive) {
          param2Store.deactivate(param2)
          if (param2Store.isActiveEmpty) setActive(false)
        }
      }
    )
  }

  def getReifiedParameters: Iterable[ALNSReifiedOperator] = for{
    p1 <- param1Store.getElements
    p2 <- param2Store.getElements
  } yield getReified(p1, p2)

  override def apply(model:CPIntSol): Unit = {
    if(selected1.isEmpty && selected2.isEmpty) {
      selected1 = Some(param1Store.select())
      selected2 = Some(param2Store.select())
      function(model, selected1.get.value, selected2.get.value)
    }
    else throw new Exception("This operator has already been used but not updated!")
  }

  override def update(costImprovement: Int, stats: SearchStatistics, fail: Boolean): Unit = {
    if(selected1.isDefined && selected2.isDefined){
      updateParam(selected1.get, selected2.get, costImprovement, stats, fail)
      selected1 = None
      selected2 = None
    }
    else throw new Exception("This operator has not been used!")
  }

  private def updateParam(param1: ALNSParameter[T1], param2: ALNSParameter[T2], costImprovement: Int, stats: SearchStatistics, fail: Boolean): Unit ={
    super.update(costImprovement, stats, fail)

    param1.update(costImprovement, stats, fail)
    if(param1.isActive)
      param1Store.adapt(param1, metric(param1, costImprovement, stats))
    else{
      param1Store.deactivate(param1)
      if(param1Store.isActiveEmpty) setActive(false)
    }

    param2.update(costImprovement, stats, fail)
    if(param2.isActive)
      param2Store.adapt(param2, metric(param2, costImprovement, stats))
    else{
      param2Store.deactivate(param2)
      if(param2Store.isActiveEmpty) setActive(false)
    }
  }

  override def getStats: ALNSStatistics = new ALNSStatistics(
    execs,
    sols,
    successfulRuns,
    time,
    avgTime,
    improvement,
    avgImprovement,
    successRate,
    timeToImprovement,
    isActive,
    nFails,
    Array(
      param1Store.getElements.map(x => (x.value.toString, x.getStats)).toArray,
      param2Store.getElements.map(x => (x.value.toString, x.getStats)).toArray
    )
  )

  override def nParamVals: Int = nParamVals(1) * nParamVals(2)

  /**
    * Returns the number of active values for the specified parameter.
    */
  def nParamVals(param: Int): Int = param match{
    case 1 => param1Store.nActive
    case 2 => param2Store.nActive
  }

  override def setActive(state: Boolean): Unit = {
    if(state){
      param1Store.getElements.foreach(_.setActive(state))
      param2Store.getElements.foreach(_.setActive(state))
      param1Store.reset()
      param2Store.reset()
    }
    super.setActive(state)
  }
  override def resetFails(): Unit = {
    param1Store.getElements.foreach(_.resetFails())
    param2Store.getElements.foreach(_.resetFails())
    super.resetFails()
  }
}
