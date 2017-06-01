package oscar.cp.searches.lns.operators

import oscar.algo.search.SearchStatistics
import oscar.cp.searches.lns.CPIntSol
import oscar.cp.searches.lns.search.ALNSStatistics
import oscar.cp.searches.lns.selection.AdaptiveStore

import scala.collection.mutable

/**
  * Adaptive large neighbourhood search operator with a single parameter of type T.
  *
  * @param function the function that the operator applies.
  * @param paramStore: an AdaptiveStore containing the possible values for the parameter.
  * @param metric the metric function used when adapting the operator parameter values.
  */
class ALNSSingleParamOperator[T](
                                  name: String,
                                  failThreshold: Int,
                                  val function: (CPIntSol, T) => Unit,
                                  val paramStore: AdaptiveStore[ALNSParameter[T]],
                                  val metric: (ALNSElement, Int, SearchStatistics) => Double
                                ) extends ALNSOperator(name, failThreshold){

  private var selected: Option[ALNSParameter[T]] = None

  /**
    * returns a reified operator representing this operator with the given parameter value.
    * @param param the parameter value in the internal adaptive store.
    */
  private def getReified(param: ALNSParameter[T]): ALNSReifiedOperator = {
    new ALNSReifiedOperator(
      name + "(" + param.value + ")",
      param.failThreshold,
      function(_:CPIntSol, param.value),
      (improvement, stats, fail) => updateParam(param, improvement, stats, fail),
      (state) => {
        param.setActive(state)
        if(!param.isActive) {
          paramStore.deactivate(param)
          if (paramStore.isActiveEmpty) setActive(false)
        }
      }
    )
  }

  def getReifiedParameters: Iterable[ALNSReifiedOperator] = paramStore.getElements.map(getReified)

  override def apply(sol: CPIntSol): Unit = {
    if(selected.isEmpty) {
      selected = Some(paramStore.select())
      function(sol, selected.get.value)
    }
    else throw new Exception("This operator has already been used but not updated!")
  }

  override def update(costImprovement: Int, stats: SearchStatistics, fail: Boolean): Unit = {
    if(selected.isDefined){
      updateParam(selected.get, improvement, stats, fail)
      selected = None
    }
    else throw new Exception("This operator has not been used!")
  }

  private def updateParam(param: ALNSParameter[T], costImprovement: Int, stats: SearchStatistics, fail: Boolean): Unit ={
    super.update(costImprovement, stats, fail)
    param.update(costImprovement, stats, fail)
    if(param.isActive)
      paramStore.adapt(param, metric(param, costImprovement, stats))
    else{
      paramStore.deactivate(param)
      if(paramStore.isActiveEmpty){
        println("Operator " + name + " deactivated")
        setActive(false)
      }
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
    Array(paramStore.getElements.map(x => (x.value.toString, x.getStats)).toArray)
  )

  override def nParamVals: Int = paramStore.nActive

  override def setActive(state: Boolean): Unit = {
    if(state){
      paramStore.getElements.foreach(_.setActive(state))
      paramStore.reset()
    }
    super.setActive(state)
  }
  override def resetFails(): Unit = {
    paramStore.getElements.foreach(_.resetFails())
    super.resetFails()
  }
}
