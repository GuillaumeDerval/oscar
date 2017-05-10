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
  private var currentId = 0L
  private lazy val selectedMap: mutable.Map[Long, (ALNSParameter[T1], ALNSParameter[T2])] = mutable.Map[Long, (ALNSParameter[T1], ALNSParameter[T2])]()
  //TODO: Review selection mechanism

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
          param1Store.remove(param1)
          if (param1Store.isEmpty) setActive(false)
        }
        if(!param2.isActive) {
          param2Store.remove(param2)
          if (param2Store.isEmpty) setActive(false)
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
      if(selected1.isEmpty) selected1 = Some(param1Store.select())
      if(selected2.isEmpty) selected2 = Some(param2Store.select())
      function(model, selected1.get.value, selected2.get.value)
    }
    else ??? //TODO: Throw exception
  }

  override def get(): (Long, (CPIntSol) => Unit) = {
    val param1 = param1Store.select()
    val param2 = param2Store.select()
    val id = currentId
    currentId += 1
    selectedMap += id -> (param1, param2)
    (id, function(_, param1.value, param2.value))
  }

  override def update(costImprovement: Int, stats: SearchStatistics, fail: Boolean): Unit = {
    if(selected1.isDefined && selected2.isDefined){
      updateParam(selected1.get, selected2.get, costImprovement, stats, fail)
      selected1 = None
      selected2 = None
    }
    else ??? //TODO: Throw exception
  }

  override def update(id: Long, costImprovement: Int, stats: SearchStatistics, fail: Boolean): Unit = {
    if(selectedMap.contains(id)){
      updateParam(selectedMap(id)._1, selectedMap(id)._2, costImprovement, stats, fail)
      selectedMap -= id
    }
    else ??? //TODO: Throw exception
  }

  private def updateParam(param1: ALNSParameter[T1], param2: ALNSParameter[T2], costImprovement: Int, stats: SearchStatistics, fail: Boolean): Unit ={
    super.update(costImprovement, stats, fail)

    param1.update(costImprovement, stats, fail)
    if(param1.isActive)
      param1Store.adapt(param1, metric(param1, costImprovement, stats))
    else{
      param1Store.remove(param1)
      if(param1Store.isEmpty) setActive(false)
    }

    param2.update(costImprovement, stats, fail)
    if(param2.isActive)
      param2Store.adapt(param2, metric(param2, costImprovement, stats))
    else{
      param2Store.remove(param2)
      if(param2Store.isEmpty) setActive(false)
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
    * Returns the number of different values for the specified parameter.
    */
  def nParamVals(param: Int): Int = param match{
    case 1 => param1Store.nElements
    case 2 => param2Store.nElements
  }
}
