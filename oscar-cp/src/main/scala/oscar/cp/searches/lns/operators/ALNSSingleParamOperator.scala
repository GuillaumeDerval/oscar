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
  private var currentId = 0L
  private lazy val selectedMap: mutable.Map[Long, ALNSParameter[T]] = mutable.Map[Long, ALNSParameter[T]]()
  private val removedParams = mutable.HashSet[ALNSParameter[T]]()
  //TODO: review selection mechanism

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
          paramStore.remove(param)
          if (paramStore.isEmpty) setActive(false)
        }
      }
    )
  }

  def getReifiedParameters: Iterable[ALNSReifiedOperator] = paramStore.getElements.map(getReified)

  override def apply(model: CPIntSol): Unit = {
    if(selected.isEmpty) {
      if (selected.isEmpty) selected = Some(paramStore.select())
      function(model, selected.get.value)
    }
    else ??? //TODO: Throw exception
  }

  override def get(): (Long, (CPIntSol) => Unit) = {
    val param = paramStore.select()
    val id = currentId
    currentId += 1
    selectedMap += id -> param
    (id, function(_, param.value))
  }

  override def update(id: Long, costImprovement: Int, stats: SearchStatistics, fail: Boolean): Unit = {
    if(selectedMap.contains(id)){
      updateParam(selectedMap(id), costImprovement, stats, fail)
      selectedMap -= id
    }
    else ??? //TODO: Throw exception
  }

  override def update(costImprovement: Int, stats: SearchStatistics, fail: Boolean): Unit = {
    if(selected.isDefined){
      updateParam(selected.get, improvement, stats, fail)
      selected = None
    }
    else ??? //TODO: Throw exception
  }

  private def updateParam(param: ALNSParameter[T], costImprovement: Int, stats: SearchStatistics, fail: Boolean): Unit ={
    super.update(costImprovement, stats, fail)
    param.update(costImprovement, stats, fail)
    if(param.isActive)
      paramStore.adapt(param, metric(param, costImprovement, stats))
    else{
      paramStore.remove(param)
      removedParams += param
      if(paramStore.isEmpty){
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

  override def nParamVals: Int = paramStore.nElements

  override def setActive(state: Boolean): Unit = {
    if(state){
      removedParams.foreach(param =>{
        param.setActive(state)
        paramStore.add(param, metric(param, 0, new SearchStatistics(0, 0, 0L, false,0L, 0, 0)))
      })
    }
    super.setActive(state)
  }
  override def resetFails(): Unit = {
    removedParams.foreach(_.resetFails())
    paramStore.getElements.foreach(_.resetFails())
    super.resetFails()
  }
}
