package oscar.cp.searches.lns.operators

import oscar.algo.search.SearchStatistics
import oscar.cp.searches.lns.CPIntSol
import oscar.cp.searches.lns.selection.AdaptiveStore

import scala.xml.Elem

/**
  * Adaptive large neighbourhood search operator with a single parameter of type T.
  *
  * @param function the function that the operator applies.
  * @param paramStore: an AdaptiveStore containing the possible values for the parameter.
  */
class ALNSSingleParamOperator[T](
                                  name: String,
                                  failThreshold: Int,
                                  val function: (T) => (CPIntSol => Unit, Option[Int], Option[Int]),
                                  val paramStore: AdaptiveStore[ALNSParameter[T]]
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
      () => function(param.value),
      (tStart, tEnd, objStart, objEnd, iterStats, fail, iter) =>
        updateParam(param, tStart, tEnd, objStart, objEnd, iterStats, fail, iter),
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

  //Warning: risk if used concurrently!
  override def getFunction: (CPIntSol => Unit, Option[Int], Option[Int]) = {
    selected = Some(paramStore.select())
    function(selected.get.value)
  }

  override def update(
                       tStart: Long,
                       tEnd: Long,
                       objStart: Int,
                       objEnd: Int,
                       iterStats: SearchStatistics,
                       fail: Boolean,
                       iter: Long
                     ): Unit = {
    if(selected.isDefined){
      updateParam(selected.get, tStart, tEnd, objStart, objEnd, iterStats, fail, iter)
      selected = None
      super.update(tStart, tEnd, objStart, objEnd, iterStats, fail, iter)
    }
    else throw new Exception("This operator has not been used!")
  }

  private def updateParam(
                           param: ALNSParameter[T],
                           tStart: Long,
                           tEnd: Long,
                           objStart: Int,
                           objEnd: Int,
                           iterStats: SearchStatistics,
                           fail: Boolean,
                           iter: Long
                         ): Unit ={
    param.update(tStart, tEnd, objStart, objEnd, iterStats, fail, iter)
    paramStore.adapt(param)
    if(!param.isActive){
      paramStore.deactivate(param)
      if(paramStore.isActiveEmpty){
        println("Operator " + name + " deactivated")
        setActive(false)
      }
    }
  }

  override def tuneParameters(): ALNSNoParamOperator = ???

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

  override def asXml(cat: String): Elem = {
    <operator>
      <name>{name}</name>
      <type>{cat}</type>
      {super.wrapStatsToXml()}
      <parameters>{paramStore.getElements.map(_.asXml("parameter"))}</parameters>
    </operator>
  }

  override def toString: String = {
    var s = "Operator:"
    s += "\n\tname: " + name
    s += "\n" + super.toString
    s += "\n\tParameters:" + paramStore.getElements.mkString("\n\t")
    s
  }
}
