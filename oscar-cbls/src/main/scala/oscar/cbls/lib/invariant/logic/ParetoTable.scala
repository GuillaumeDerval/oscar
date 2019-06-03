package oscar.cbls.lib.invariant.logic

import oscar.cbls.IntValue
import oscar.cbls.algo.quick.QList
import oscar.cbls.core.computation.ChangingIntValue
import oscar.cbls.core.{IntInvariant, IntNotificationTarget}

//the output is the first indice i in the table such tat for each j, table(i)(j) >= variables(j)
//default if no such line exist
class ParetoTable(variables:Array[IntValue],
                  tables:Array[Array[Long]],
                  defaultIfNoDominate:Long)
  extends IntInvariant
    with IntNotificationTarget{
  val t = tables.length
  val d = variables.length
  for(t <- tables) require(t.length == d)

  registerStaticAndDynamicDependencyArrayIndex(variables)
  this.finishInitialization()


  val smallestAmongAllRows = Array.tabulate(d)(i => tables.map(row => row(i)).min)
  val dimensionList = QList.buildFromIterable(0 until d)

  var exploreFrom:Int = -1
  override def notifyIntChanged(v: ChangingIntValue, id: Int, oldVal: Long, newVal: Long): Unit = {
    //a set of possible schedules:
    //restart
    //something was increased, but still fits the current line, => no propagate
    //something has increased and does not fit anymore on the current line => schedule,start from old
    //someting has decreased: start from scratch

    //startFromScratch is represented by exploreFrom set to -1
    //otherwise it is the line where we start from, excluded
    //this basically is the same

    if(isScheduled) return
    if(newVal > oldVal && exploreFrom != -1){
      //it has increased and there is a chance to set a starting point
      if(tables(this.newValue.toInt)(id) >= newVal) {
        //current line can still accomodate this new value, so nothing to do
      }else{
        //curent line cannot accomodate this change, so we schyedle for propagation,
        // but still, we can start at the current line forhte exploration
        this.scheduleForPropagation()
      }
    }else{
      //it decreases, so we schedule and forget the startFroom
      this.scheduleForPropagation()
      exploreFrom = -1
    }
  }

  override def performInvariantPropagation(): Unit = {
    val a = searchFromScratchLin(variables.map(_.value),exploreFrom)
    this := (if (a == -1) defaultIfNoDominate else a)
    exploreFrom = a
  }

  def searchFromScratchLin(v:Array[Long],staAt:Int):Int = {
    var i = staAt
    //if the smallest is bigger or equal to the value, we do not need to check it.
    //this is a speedup when there are many dimensions, with many zero's as we expect here.
    val relevantDimensions:QList[Int] = QList.qFilter(dimensionList,i => smallestAmongAllRows(i) < v(i))

    while(i < t){
      if(dominates(tables(i),v,relevantDimensions)) return i
      i = i+1
    }
    -1
  }

  def dominates(x:Array[Long],y:Array[Long],relevantDimensions:QList[Int]):Boolean = {
    var d = relevantDimensions
    while(d != null){
      val i = d.head
      if(x(i) < y(i)) return false
      d = d.tail
    }
    true
  }
}


