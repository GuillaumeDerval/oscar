package oscar.cp.searches.lns.operators

import scala.collection.mutable

/**
  * This class keeps track of the closeness relationship between variables. The closeness is defined as the average
  * volume of propagation that is involved on one variable when another is instantiated.
  */
class ClosenessStore(val length: Int){
  val nImpacted: Array[Array[Int]] = Array.tabulate(length, length){(_, _) => 0}
  val closeness: Array[Array[Double]] = Array.tabulate(length, length){(i, j) => if(i == j) 1.0 else 0.0}

  /**
    * Updates the closeness store with the propagation given an instantiated variable.
    * @param instantiated the var that has been instantiated.
    * @param propagation the propagation for each other var (a negative value means that the var was already
    *                    instantiated and thus it should not be updated).
    */
  def update(instantiated: Int, propagation: Array[Double]): Unit ={
    for(i <- propagation.indices) if(i != instantiated && propagation(i) >= 0.0){

      //Updating average propagation:
      var newCloseness = (closeness(instantiated)(i) * nImpacted(instantiated)(i) + propagation(i)) / (nImpacted(instantiated)(i) + 1)
      closeness(instantiated)(i) = newCloseness
      nImpacted(instantiated)(i) +=1
    }
  }

  /**
    * Get the closeness between vars i and j.
    * @return the closeness between i and j or 0.0 if i or j are not valid indexes.
    */
  def getCloseness(i:Int, j:Int): Double = if(i < length && j < length) closeness(i)(j) else 0.0

  /**
    * Get the close vars of i.
    * @return a list of the close vars of i (empty if no var is close or i is not a valid index).
    */
  def getClose(i: Int): Iterable[Int] ={
    var closeSet = new mutable.ListBuffer[Int]
    if(!(i < length)) return closeSet

    for(v <- closeness(i).indices) if(v != i && closeness(i)(v) > 0.0)
      closeSet += v

    closeSet
  }

  /**
    * returns the close subset to which initalVar is a part of.
    * @return all the vars in the subset excluding initial var (might be empty).
    */
  def getCloseSubset(initialVar: Int, maxSubsetSize: Int): mutable.LinkedHashSet[Int]= {
    val subset = new mutable.LinkedHashSet[Int]  //Current subset
    if(initialVar >= length) return subset

    val closeToSubset = Array.fill[Double](length){0.0}
    val toAddNext = new mutable.PriorityQueue[Int]()(Ordering.by[Int, Double](i => closeToSubset(i)))
    for(v <- getClose(initialVar)){
      closeToSubset(v) = closeness(initialVar)(v)
      toAddNext += v
    }

    while(subset.size < maxSubsetSize && toAddNext.nonEmpty){
      val v = toAddNext.dequeue()
      if(v != initialVar && !subset.contains(v)){
        subset += v
        for(x <- getClose(v)) if(!subset.contains(v) && closeness(v)(x) > closeToSubset(x)){
          closeToSubset(x) = closeness(v)(x)
          toAddNext += x
        }
      }
    }

    subset
  }

  override def toString: String = {
    var str = ""
    for(i <- 0 until length){
      for(j <- 0 until length) str += "(" + closeness(i)(j) + ", " + nImpacted(i)(j) + ")  "
      str += "\n"
    }
    str
  }
}
