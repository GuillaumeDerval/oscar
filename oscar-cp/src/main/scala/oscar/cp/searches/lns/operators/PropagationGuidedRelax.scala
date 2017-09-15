package oscar.cp.searches.lns.operators

import oscar.algo.Inconsistency
import oscar.cp.{CPIntVar, CPSolver}
import oscar.cp.searches.lns.CPIntSol

import scala.collection.mutable
import scala.util.Random

object PropagationGuidedRelax{
  var N = 0
  lazy val closeness: PropagationGuidedRelax = new PropagationGuidedRelax(N) // Closeness store used for propagation guided relax

  /**
    * Relaxes variables using propagation to guide the relaxation until the estimated size s of the neighbourhood is attained.
    * @param s The estimated size of the neighbourhood to attain.
    */
  def propagationGuidedRelax(solver: CPSolver, vars: Iterable[CPIntVar], currentSol: CPIntSol, s: Double): Unit = {
    if(N == 0) N = vars.size
    //    println("relaxing to size " + s)
    val varSeq = vars.toSeq
    val varArray = varSeq.indices.toArray //map to real index of variables
    var boundStart = varArray.length //Elements of varArray from this index are bound
    val domainSize = varSeq.map(v => v.size).toArray //initial domain size of each variable
    val prevDomainSize = domainSize.clone()
    var size = domainSize.map(i => math.log(i)).sum //Current estimation of the search space obtained
    var toFreezeNext = -1 //next var to freeze (most impacted by previous propagation)

    while (size > s) {

      val next = if (toFreezeNext == -1) varArray(Random.nextInt(boundStart)) //If no var to freeze next, selecting random var
      else toFreezeNext
      solver.add(varSeq(next) === currentSol.values(next)) //Freezing var
      // propagation should be called as var is frozen
      if (!varSeq(next).isBound) throw Inconsistency

      //Updating bounded vars and var to freeze:
      toFreezeNext = -1
      var maxImpact = 0
      val propagation = Array.fill[Double](varSeq.length){-1.0} //Propagation impact (vars already bound are ignored)
      var i = 0
      size = 0.0

      while (i < boundStart) {
        val x = varArray(i)
        val domSizeX = varSeq(x).size
        propagation(x) = (prevDomainSize(x) - domSizeX).toDouble / domainSize(x)
        prevDomainSize(x) = domSizeX
        size += math.log(varSeq(x).size)

        if (varSeq(x).isBound) {
          boundStart -= 1
          varArray(i) = varArray(boundStart)
          varArray(boundStart) = x //marking var as bound
        }

        else if (domSizeX < domainSize(x)) {
          val impact = domainSize(x) - domSizeX
          if (impact > maxImpact) {
            toFreezeNext = x
            maxImpact = impact
          } //If the var is the most impacted, selecting it as next to freeze
          i += 1
        }

        else i += 1
      }

      closeness.update(next, propagation)
    }

    //    println("relaxation done, " + (varSeq.length - boundStart) + " vars frozen")
  }

  /**
    * Relaxes variables using propagation to guide the relaxation until the estimated size s of the neighbourhood is attained.
    * @param s The estimated size of the neighbourhood to attain.
    */
  def reversedPropagationGuidedRelax(solver: CPSolver, vars: Iterable[CPIntVar], currentSol: CPIntSol, s: Double): Unit = {
    if(N == 0) N = vars.size
    //    println("relaxing to size " + s)
    val varSeq = vars.toSeq
    val varArray = varSeq.indices.toArray //map to real index of variables

    var next = Random.nextInt(varSeq.length) //Selecting randomly first var to relax
    varArray(next) = varSeq.length-1
    varArray(varSeq.length-1) = next

    var relaxStart = varArray.length-1 //Elements of varArray from this index are part of the relaxed variables
    var avgSize = varSeq(next).size
    var size = math.log(avgSize) //Current estimation of the search space obtained
    var subset = closeness.getCloseSubset(next, (math.round(s - size) / avgSize).toInt) //Subset of next

    while (size < s && relaxStart > 0) {

      if(subset.isEmpty){ //No more element in subset:
        next = Random.nextInt(relaxStart) //Selecting new random var as next
        subset = closeness.getCloseSubset(next, 10) //Retrieving subset of this var
      }
      else{
        next = subset.head
        subset -= next
      }

      relaxStart -= 1
      varArray(next) = varArray(relaxStart)
      varArray(relaxStart) = next
      val relaxed = varSeq(next).size
      avgSize = (avgSize * (varSeq.length - relaxStart - 1) + relaxed) / (varSeq.length - relaxStart)
      size += math.log(relaxed)
    }

    for(i <- (0 until relaxStart).map(x => varArray(x))){
      solver.add(varSeq(i) === currentSol.values(i)) //Freezing var
      if (!varSeq(i).isBound) throw Inconsistency
    }

    //    println("relaxation done, " + relaxStart + " vars frozen")
  }
}

/**
  * This class keeps track of the closeness relationship between variables. The closeness is defined as the average
  * volume of propagation that is involved on one variable when another is instantiated.
  */
//TODO: Manage memory better
class PropagationGuidedRelax(val size: Int){
  //If too much variables, these arrays are way to big!
  val nImpacted: Array[Array[Int]] = Array.tabulate(size, size){ (_, _) => 0}
  val closeness: Array[Array[Double]] = Array.tabulate(size, size){ (i, j) => if(i == j) 1.0 else 0.0}

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
  def getCloseness(i:Int, j:Int): Double = if(i < size && j < size) closeness(i)(j) else 0.0

  /**
    * Get the close vars of i.
    * @return a list of the close vars of i (empty if no var is close or i is not a valid index).
    */
  def getClose(i: Int): Iterable[Int] ={
    var closeSet = new mutable.ListBuffer[Int]
    if(!(i < size)) return closeSet

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
    if(initialVar >= size) return subset

    val closeToSubset = Array.fill[Double](size){0.0}
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
    for(i <- 0 until size){
      for(j <- 0 until size) str += "(" + closeness(i)(j) + ", " + nImpacted(i)(j) + ")  "
      str += "\n"
    }
    str
  }
}
