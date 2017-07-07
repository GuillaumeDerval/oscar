package oscar.cp.searches.lns.operators

import oscar.algo.Inconsistency
import oscar.cp.{CPIntVar, CPSolver}
import oscar.cp.searches.lns.CPIntSol

import scala.collection.mutable
import scala.util.Random


object RelaxationFunctions {

  /**
    * Relaxes randomly k variables.
    * @param k The number of variables to relax (must be >= 0 and < vars.size)
    */
  def randomRelax(solver: CPSolver, vars: Iterable[CPIntVar], currentSol: CPIntSol, k: Int): Unit = {
    val varSeq = vars.toSeq
    val varArray = varSeq.indices.toArray //map to real indice of variable
    var boundStart = varArray.length //Elements of varArray from this index are bound

    while(vars.count(!_.isBound) > k){
      val i = Random.nextInt(boundStart) //Selecting new var to freeze randomly
      val x = varArray(i)
      solver.add(varSeq(x) === currentSol.values(x))
      if(! varSeq(x).isBound) throw Inconsistency

      //marking var as bound:
      boundStart -= 1
      varArray(i) = varArray(boundStart)
      varArray(boundStart) = x
    }

//    println("relaxation done, " + (vars.size - k) + " vars frozen")
  }

  /**
    * Relaxes randomly k successive variables.
    * @param k The number of variables to relax (must be >= 0 and < vars.size)
    */
  def successiveRelax(solver: CPSolver, vars: Iterable[CPIntVar], currentSol: CPIntSol, k: Int): Unit = {
//    println("relaxing " + k + " variables")
    val varSeq = vars.toSeq
    var i = Random.nextInt(vars.size)-1 //start of the relaxed sequence (inclusive)
    if(i < 0) i = varSeq.length-1

    while(i > 0 && vars.count(!_.isBound) > k) {
      solver.add(varSeq(i) === currentSol.values(i))
      if (!varSeq(i).isBound) throw Inconsistency

      i -= 1
      if(i < 0) i = varSeq.length-1
    }

//    println("relaxation done, " + (varSeq.length - k) + " vars frozen")
  }

  /**
    * Relaxes variables using propagation to guide the relaxation until the estimated size s of the neighbourhood is attained.
    * @param s The estimated size of the neighbourhood to attain.
    */
  def propagationGuidedRelax(solver: CPSolver, vars: Iterable[CPIntVar], currentSol: CPIntSol, closeness:Option[ClosenessStore], s: Double): Unit = {
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

      if(closeness.isDefined) closeness.get.update(next, propagation)
    }

//    println("relaxation done, " + (varSeq.length - boundStart) + " vars frozen")
  }

  /**
    * Relaxes variables using propagation to guide the relaxation until the estimated size s of the neighbourhood is attained.
    * @param s The estimated size of the neighbourhood to attain.
    */
  def reversedPropagationGuidedRelax(solver: CPSolver, vars: Iterable[CPIntVar], currentSol: CPIntSol, closeness: ClosenessStore, s: Double): Unit = {
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

  /**
    * Relaxes variables using their values to guide the relaxation.
    * @param k The number of variables to relax (must be >= 0 and < vars.size)
    * @param scheme The way to relax variables. Possible keys:
    *               Random: selects the groups randomly
    *               MaxGroup: relaxes first the biggest groups of variables having the same value
    *               MinGroup: relaxes first the smallest groups of variables having the same values
    *               MaxVal: relaxes first the groups of variables having the biggest values
    *               MinVal: relaxes first the groups of variables having the smallest values
    */
  def valueGuidedRelax(solver: CPSolver, vars: Iterable[CPIntVar], currentSol: CPIntSol, k: Int, scheme: String): Unit = {

    //Identifying groups of variables
    val varGroups = mutable.HashMap[Int, mutable.HashSet[Int]]()
    val varSeq = vars.toSeq
    varSeq.indices.foreach(i =>{
      val value = currentSol.values(i)
      if(varGroups.contains(value)) varGroups(value) += i
      else varGroups += value -> mutable.HashSet[Int](i)
    })

    val varList: Seq[Int] = scheme match{
      case "Random" => Random.shuffle(varGroups.toSeq).flatMap(_._2)
      case "MaxGroup" => varGroups.toSeq.sortWith(_._2.size < _._2.size).flatMap(_._2)
      case "MinGroup" => varGroups.toSeq.sortWith(_._2.size > _._2.size).flatMap(_._2)
      case "MaxVal" => varGroups.toSeq.sortWith(_._1 < _._1).flatMap(_._2)
      case "MinVal" => varGroups.toSeq.sortWith(_._1 > _._1).flatMap(_._2)
    }

    var i = 0
    while(i < varSeq.length && vars.count(!_.isBound) > k){
      val x = varList(i)
      solver.add(varSeq(x) === currentSol.values(x))
      if(! varSeq(x).isBound) throw Inconsistency
      i += 1
    }
  }

  /**
    * Makes the hypothesis of a predecessor model: relaxes a sequence of k variables by using their value to find the
    * next variable in the sequence.
    * @param k The number of variables to relax (must be >= 0 and < vars.size)
    */
  def predecessorRelax(solver: CPSolver, vars: Iterable[CPIntVar], currentSol: CPIntSol, k: Int): Unit = {
    val varSeq = vars.toSeq
    val varArray = varSeq.indices.toArray //map to real indice of variable
    val mapToIdx = varArray.indices.toArray //map to indice of variable in varArray
    var boundStart = varArray.length //Elements of varArray from this index are to be relaxed
    var lastVal = -1

    while(varArray.length - boundStart < k){
      val x = if(lastVal >= 0 && lastVal < varSeq.length && !varSeq(lastVal).isBound) lastVal else varArray(Random.nextInt(boundStart))
      lastVal = currentSol.values(x)

      //marking var as to relax:
      boundStart -= 1
      val i = mapToIdx(x)
      varArray(i) = varArray(boundStart)
      mapToIdx(varArray(boundStart)) = i
      varArray(boundStart) = x
      mapToIdx(x) = boundStart
    }

    var i = 0
    while(vars.count(!_.isBound) > k) {
      val x = varArray(i)
      solver.add(varSeq(x) === currentSol.values(x))
      if (!varSeq(x).isBound) throw Inconsistency
      i += 1
    }
  }
}
