package oscar.cp.constraints.tables

import oscar.algo.reversible.ReversibleSparseBitSet
import oscar.cp.core.CPOutcome._
import oscar.cp.core.delta.DeltaIntVar
import oscar.cp.core.variables.{CPIntVar, CPIntVarViewOffset}
import oscar.cp.core.{CPStore, Constraint, _}

import scala.collection.mutable.ArrayBuffer

/**
 * Implementation of the Compact Table algorithm (CT) for the table constraint.
 * @param X the variables restricted by the constraint.
 * @param table the list of tuples composing the table.
 * @author Pierre Schaus pschaus@gmail.com
 * @author Helene Verhaeghe helene.verhaeghe27@gmail.com
 */
final class TableCTNegStar(X: Array[CPIntVar], table: Array[Array[Int]], star: Int = -1, needPreprocess: Boolean = true) extends Constraint(X(0).store, "TableCTNegStar") {

  /* Set default star value */
  private[this] val _star = -1

  /* Setting idempotency & lower priority for propagate() */
  idempotent = true
  priorityL2 = CPStore.MaxPriorityL2 - 1

  /* Basic information and precomputed datas*/
  private[this] val arity = X.length

  private[this] val spans = Array.tabulate(arity)(i => X(i).max - X(i).min + 1)

  private[this] val offsets = Array.tabulate(arity)(i => X(i).min)
  private[this] val filteredTable = table.filter(t => (0 until arity).forall(i => X(i).hasValue(t(i)) || t(i) == star))

  private[this] val T0 = Array.tabulate(filteredTable.length, arity) { case (t, i) => if (filteredTable(t)(i) == star) _star else filteredTable(t)(i) - offsets(i) }

  private[this] val x = Array.tabulate(arity)(i => new CPIntVarViewOffset(X(i), -offsets(i)))

  private[this] val preprocessedTable = if (needPreprocess) preprocess else T0
  private[this] val nbTuples = preprocessedTable.length

  /* Maximum number of combination of star positions */
  private[this] val maxNbGroup = Math.pow(2, arity).toInt

  /* Dispositions of the different combinations of star positions */
  private[this] val starPositionByGroup: Array[Array[Int]] = {
    val temp = Array.fill(maxNbGroup)(new Array[Int](arity))
    def comb(times: Int, repeat: Int, index: Int): Unit = {
      val repeatTwice = 2 * repeat
      for (t <- 0 until times) {
        val offset = t * repeatTwice
        for (r <- 0 until repeat)
          temp(offset + r)(index) = 0
        for (r <- repeat until repeatTwice)
          temp(offset + r)(index) = 1
      }
      if (index < arity - 1)
        comb(times * 2, repeat / 2, index + 1)
    }
    comb(1, maxNbGroup / 2, 0)
    temp
  }
  /* Indexes of the stars for each combinations of star positions */
  private[this] val starPositionByGroupSparse = {
    val temp = new Array[Int](arity)
    Array.tabulate(maxNbGroup) { i =>
      var nb = 0
      var j = arity
      while (j > 0) {
        j -= 1
        if (starPositionByGroup(i)(j) == 1) {
          temp(nb) = j
          nb += 1
        }
      }
      val newArray = new Array[Int](nb)
      System.arraycopy(temp, 0, newArray, 0, nb)
      newArray
    }
  }
  /* Array containing the different multiplicator for each combination of star position */
  private[this] val multiplicator = Array.fill(arity, maxNbGroup)(1)

  private[this] val T: Map[Int, Array[Array[Int]]] = preprocessedTable.groupBy(tup => tup.foldLeft(0)((a, b) => a * 2 + (if (b == _star) 1 else 0)))
  private[this] val nonEmptyGroupId = {
    val buf = ArrayBuffer[Int]()
    var i = maxNbGroup
    while (i > 0) {
      i -= 1
      if (T.contains(i))
        buf += i
    }
    buf.toArray
  }

  /* Computed information about the repartition by group into the BitSets*/
  private[this] val blockOffset = Array.fill(maxNbGroup)(0)
  private[this] val setID = scala.collection.mutable.Set[Int]()
  private[this] val nbBlock = {
    var currentOffset = 0
    var i = 0
    while (i < maxNbGroup) {
      blockOffset(i) = currentOffset
      if (T.contains(i)) {
        setID ++= T(i).indices.map(_ + currentOffset * 64)
        currentOffset += (((T(i).length - 1) / 64) + 1)
      }
      i += 1
    }
    currentOffset
  }
  private[this] val realOffset = blockOffset.map(_ * 64)
  private[this] val hashMult = {
    val temp = Array.fill(nbBlock)(0)
    var i = 0
    var hash = 0
    while (hash < maxNbGroup - 1) {
      var j = blockOffset(hash + 1) - blockOffset(hash)
      while (j > 0 && i < nbBlock) {
        j -= 1
        temp(i) = hash
        i += 1
      }
      hash += 1
    }
    while (i < nbBlock) {
      temp(i) = hash
      i += 1
    }
    temp
  }

  private[this] val maxDomain = X.maxBy(_.size).size
  private[this] val domainArray = new Array[Int](maxDomain)
  private[this] var domainArraySize = 0

  private[this] val dangerousTuples = new ReversibleSparseBitSet(s, nbBlock * 64, setID)
  private[this] val variableValueAntiSupportsRM = Array.tabulate(arity)(i => new Array[dangerousTuples.BitSet](spans(i)))
  private[this] val variableValueAntiSupports = Array.tabulate(arity)(i => new Array[dangerousTuples.BitSet](spans(i)))
  private[this] val deltas: Array[DeltaIntVar] = new Array[DeltaIntVar](arity)

  /**
   * Method to compute the table without intersections
   * @return The new table, without any overlap
   */
  private def preprocess: Array[Array[Int]] = {

    val t = System.currentTimeMillis()
    val orderedVars = Array.tabulate(arity)(i => i)
    orderedVars.sortBy(x(_).size)

    val range = 0 until arity
    val ordTuple: Ordering[(Array[Int], Int)] = Ordering.by(t => t._2)
    val priorQueue = new scala.collection.mutable.PriorityQueue[(Array[Int], Int)]()(ordTuple)
    val keepSet = scala.collection.mutable.Set[Array[Int]]()

    T0.foreach { tuple =>
      var count = 1
      range.foreach(i => if (tuple(i) == _star) count *= x(i).size)
      priorQueue += ((tuple, count))
    }

    while (priorQueue.nonEmpty) {
      val elem = priorQueue.dequeue()
      if (keepSet.forall(tuple => !range.forall(i => tuple(i) == elem._1(i) || tuple(i) == star || elem._1(i) == star))) {
        keepSet += elem._1
      } else {
        if (elem._2 != 1) {
          /* discard direct if no star and overlap */
          val index = orderedVars.find(i => elem._1(i) == _star).get
          val newPriority = elem._2 / x(index).size
          for (vl <- x(index).iterator) {
            val newelem = elem._1.clone()
            newelem(index) = vl
            priorQueue.enqueue((newelem, newPriority))
          }
        }
      }

    }

    val array = keepSet.toArray
    println("preprocess time : " + (System.currentTimeMillis() - t))
    array
  }

  override def setup(l: CPPropagStrength): CPOutcome = {

    /* Success if table is empty initialy or after initial filtering */
    if (nbTuples == 0)
      return Success

    /* Retrieve the current valid tuples */
    val (dangerous, dangerousByHash) = collectDangerousTuples()

    if (dangerous.isEmpty)
      return Success

    /* Remove non dangerous tuples */
    dangerousTuples.collect(new dangerousTuples.BitSet(dangerous))
    dangerousTuples.intersectCollected()


    /* Compute AntiSupports = Compute for each for each variable/value pair the dangerous tuples */
    computeAntiSupports(dangerousByHash)

    /* Call propagate() when domains change */
    var i = 0
    while (i < arity) {
      deltas(i) = x(i).callPropagateOnChangesWithDelta(this)
      i += 1
    }

    /* Compute first multiplicators and Propagate a first time */
    updateMultiplicator()
    basicPropagate()
  }

  private[this] def showTable(): Unit = {
    table.foreach { t =>
      println(t.mkString("\t"))
    }
    println("star value:" + star)
    println("domains:" + X.mkString(","))
  }

  /**
   * Invalidates tuples by handling delta, the set of values removed from D(x) since the last call to this function.
   * @param varIndex the index of x in the array of variables.
   * @param delta the set of values removed since the last call.
   * @return the outcome i.e. Failure or Success.
   */
  @inline private def updateDelta(varIndex: Int, delta: DeltaIntVar): CPOutcome = {

    val intVar = x(varIndex)
    val varSize = intVar.size

    dangerousTuples.clearCollected()

    /* Update the value of dangerousTuples by considering D(x) or delta */

    if (varSize == 1) {

      /* The variable is assigned */
      dangerousTuples.collect(variableValueAntiSupports(varIndex)(intVar.min))
      dangerousTuples.intersectCollected()

    } else {

      if (delta.size < varSize) {

        /* Use delta to update dangerousTuples */
        domainArraySize = delta.fillArray(domainArray)
        var i = 0
        /* Collect all the removed tuples by doing or's with precomputed masks */
        while (i < domainArraySize) {
          dangerousTuples.collect(variableValueAntiSupportsRM(varIndex)(domainArray(i)))
          i += 1
        }
        /* Remove from the anti-supports all the collected tuples, no longer dangerous */
        dangerousTuples.removeCollected()

      } else {

        /* Don't use delta = reset strategy = recompute from the domain */
        domainArraySize = intVar.fillArray(domainArray)
        var i = 0
        while (i < domainArraySize) {
          dangerousTuples.collect(variableValueAntiSupports(varIndex)(domainArray(i)))
          i += 1
        }
        /* Intersect the set of dangrous tuples with the dangerous tuples collected */
        dangerousTuples.intersectCollected()

      }
    }

    /* Success if there are no more dangerous tuples */
    if (dangerousTuples.isEmpty())
      return Success

    Suspend
  }

  /**
   * Update the multiplicators for each group of tuples and each variable concerned
   */
  private[this] def updateMultiplicator() = {

    var j = nonEmptyGroupId.length
    while (j > 0) {
      j -= 1
      val hash = nonEmptyGroupId(j)
      val sparseIndex = starPositionByGroupSparse(hash)
      var i = sparseIndex.length
      var mult = 1
      while (i > 0) {
        i -= 1
        val valIndex = sparseIndex(i)
        mult *= x(valIndex).size
      }
      var varId = arity
      while (varId > 0) {
        varId -= 1
        if (starPositionByGroup(hash)(varId) == 1)
          multiplicator(varId)(hash) = mult / x(varId).size
        else
          multiplicator(varId)(hash) = mult
      }
    }

  }

  /**
   * Perform a consistency check : for each variable value pair (x,a), we check if
   * the number of dangerous tuples doesn't exceed all the possible tuples with the value.
   * Unsupported values are removed.
   * @return the outcome i.e. Failure or Success.
   */
  override def propagate(): CPOutcome = {

    updateMultiplicator()

    var varIndex = x.length
    while (varIndex > 0) {
      varIndex -= 1
      if (deltas(varIndex).size > 0) {
        if (updateDelta(varIndex, deltas(varIndex)) == Success) {
          return Success
        }
      }
    }

    basicPropagate()

  }

  /**
   * Heart of the propagation step
   * Loop on the variable-values until no changes
   * Remove the pair if the number of tuple as reach the threshold
   * @return the outcome i.e. Failure or Suspend
   */
  @inline def basicPropagate(): CPOutcome = {

    var changed = false
    var cardinalSizeInit = 1L
    do {
      changed = false
      cardinalSizeInit = x.foldLeft(1L)((i, j) => i * j.size)
      var varIndex = x.length
      while (varIndex > 0) {
        varIndex -= 1

        domainArraySize = x(varIndex).fillArray(domainArray)
        var i = 0
        var value = 0
        val cardinalSize = cardinalSizeInit / x(varIndex).size

        while (i < domainArraySize) {
          value = domainArray(i)
          val count = dangerousTuples.intersectCount(variableValueAntiSupports(varIndex)(value), hashMult, multiplicator(varIndex))
          if (count == cardinalSize) {
            if (x(varIndex).removeValue(value) == Failure) {
              return Failure
            } else {
              changed = true
              dangerousTuples.clearCollected()
              dangerousTuples.collect(variableValueAntiSupportsRM(varIndex)(value))
              dangerousTuples.removeCollected()
              if (dangerousTuples.isEmpty()) {
                return Success
              }
              cardinalSizeInit /= (x(varIndex).size + 1)
              cardinalSizeInit *= x(varIndex).size
            }
            updateMultiplicator()
          }
          i += 1
        }
      }
    } while (changed)

    Suspend
  }

  /* ----- Functions used during the setup of the constraint ----- */

  /**
   * Retrieve the dangerous tuples from the table and store their index in dangerousTuplesBuffer.
   * @return the ArrayBuffer containing the dangerous tuples.
   */
  @inline private def collectDangerousTuples(): (ArrayBuffer[Int], Array[ArrayBuffer[Int]]) = {
    val dangerousTuplesBuffer = ArrayBuffer[Int] ()
    val dangerousByHash = Array.fill(maxNbGroup)(ArrayBuffer[Int] ())

    var i = nonEmptyGroupId.length
    while (i > 0) {
      i -= 1
      val hash = nonEmptyGroupId(i)
      var tupleIndex = T(hash).length
      while (tupleIndex > 0) {
        tupleIndex -= 1
        if (isTupleDangerous(hash, tupleIndex)) {
          dangerousTuplesBuffer += tupleIndex + realOffset(hash)
          dangerousByHash(hash) += tupleIndex
        }
      }
    }

    (dangerousTuplesBuffer, dangerousByHash)
  }

  /**
   * Check if a tuple is dangerous.
   * @param tupleIndex the index of the tuple in the table.
   * @return true if the tuple is dangerous, false otherwise.
   */
  @inline private def isTupleDangerous(hash: Int, tupleIndex: Int): Boolean = {
    var varIndex = 0
    while (varIndex < arity) {
      if (!x(varIndex).hasValue(T(hash)(tupleIndex)(varIndex)) && T(hash)(tupleIndex)(varIndex) != _star) {
        return false
      }
      varIndex += 1
    }
    true
  }

  /**
   * Compute the mask for each variable value pair (x,a).
   */
  @inline private def computeAntiSupports(dangerous: Array[ArrayBuffer[Int]]): Unit = {

    val varValueSupports = Array.tabulate(x.length)(i => Array.tabulate(spans(i))(v => new ArrayBuffer[Int]()))
    val varValueSupportsStar = Array.fill(x.length)(new ArrayBuffer[Int]())

    /* Collect the supports */
    var j = nonEmptyGroupId.length
    while (j > 0) {
      j -= 1
      val hash = nonEmptyGroupId(j)
      var dangerousIndex = 0
      while (dangerousIndex < dangerous(hash).length) {
        val tupleIndex = dangerous(hash)(dangerousIndex)
        var varIndex = 0
        while (varIndex < arity) {
          val value = T(hash)(tupleIndex)(varIndex)
          if (value == _star)
            varValueSupportsStar(varIndex) += tupleIndex + realOffset(hash)
          else
            varValueSupports(varIndex)(value) += tupleIndex + realOffset(hash)
          varIndex += 1
        }
        dangerousIndex += 1
      }
    }

    /* Create the final support bitSets */
    for (varIndex <- variableValueAntiSupports.indices) {
      for (valueIndex <- variableValueAntiSupports(varIndex).indices) {
        variableValueAntiSupports(varIndex)(valueIndex) = new dangerousTuples.BitSet(varValueSupports(varIndex)(valueIndex) ++ varValueSupportsStar(varIndex))
        variableValueAntiSupportsRM(varIndex)(valueIndex) = new dangerousTuples.BitSet(varValueSupports(varIndex)(valueIndex))
      }
    }
  }
}



