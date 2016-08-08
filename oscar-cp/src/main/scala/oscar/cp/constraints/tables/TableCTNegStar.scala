package oscar.cp.constraints.tables

import oscar.algo.reversible.{ReversibleInt, ReversibleSparseBitSet}
import oscar.cp.core.CPOutcome._
import oscar.cp.core.{CPStore, Constraint, _}
import oscar.cp.core.delta.DeltaIntVar
import oscar.cp.core.variables.{CPIntVar, CPIntVarViewOffset}

import scala.collection.mutable.ArrayBuffer

/**
 * Implementation of the Compact Table algorithm (CT) for the table constraint.
 * @param X the variables restricted by the constraint.
 * @param table the list of tuples composing the table.
 * @author Pierre Schaus pschaus@gmail.com
 * @author Helene Verhaeghe helene.verhaeghe27@gmail.com
 */
final class TableCTNegStar(X: Array[CPIntVar], table: Array[Array[Int]], star: Int = -1) extends Constraint(X(0).store, "TableCTNegStar") {

  private[this] val _star = -1

  /* Setting idempotency & lower priority for propagate() */
  idempotent = true
  priorityL2 = CPStore.MaxPriorityL2 - 1

  /* Basic information */
  private[this] val arity = X.length

  private[this] val spans = Array.tabulate(arity)(i => X(i).max - X(i).min + 1)

  private[this] val offsets = Array.tabulate(arity)(i => X(i).min)
  private[this] val filteredTable = table.filter(t => (0 until arity).forall(i => X(i).hasValue(t(i)) || t(i) == star))

  private[this] val T0 = Array.tabulate(filteredTable.length, arity) { case (t, i) => if (filteredTable(t)(i) == star) _star else filteredTable(t)(i) - offsets(i) }

  private[this] val x = Array.tabulate(arity)(i => new CPIntVarViewOffset(X(i), -offsets(i)))

  private[this] val T: Array[Array[Int]] = preprocess
  private[this] val nbTuples = T.length

  private[this] val maxDomain = X.maxBy(_.size).size
  private[this] val domainArray = new Array[Int](maxDomain)
  private[this] var domainArraySize = 0


  private[this] val dangerousTuples = new ReversibleSparseBitSet(s, nbTuples, 0 until nbTuples)
  private[this] val variableValueAntiSupportsRM = Array.tabulate(arity)(i => new Array[dangerousTuples.BitSet](spans(i)))
  private[this] val variableValueAntiSupports = Array.tabulate(arity)(i => new Array[dangerousTuples.BitSet](spans(i)))
  private[this] val starsByTuples = new Array[dangerousTuples.BitSet](arity)
  private[this] val starTupleCombinatoire = Math.pow(2, arity).toInt
  private[this] val splitStarsByTuples = new Array[dangerousTuples.BitSet](starTupleCombinatoire)
  private[this] val splitMultByTuples = Array.fill(starTupleCombinatoire)(new Array[Int](arity))
  comb(1, starTupleCombinatoire / 2, 0)
  private[this] val splitMultByTuplesSparse = {
    val temp = new Array[Int](arity)
    var nb = 0
    Array.tabulate(starTupleCombinatoire) {
      i =>
        nb = 0
        var j = arity
        while (j > 0) {
          j -= 1
          if (splitMultByTuples(i)(j) == 1) {
            temp(nb) = j
            nb += 1
          }
        }
        val newArray = new Array[Int](nb)
        System.arraycopy(temp, 0, newArray, 0, nb)
        newArray
    }
  }

  private[this] val deltas: Array[DeltaIntVar] = new Array[DeltaIntVar](arity)

  private[this] val unBoundVars = Array.tabulate(arity)(i => i)
  private[this] val unBoundVarsSize = new ReversibleInt(s, arity)

  /**
   * Recursive method to compute the array of all star position combinaison
   */
  private def comb(times: Int, repeat: Int, index: Int): Unit = {
    val repeatTwice = 2 * repeat
    for (t <- 0 until times) {
      val offset = t * repeatTwice
      for (r <- 0 until repeat)
        splitMultByTuples(offset + r)(index) = 0
      for (r <- repeat until repeatTwice)
        splitMultByTuples(offset + r)(index) = 1
    }
    if (index < arity - 1)
      comb(times * 2, repeat / 2, index + 1)
  }

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

    /* Retrieve the current valid tuples */
    val dangerous = collectDangerousTuples()

    if (dangerous.isEmpty) return Failure

    /* Remove non dangerous tuples */
    dangerousTuples.collect(new dangerousTuples.BitSet(dangerous))
    dangerousTuples.intersectCollected()

    /* Compute AntiSupports = Compute for each for each variable/value pair the dangerous tuples */
    computeAntiSupports(dangerous)

    /* Call propagate() when domains change */
    var i = 0
    while (i < arity) {
      deltas(i) = x(i).callPropagateOnChangesWithDelta(this)
      i += 1
    }

    /* Propagate a first time */
    initPropagate()
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
    var changed = false

    dangerousTuples.clearCollected()

    /* Update the value of dangerousTuples by considering D(x) or delta */
    if (varSize == 1) {
      /* The variable is assigned */
      dangerousTuples.collect(variableValueAntiSupports(varIndex)(intVar.min))
      changed = dangerousTuples.intersectCollected()
    }
    else {

      if (delta.size < varSize) {

        /* Use delta to update dangerousTuples */
        domainArraySize = delta.fillArray(domainArray)
        var i = 0
        /* Collect all the removed tuples by doing or's with precomputed masks */
        while (i < domainArraySize) {
          dangerousTuples.collect(variableValueAntiSupportsRM(varIndex)(domainArray(i)))
          i += 1
        }
        /* Remove from the dangerous supports all the collected tuples, no longer supported */
        changed = dangerousTuples.removeCollected()

      } else {

        /* Don't use delta = reset strategy = recompute from the domain */
        domainArraySize = intVar.fillArray(domainArray)
        var i = 0
        while (i < domainArraySize) {
          dangerousTuples.collect(variableValueAntiSupports(varIndex)(domainArray(i)))
          i += 1
        }
        /* Intersect the set of dangrous tuples with the valid tuples collected */
        changed = dangerousTuples.intersectCollected()

      }
    }

    /* Success if there are no more dangerous tuples */
    if (dangerousTuples.isEmpty())
      return Success

    Suspend
  }


  /**
   * Perform a consistency check : for each variable value pair (x,a), we check if
   * the number of dangerous tuples doesn't exceed all the possible tuples with the value.
   * Unsupported values are removed.
   * @return the outcome i.e. Failure or Success.
   */
  override def propagate(): CPOutcome = {

    var nChanged = 0
    var changedVarIdx = 0
    var unBoundVarsSize_ = unBoundVarsSize.value
    var j = unBoundVarsSize.value
    while (j > 0) {
      j -= 1
      val varIndex = unBoundVars(j)
      if (deltas(varIndex).size > 0) {
        nChanged += 1
        changedVarIdx = varIndex
        if (updateDelta(varIndex, deltas(varIndex)) == Success) {
          return Success
        }
      }
    }

    val cardinalSizeInit = unBoundVars.foldLeft(1)((i, j) => i * x(j).size)

    j = unBoundVarsSize_
    while (j > 0) {
      j -= 1
      val varIndex = unBoundVars(j)


      if ((nChanged > 1 || changedVarIdx != varIndex) && !x(varIndex).isBound) {
        domainArraySize = x(varIndex).fillArray(domainArray)
        var i = 0
        var value = 0
        val cardinalSize = cardinalSizeInit / x(varIndex).size

        while (i < domainArraySize) {
          value = domainArray(i)
          var count = dangerousTuples.intersectCount(splitStarsByTuples(0), variableValueAntiSupports(varIndex)(value))
          var j = 1
          while (j < starTupleCombinatoire) {
              val sparseIndex = splitMultByTuplesSparse(j)
              var mult = 1
              var vIndex = sparseIndex.length
              while (vIndex > 0) {
                vIndex -= 1
                mult *= x(sparseIndex(vIndex)).size
              }
              if (splitMultByTuples(j)(varIndex) == 1)
                mult /= x(varIndex).size
              count += dangerousTuples.intersectCount(splitStarsByTuples(j), variableValueAntiSupports(varIndex)(value)) * mult

            j += 1
          }
          if (count == cardinalSize) {
            if (x(varIndex).removeValue(value) == Failure) {
              return Failure
            } else {
              dangerousTuples.clearCollected()
              dangerousTuples.collect(variableValueAntiSupportsRM(varIndex)(value))
              dangerousTuples.removeCollected()
            }
          }
          i += 1
        }
      }
      if (x(varIndex).isBound) {
        unBoundVarsSize_ -= 1
        unBoundVars(j) = unBoundVars(unBoundVarsSize_)
        unBoundVars(unBoundVarsSize_) = varIndex
      }
    }
    unBoundVarsSize.value = unBoundVarsSize_

    Suspend
  }

  /**
   * Initial propagation
   * @return the outcome i.e. Failure or Suspend
   */
  @inline def initPropagate(): CPOutcome = {

    var unBoundVarsSize_ = unBoundVarsSize.value
    var j = unBoundVarsSize.value

    val cardinalSizeInit = unBoundVars.foldLeft(1)((i, j) => i * x(j).size)

    j = unBoundVarsSize_
    while (j > 0) {
      j -= 1
      val varIndex = unBoundVars(j)

      if (!x(varIndex).isBound) {
        domainArraySize = x(varIndex).fillArray(domainArray)
        var i = 0
        var value = 0
        val cardinalSize = cardinalSizeInit / x(varIndex).size

        while (i < domainArraySize) {
          value = domainArray(i)

          var count = dangerousTuples.intersectCount(splitStarsByTuples(0), variableValueAntiSupports(varIndex)(value))
          var j = 1
          while (j < starTupleCombinatoire) {
            var mult = 1
            var vIndex = 0
            while (vIndex < arity) {
              if (splitMultByTuples(j)(vIndex) == 1 && varIndex != vIndex)
                mult *= x(vIndex).size
              vIndex += 1
            }
            count += dangerousTuples.intersectCount(splitStarsByTuples(j), variableValueAntiSupports(varIndex)(value)) * mult
            j += 1
          }

          if (count == cardinalSize) {
            if (x(varIndex).removeValue(value) == Failure) {
              return Failure
            } else {
              dangerousTuples.clearCollected()
              dangerousTuples.collect(variableValueAntiSupportsRM(varIndex)(value))
              dangerousTuples.removeCollected()
            }
          }
          i += 1
        }
      }
      if (x(varIndex).isBound) {
        unBoundVarsSize_ -= 1
        unBoundVars(j) = unBoundVars(unBoundVarsSize_)
        unBoundVars(unBoundVarsSize_) = varIndex
      }
    }
    unBoundVarsSize.value = unBoundVarsSize_

    Suspend
  }

  /* ----- Functions used during the setup of the constraint ----- */

  /**
   * Retrieve the dangerous tuples from the table and store their index in dangerousTuplesBuffer.
   * @return the ArrayBuffer containing the dangerous tuples.
   */
  @inline private def collectDangerousTuples(): ArrayBuffer[Int] = {

    val dangerousTuplesBuffer = ArrayBuffer[Int]()

    var tupleIndex = 0
    while (tupleIndex < nbTuples) {
      if (isTupleDangerous(tupleIndex)) {
        dangerousTuplesBuffer += tupleIndex
      }
      tupleIndex += 1
    }

    dangerousTuplesBuffer
  }

  /**
   * Check if a tuple is dangerous.
   * @param tupleIndex the index of the tuple in the table.
   * @return true if the tuple is dangerous, false otherwise.
   */
  @inline private def isTupleDangerous(tupleIndex: Int): Boolean = {
    var varIndex = 0
    while (varIndex < arity) {
      if (!x(varIndex).hasValue(T(tupleIndex)(varIndex)) && T(tupleIndex)(varIndex) != _star) {
        return false
      }
      varIndex += 1
    }
    true
  }

  /**
   * Compute the mask for each variable value pair (x,a).
   */
  @inline private def computeAntiSupports(dangerous: ArrayBuffer[Int]): Unit = {

    val varValueSupports = Array.tabulate(x.length)(i => Array.tabulate(spans(i))(v => new ArrayBuffer[Int]()))
    val varValueSupportsStar = Array.fill(x.length)(new ArrayBuffer[Int]())

    /* Collect the supports */
    var dangerousIndex = 0
    while (dangerousIndex < dangerous.length) {
      val tupleIndex = dangerous(dangerousIndex)
      var varIndex = 0
      while (varIndex < arity) {
        val value = T(tupleIndex)(varIndex)
        if (value == _star)
          varValueSupportsStar(varIndex) += tupleIndex
        else
          varValueSupports(varIndex)(value) += tupleIndex
        varIndex += 1
      }
      dangerousIndex += 1
    }

    /* Create the final support bitSets */
    for (varIndex <- variableValueAntiSupports.indices) {
      starsByTuples(varIndex) = new dangerousTuples.BitSet(varValueSupportsStar(varIndex))
      for (valueIndex <- variableValueAntiSupports(varIndex).indices) {
        variableValueAntiSupports(varIndex)(valueIndex) = new dangerousTuples.BitSet(varValueSupports(varIndex)(valueIndex) ++ varValueSupportsStar(varIndex))
        variableValueAntiSupportsRM(varIndex)(valueIndex) = new dangerousTuples.BitSet(varValueSupports(varIndex)(valueIndex))
      }
    }

    /* Compute the position of star combination */
    splitStarsByTuples(0) = new dangerousTuples.BitSet(List())
    starsByTuples.foreach(t => splitStarsByTuples(0) |= t)
    ~splitStarsByTuples(0)

    var i = 1
    while (i < starTupleCombinatoire) {
      splitStarsByTuples(i) = new dangerousTuples.BitSet(List())
      ~splitStarsByTuples(i)
      var varIndex = 0
      while (varIndex < arity) {
        if (splitMultByTuples(i)(varIndex) == 1)
          splitStarsByTuples(i) &= starsByTuples(varIndex)
        else
          splitStarsByTuples(i) &~= starsByTuples(varIndex)
        varIndex += 1
      }
      i += 1
    }
  }
}
