/*******************************************************************************
  * OscaR is free software: you can redistribute it and/or modify
  * it under the terms of the GNU Lesser General Public License as published by
  * the Free Software Foundation, either version 2.1 of the License, or
  * (at your option) any later version.
  *
  * OscaR is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU Lesser General Public License  for more details.
  *
  * You should have received a copy of the GNU Lesser General Public License along with OscaR.
  * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
  ******************************************************************************/

/*******************************************************************************
  * Contributors:
  *     This code has been initially developed by CETIC www.cetic.be
  *         by Gaël Thouvenin
  ******************************************************************************/


package oscar.cbls.binPacking.solver

import oscar.cbls.binPacking.model.{Bin, BinPackingProblem, Item}
import oscar.cbls.invariants.core.computation.CBLSSetVar
import oscar.cbls.search.algo.{HotRestart, IdenticalAggregator}
import oscar.cbls.search.core.{EasyNeighborhood, JumpNeighborhood}
import oscar.cbls.search.move.{AssignMove, SwapMove}

import scala.util.Random

/**
 * Neighborhood that aims to move an item from the given set of items to one of the given bins.
 * @param problem the problem
 * @param best true if you want the best move false if you want the first acceptable move
 * @param areItemsIdentical only one if identical items will be considered for moves; this speeds up thing.
 *                          supposed to be an equivalence relation.
 *                          Identical items must be of the same size, but this does not need to be tested,
 *                          since an internal pre-filter performs this.
 *                          by default, we consider that items of the same size are identical
 * @param areBinsIdentical only one of identical bins will be considered for moves; this speeds up things.
 *                         Supposed to be an equivalence relation.
 *                         items of different sizes will be considered as different by the algorithms
 *                         through an additional mechanism, so this does not need to be tested.
 *                         by default, we consider that bins with identical free spaces are identical
 * @param from set of selectable Items ids that can be moved. If left to null, then an Item is taken from the most violated bin
 * @param to set of selectable Bins ids to move the selected Item in. If left to null, then any Bin can be selected
 * @author gael.thouvenin@student.umons.ac.be
 */
case class MoveItemSetRestrictions(problem: BinPackingProblem,
                  best:Boolean = false,
                  areItemsIdentical: (Item,Item) => Boolean = null,
                  areBinsIdentical: (Bin,Bin) => Boolean = null,
                  var from: CBLSSetVar = null,
                  var to: CBLSSetVar = null)
    extends EasyNeighborhood(best, "ItemMove") {
  private var startIndex = 0
  private val rand = new Random()
  private val hasFromBeenSet = if (from == null) false else true

  override def exploreNeighborhood(): Unit = {
    if (! hasFromBeenSet ) from = problem.bins(selectAny(problem.mostViolatedBins.value.toIndexedSeq)).items

    if (from.value.size == 0) return
    if (to != null && to.value.size == 0) return

    // We'll have to filter the items here, given an areItemsIdentical function has been provided
    val iterableIdItems:Iterable[Int] = if(areItemsIdentical != null) {
      val idItemsGroupedBySize = from.value.groupBy(problem.items(_).size).values
      idItemsGroupedBySize.map(l => IdenticalAggregator.removeIdenticals[Int](l.toList,(a:Int, b:Int) => areItemsIdentical(problem.items(a), problem.items(b)))).flatten
    }
    else from.value

    val listIdItems = iterableIdItems.toList

    //We'll have to filter the bins as well, given an areBinsIdentical function has been provided
    val iterableIdBins:Iterable[Int] = if(areBinsIdentical != null) {
      val idBinsGroupedBySpareSize = (if(to != null) to.value else 0 until problem.binCount).groupBy(x => problem.bins(x).size - problem.bins(x).content.value).values
      idBinsGroupedBySpareSize.map(l => IdenticalAggregator.removeIdenticals(l.toList, (a:Int, b:Int) => areBinsIdentical(problem.bins(a), problem.bins(b)))).flatten
    }
    else if(to != null) to.value else 0 until problem.binCount

    val restartScheme = HotRestart(0 until listIdItems.size, startIndex)

    for {
      indexIdItemToMove <- restartScheme
      idItemToMove = listIdItems(indexIdItemToMove)
    } {
      val idBinOfItemToMove = problem.items(idItemToMove).bin.value
      problem.bins(idBinOfItemToMove).items :-= idItemToMove
      for{
        idNewBin <- iterableIdBins
        newBin = problem.bins(idNewBin)
      } {

        val newObj = obj.insertValAssumeNotAlreadyIn(newBin.items,idItemToMove)

        if (moveRequested(newObj) &&
          submitFoundMove(AssignMove(problem.items(idItemToMove).bin, newBin.number, newObj))) {
          problem.bins(idBinOfItemToMove).items :+= idItemToMove
          if(!best) startIndex = idItemToMove + 1
          return
        }
      }
      problem.bins(idBinOfItemToMove).items :+= idItemToMove
    }
  }

  private def selectAny(l:IndexedSeq[Int]): Int = l(rand.nextInt(l.size))
}

/**
 * Neighborhood that aims to swap two items from the given sets.
 * @param problem the problem
 * @param best true if you want the best move false if you want the first acceptable move
 * @param areItemsIdentical only one if identical items will be considered for moves; this speeds up thing.
 *                          supposed to be an equivalence relation.
 *                          Identical items must be of the same size, but this does not need to be tested,
 *                          since an internal pre-filter performs this.
 *                          by default, we consider that items of the same size are identical
 * @param from set of selectable Items ids for the swapping. If left to null, then an Item is taken from the most violated bin
 * @param to set of selectable Items ids for the swapping. If left to null, then any Item can be selected
 * @author gael.thouvenin@student.umons.ac.be
 */
case class SwapItemsSetRestrictions(problem: BinPackingProblem,
                   best:Boolean = false,
                   areItemsIdentical: (Item,Item) => Boolean = null,
                   var from: CBLSSetVar = null,
                   var to: CBLSSetVar = null)
  extends EasyNeighborhood(best, "Swap") {

  private var startIndex = 0
  private val rand = new Random()
  private val hasFromBeenSet = if (from == null) false else true
  private val size: Int => Int = problem.items(_).size

  override def exploreNeighborhood(): Unit =  {
    if (! hasFromBeenSet ) from = problem.bins(selectAny(problem.mostViolatedBins.value.toIndexedSeq)).items

    if  (from.value.size == 0)  {println("from is empty"); return}
    if  (to != null && to.value.size == 0) {println("to is empty"); return}

    // We'll have to filter the items of the 'from' set here, given an areItemsIdentical function has been provided
    val iterableIdItems1:Iterable[Int] = if(areItemsIdentical != null) {
      val idItemsGroupedBySize = from.value.groupBy(size).values
      idItemsGroupedBySize.map(l => IdenticalAggregator.removeIdenticals[Int](l.toList,(a:Int, b:Int) => areItemsIdentical(problem.items(a), problem.items(b)))).flatten
    }
    else from.value

    val listIdItems1 = iterableIdItems1.toList

    // We'll have to filter the items of the 'to' set here, given an areItemsIdentical function has been provided
    val iterableIdItems2:Iterable[Int] = if(areItemsIdentical != null) {
      val idItemsGroupedBySize = (if(to!=null) to.value else 0 until problem.itemCount).groupBy(size).values
      idItemsGroupedBySize.map(l => IdenticalAggregator.removeIdenticals[Int](l.toList,(a:Int, b:Int) => areItemsIdentical(problem.items(a), problem.items(b)))).flatten
    }
    else if(to != null) to.value else 0 until problem.itemCount

    val restartScheme = HotRestart(0 until listIdItems1.size, startIndex)

    for {
      indexIdItemToSwap1 <- restartScheme
      idItemToSwap1 = listIdItems1(indexIdItemToSwap1)
      idBinOfItemToSwap1 = problem.items(idItemToSwap1).bin.value
      bin1 = problem.bins(idBinOfItemToSwap1)
    } {
      bin1.items :-= idItemToSwap1
      for {
        idItemToSwap2 <- iterableIdItems2
        idBinOfItemToSwap2 = problem.items(idItemToSwap2).bin.value
        bin2 = problem.bins(idBinOfItemToSwap2)
      } {
        bin2.items :+= idItemToSwap1
        bin2.items :-= idItemToSwap2
        bin1.items :+= idItemToSwap2
        val newObj = obj.value
        bin2.items :-= idItemToSwap1
        bin1.items :-= idItemToSwap2
        bin2.items :+= idItemToSwap2

        if (moveRequested(newObj) &&
          submitFoundMove(SwapMove(problem.items(idItemToSwap1).bin, problem.items(idItemToSwap2).bin, newObj))) {
          if(!best) startIndex = idBinOfItemToSwap1 + 1
          bin1.items :+= idItemToSwap1
          return
        }
      }
      problem.bins(idBinOfItemToSwap1).items :+= idItemToSwap1
    }
  }

  private def selectAny(l:IndexedSeq[Int]): Int = l(rand.nextInt(l.size))
}

/**
 * Neighborhood that aims to swap two items from the given sets without regard to the objective function.
 * @param problem the problem
 * @param from set of selectable Items ids for the swapping. If left to null, then an Item is taken from the most violated bin
 * @param to set of selectable Items ids for the swapping. If left to null, then any Item can be selected
 * @author gael.thouvenin@student.umons.ac.be
 */
case class JumpSwapItemsSetRestrictions(problem: BinPackingProblem,
                            var from: CBLSSetVar = null,
                            var to: CBLSSetVar = null)
  extends JumpNeighborhood {

  private val rand = new Random()
  private val hasFromBeenSet = if (from == null) false else true
  private var idItemToSwap1:Int = -1
  private var idItemToSwap2:Int = -1
  override def canDoIt:Boolean = {
    if (! hasFromBeenSet ) from = problem.bins(selectAny(problem.mostViolatedBins.value.toIndexedSeq)).items

    if  (from.value.size == 0)  return false
      if  (to != null && to.value.size == 0)  return false

    idItemToSwap1 = selectAny(from.value.toIndexedSeq)
    idItemToSwap2 = selectAny(if(to!=null) to.value.toIndexedSeq else 0 until problem.itemCount)
    if(idItemToSwap1 == idItemToSwap2 && from.value.size == 1 && to.value.size == 1)  return false
    return true
  }


  override def doIt(): Unit = {
    while (idItemToSwap1 == idItemToSwap2)  {
      idItemToSwap1 = selectAny(from.value.toIndexedSeq)
      idItemToSwap2 = selectAny(if(to!=null) to.value.toIndexedSeq else 0 until problem.itemCount)
    }

    val itemToSwap1 = problem.items(idItemToSwap1)
    val bin1 = problem.bins(itemToSwap1.bin.value)

    val itemToSwap2 = problem.items(idItemToSwap2)
    val bin2 = problem.bins(itemToSwap2.bin.value)

    bin1.items :-= itemToSwap1.number
    bin2.items :-= itemToSwap2.number

    bin1.items :+= itemToSwap2.number
    bin2.items :-= itemToSwap1.number

    itemToSwap1.bin :=: itemToSwap2.bin
  }

  override def shortDescription(): String = "JumpSwapItems"

  private def selectAny(l:IndexedSeq[Int]): Int = l(rand.nextInt(l.size))
}


/**
 * Neighborhood that aims to transfer every Item from one of the 'from' given bins to one of the 'destination' given bins
 * @param problem the problem
 * @param from set of selectable Bins ids to be emptied. If left to null, it is one of the most violated bin that will be emptied
 * @param destination set of selectable Bins ids to receive the transferred Items. If left to null, then any Bin can be selected
 * @author gael.thouvenin@student.umons.ac.be
 */
case class EmptyBinSetRestrictions(problem: BinPackingProblem,
                       var from: CBLSSetVar = null,
                       var destination: CBLSSetVar = null)
  extends JumpNeighborhood {

  private val rand = new Random()
  private val hasFromBeenSet = if (from == null) false else true
  private var idBinToEmpty:Int = -1
  private var idBinToTransfer:Int = -1
  override def canDoIt:Boolean = {
    val numberOfPossibilities1 = if (! hasFromBeenSet ) problem.mostViolatedBins.value.size else from.value.size

    if  (numberOfPossibilities1 == 0) return false
    if  (destination.value.size == 0) return false

    idBinToEmpty = if (! hasFromBeenSet ) problem.bins(selectAny(problem.mostViolatedBins.value.toIndexedSeq)).number
    else selectAny(from.value.toIndexedSeq)
    idBinToTransfer = selectAny(if(destination!=null) destination.value.toIndexedSeq else 0 until problem.binCount)

    if(idBinToEmpty == idBinToTransfer && numberOfPossibilities1 == 1 && destination.value.size == 1) return false
    return true
  }

  override def doIt(): Unit = {

    while (idBinToEmpty == idBinToTransfer)  {
      idBinToEmpty = if (! hasFromBeenSet ) problem.bins(selectAny(problem.mostViolatedBins.value.toIndexedSeq)).number
      else selectAny(from.value.toIndexedSeq)
      idBinToTransfer = selectAny(destination.value.toIndexedSeq)
    }
    val binToEmpty = problem.bins(idBinToEmpty)
    val binToTransfer = problem.bins(idBinToTransfer)

    for(item <- problem.bins(idBinToEmpty).items.value) {
      binToEmpty.items    :-= item
      binToTransfer.items :+= item
      problem.items(item).bin := idBinToTransfer
    }
  }

  override def shortDescription(): String = "EmptyBin"

  private def selectAny(l:IndexedSeq[Int]): Int = l(rand.nextInt(l.size))
}