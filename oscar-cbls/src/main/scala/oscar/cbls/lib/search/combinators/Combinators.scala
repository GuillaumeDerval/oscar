/**
 * *****************************************************************************
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
 * ****************************************************************************
 */
package oscar.cbls.lib.search.combinators

import java.awt.{Color, Dimension}
import javax.swing.JFrame

import oscar.cbls.algo.heap.{BinomialHeap, BinomialHeapWithMove}
import oscar.cbls._
import oscar.cbls.core._
import oscar.cbls.core.search._
import oscar.cbls.util.{Statistics, StopWatch}
import oscar.cbls.visual.FunctionGraphic.{AdjustMaxValue, ObjFunctionGraphicContainer, Zoom}

import scala.collection.immutable.SortedMap
import scala.language.implicitConversions

/**
 * @author renaud.delandtsheer@cetic.be
 */
abstract class NeighborhoodCombinator(a: Neighborhood*) extends Neighborhood {
  //this resets the internal state of the move combinators
  override def reset() {
    for (n <- a) n.reset()
  }

  override def resetStatistics(){
    for (n <- a) n.resetStatistics()
  }

  override def verbose_=(i: Int): Unit = {
    for (n <- a) n.verbose = i
    super.verbose_=(i)
  }

  override def toString: String = this.getClass.getSimpleName + "(" + a.mkString(",") + ")"

  override def collectProfilingStatistics: List[String] = a.flatMap(_.collectProfilingStatistics).toList
}

abstract class NeighborhoodCombinatorNoProfile(a: Neighborhood*) extends NeighborhoodCombinator(a:_*){
  override def collectProfilingStatistics: List[String] = List.empty
  override def resetStatistics(){}
}

