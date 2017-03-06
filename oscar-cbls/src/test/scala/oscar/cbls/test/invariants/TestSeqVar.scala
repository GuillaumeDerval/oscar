package oscar.cbls.test.invariants

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

import oscar.cbls.algo.seq.functional.{IntSequence, UniqueIntSequence}
import oscar.cbls.core.computation.{IntValue, SeqValue, Store, CBLSSeqVar}
import oscar.cbls.core.propagation.ErrorChecker
import oscar.cbls.lib.invariant.seq.{PositionsOf, Content, Size}

import scala.collection.immutable.SortedSet

/**
 * Created by rdl on 18-05-16.
 */
object TestSeqVar extends App{

  val m = new Store(verbose = true,propagateOnToString = true, checker = Some(new ErrorChecker()))
  val a = new CBLSSeqVar(m,IntSequence(List(1,2,3,5)), n = "toto")

  val size1 = Size(a.createClone())
  val size2 = Size(a)
  val pos2 = PositionsOf(a, 2)
  val content = Content(a)
  m.registerForPartialPropagation(size2)
  m.close()

  println(m.stats)

  println(size2)
//  println(content)

  println("insertAtPosition(45,3)")

  a.insertAtPosition(45,3)
  require(size2.value == 5, "size2 " + size2 + " should==5 " + a.toStringNoPropagate)

  val checkpoint = a.defineCurrentValueAsCheckpoint(true)
  println("defined checkpoint " + checkpoint)
  println("insert&Move")

  a.move(1,3,4,false)
  a.insertAtPosition(12,5)
  a.remove(a.value.positionOfFirstOccurrence(2).head)
  a.move(1,3,4,true)
  a.move(1,3,4,true)

  println(size2)
  println(content)//fail ici
  println(a)
  println(pos2)

  require(5 == size2.value)

  println("\n\n")
  a.rollbackToTopCheckpoint(checkpoint)
  require(size2.value == 5, "size2 " + size2 + " should==5 " + a.toStringNoPropagate)
  require(content.value equals SortedSet(1,2,3,5,45))
  println(content)
  require(a.value equals checkpoint)
  println(pos2)
  require(size1.value == size2.value)
}

