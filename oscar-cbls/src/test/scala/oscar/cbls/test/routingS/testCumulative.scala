package oscar.cbls.test.routingS

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


import oscar.cbls.algo.seq.functional.IntSequence
import oscar.cbls.invariants.core.computation.{IntValue, CBLSIntConst, CBLSSeqVar, Store}
import oscar.cbls.invariants.core.propagation.ErrorChecker
import oscar.cbls.invariants.lib.routing.ForwardCumulativeIntegerDimensionOnVehicle

/**
 * Created by rdl on 03-01-17.
 */
object TestCumulative extends App{

  val m = new Store(checker = Some(new ErrorChecker()))

  val n = 10
  val v = 2
  val startingValue:Array[IntValue] = Array(CBLSIntConst(0),CBLSIntConst(1))

  val route = new CBLSSeqVar(m,
    initialValue = IntSequence(0 until v),
    maxVal = n-1,
    n = "route")

  //op: (fromNode,toNode,contentAtFromNode)=> contentAtToNode
  def op(fromNode:Int,toNode:Int,contentAtFromNode:Int) = if(toNode%2 == 0) fromNode + toNode + contentAtFromNode else toNode

  val  (contentAtNode,contentAtEnd,lastPointOfVehicle) = ForwardCumulativeIntegerDimensionOnVehicle(route,n,v,op,startingValue,-1)

  m.close()

  route.insertAtPosition(2,1)
  route.insertAtPosition(3,2)
  route.insertAtPosition(4,3)
  route.insertAtPosition(5,4)
  route.insertAtPosition(6,5)
  route.insertAtPosition(7,6)

  m.propagate()

  println("routes:" + route.value)
  println("contentAtNode:" + contentAtNode.mkString(","))
  println("contentAtEnd:" + contentAtEnd.mkString(","))
  println("lastPointOfVehicle:" + lastPointOfVehicle.mkString(","))

  println()

  route.insertAtPosition(9,5)

  route.insertAtPosition(8,8)

  route.move(2,6,8,false)



  m.propagate()

  println("routes:" + route.value)
  println("contentAtNode:" + contentAtNode.mkString(","))
  println("contentAtEnd:" + contentAtEnd.mkString(","))
  println("lastPointOfVehicle:" + lastPointOfVehicle.mkString(","))

  /*

  route.move(1,4,5,true)

  m.propagate()

  println("routes:" + route.value)
  println("contentAtNode:" + contentAtNode.mkString(","))
  println("contentAtEnd:" + contentAtEnd.mkString(","))
  println("lastPointOfVehicle:" + lastPointOfVehicle.mkString(","))


  route.move(2,4,0,true)

  m.propagate()

  println("routes:" + route.value)
  println("contentAtNode:" + contentAtNode.mkString(","))
  println("contentAtEnd:" + contentAtEnd.mkString(","))
  println("lastPointOfVehicle:" + lastPointOfVehicle.mkString(","))

  route.move(1,2,4,false)

  m.propagate()

  println("routes:" + route.value)
  println("contentAtNode:" + contentAtNode.mkString(","))
  println("contentAtEnd:" + contentAtEnd.mkString(","))
  println("lastPointOfVehicle:" + lastPointOfVehicle.mkString(","))
*/
}
