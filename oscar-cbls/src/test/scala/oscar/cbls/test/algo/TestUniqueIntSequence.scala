package oscar.cbls.test.algo

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

import oscar.cbls.algo.seq.functional.UniqueIntSequence

/**
 * Created by rdl on 20-04-16.
 */
object TestUniqueIntSequence extends App{
  val a = UniqueIntSequence.empty()
  println(a)

  def check(seq:UniqueIntSequence,pos:Int,value:Int){
    require(seq.positionOfValue(value).head == pos,seq + ".positionOfValue(" + value + ").head:" + seq.positionOfValue(value).head + " expected:" + pos)
    require(seq.valueAtPosition(pos).head == value)
    require(seq.explorerAtValue(value).head.value == value,"seq.explorerAtValue(value).head.value:" + seq.explorerAtValue(value).head.value + " should== value:" + value)
    require(seq.explorerAtValue(value).head.position == pos)
    require(seq.explorerAtPosition(pos).head.value == value)
    require(seq.explorerAtPosition(pos).head.position == pos)
  }

  def checkSeq(seq:UniqueIntSequence,values:Int*){
    seq.check
    for((value,pos) <- values.zipWithIndex){
      check(seq,pos,value)
    }

    require(seq.iterator.toList equals values.toList, "seq.iterator.toList:" + seq.iterator.toList + "!= values.toList:" + values.toList)
    var crawlerAtEnd = seq.explorerAtPosition(seq.size-1)
    var acc:List[Int] = List.empty
    while(crawlerAtEnd match{case Some(c) => acc = c.value :: acc; crawlerAtEnd = c.prev; true case None => false}){}
    require(acc equals values.toList, "acc:" + acc)

    require(seq.regularize().iterator.toList equals values.toList, "seq.regularize.iterator.toList:" + seq.regularize().iterator.toList + " != values.toList:" + values.toList)
  }

  val b = a.insertAtPosition(5,0,fast=true) //insert first
  println(b)
  checkSeq(b,5)

  val c = b.insertAtPosition(6,1,fast=true)//insert at end
  println(c)
  checkSeq(c,5,6)

  val d = c.insertAtPosition(7,0,fast=true)//insert at head
  println(d)
  checkSeq(d,7,5,6)

  val e = d.insertAtPosition(8,2,fast=true)//insert inside
  println(e)
  checkSeq(e,7,5,8,6)

  val f = e.delete(1,fast=true)//delete inside
  println(f)
  checkSeq(f,7,8,6)

  val g = f.insertAtPosition(5,2,fast=true)//insert inside
  println(g)
  checkSeq(g,7,8,5,6)

  val h = g.insertAtPosition(10,0,fast=true)//insert inside
  println(h)
  checkSeq(h,10,7,8,5,6)

  val i = h.insertAtPosition(54,3,fast=true)//insert inside
  println(i)
  checkSeq(i,10,7,8,54,5,6)

  val j = i.delete(2,fast=true).regularize()//insert inside
  println(j)
  checkSeq(j,10,7,54,5,6)

  println("flip")
  val kk = j.moveAfter(1,2,0,true,fast=true)//flip
  println(kk)
  checkSeq(kk,10,54,7,5,6)

  val k = j.moveAfter(1,2,4,false,fast=true)//move upwards
  println(k)
  checkSeq(k,10,5,6,7,54)

  val kkk = j.moveAfter(1,2,4,true,fast=true)//move upwards with flip
  println(kkk)
  checkSeq(kkk,10,5,6,54,7)

  val l = kkk.moveAfter(0,2,3,true,fast=true)//move upwards with flip
  println(l)
  checkSeq(l,54,6,5,10,7)

  val m = kkk.moveAfter(1,4,0,true,fast=true)//flip
  println(m)
  checkSeq(m,10,7,54,6,5)

  val n = kkk.moveAfter(1,4,0,false)//nop
  println(n)
  checkSeq(n,10,5,6,54,7)


  val nd = n.moveAfter(2,4,0,false)//move downwards no flip
  println(nd)
  checkSeq(nd,10,6,54,7,5)


  val ndf = n.moveAfter(2,4,0,true)//move downwards flip
  println(ndf)
  checkSeq(ndf,10,7,54,6,5)

  val test = i.moveAfter(1,2,4,true)//move upwards with flip
  val test2 = test.moveAfter(3,4,0,true)//move upwards with flip
  println(test2)
  val test3 = test2.regularize()
  println(test3)

  require(test3 equals test2)

}
