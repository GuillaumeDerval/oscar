package oscar.cbls.test.invariants.algo

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

import oscar.cbls.algo.rb.RedBlackTreeMap

import scala.collection.immutable.SortedMap
import scala.util.Random

/**
 * Created by rdl on 24-05-16.
 */
object TestRedBlackTree extends App{

  def checkEqual(a:SortedMap[Int,Boolean],b:RedBlackTreeMap[Boolean]){
    require(a.size == b.size)
    for((key,value) <- a){
      require(b.get(key).head == value)
    }
    for((key,value) <- b.content){
      require(a.get(key).head == value)
    }

    for(i <- 0 to 1000){
      require(a.get(i) equals b.get(i))
    }
  }


  var reference:SortedMap[Int,Boolean] = SortedMap.empty
  var sut = RedBlackTreeMap.empty[Boolean]
  val random = new Random()

  for(it <- 0 to 10000){
    if(random.nextBoolean()){
      //insert something
      val key:Int = (random.nextDouble()*1000).toInt
      val value = random.nextBoolean()
      reference = reference + ((key,value))
      sut = sut.insert(key,value)
      println("step " + it + " inserting " + key)
    }else if (reference.size > 2){
      //remove something
      val key:Int = (reference.keys.toList.apply((random.nextDouble()*reference.size).toInt))
      reference = reference - (key)
      sut = sut.remove(key)
      println("step " + it + " removing " + key)
    }
    checkEqual(reference,sut)
    //create set from sorted

    checkEqual(reference,RedBlackTreeMap.makeFromSorted(reference.toList.sortBy(_._1)))
  }

}
