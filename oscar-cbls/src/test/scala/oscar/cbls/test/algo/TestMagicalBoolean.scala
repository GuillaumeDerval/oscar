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

import oscar.cbls.algo.magicArray.{MagicBoolArray}
import scala.util.Random

/**
  * Created by  Jannou Brohée on 3/10/16.
  */
object TestMagicalBoolean extends App{

  def checkEqual(a:Array[Boolean],b:MagicBoolArray) {
    require(a.length == b.length)
    for (id <- 0 until a.length - 1) {
      require(a(id) == b(id))
    }
  }
  var reference: Array[Boolean] =  Array.ofDim(10000)
  var sut:MagicBoolArray  = MagicBoolArray(10000)
  val random = new Random()

  for(it <- 0 to 9999){
    val value = random.nextBoolean()
    reference(it)=value
    sut(it) = value
  }
  var iter= sut.indicesAtTrue
  var iterString :String ="["
  var iter2List = iter.toList
  iter= sut.indicesAtTrue
  while(iter.hasNext){
    val tmp:Int = iter.next()
    iterString+= tmp.toString
    if(iter.hasNext) iterString+=","
    require(sut(tmp))
  }
  iterString+="]"
  //check toString()
  require(sut.toString.equals(iterString))

  //check if there are elements  "true" which is not in the iterator
  for(n<- 0 until sut.length-1){
    if(!iter2List.contains(n)) require(!sut(n))
  }

  checkEqual(reference,sut)
  var reference2: Array[Boolean] =  Array[Boolean](false,false,false,false,false,false,false,false,false,false)
  var reference3: Array[Boolean] =  Array[Boolean](true,true,true,true,true,true,true,true,true,true)
  sut  = MagicBoolArray(10)
  var tim:Long = System.currentTimeMillis()
 /* for(it <-0 to Int.MaxValue-2){
    val value = random.nextBoolean()
   // if(verbose) println("set element at id :"+0+" to value :"+value)
    sut.set(0,value)
    //if(verbose) println("clearAll()")
    sut.clearAll()//==> false
    checkEqual(reference2,sut)
    //if(verbose)  println("setAll()")
    sut.setAll() //==> true
    checkEqual(reference3,sut)
  }
  require(sut.global== -1)
  println((Int.MaxValue-2)+" iterations ==>  "+(System.currentTimeMillis()-tim)+"ms")*/


}
