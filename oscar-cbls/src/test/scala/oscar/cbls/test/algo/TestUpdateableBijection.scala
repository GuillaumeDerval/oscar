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

import oscar.cbls.algo.fun.{PiecewiseLinearBijectionNaive, PiecewiseLinearFun, LinearTransform}

/*
object TestUpdateableBijection extends App{
  val maxVal = 100
  val fn = new oscar.cbls.invariants.core.algo.fun.mutable.PiecewiseLinearFun()
  var fnFun = new PiecewiseLinearFun()
  var reverse = new PiecewiseLinearBijectionNaive(fnFun)

  def compare(): Unit ={
    for(i <- 0 to maxVal){
      //println("checking " + i + ": " + fn(i))
      if(fn(i) != fnFun(i)) throw new Error("mismatch " + i +  " fn(i)" + fn(i) + " naiveFn(i):" + fnFun(i))
      if(reverse.backward(fn(i)) != i)  throw new Error("mismatch on bijection i:" + i + " fnFun(i):" + fnFun(i) + " reverse.backward(fn(i)):" + reverse.backward(fn(i)))
    }
  }

  def update(fromIncluded:Int,toIncluded:Int,add:LinearTransform): Unit ={
    println("BEFORE:"+
    "\nnaive:     " + fn +
    "\nfunctional:" + fnFun +
    "\nreverse:   " + reverse.backward + "\n")

    println("AFTER:")

    fn.update(fromIncluded,toIncluded,add)
    println("mutable:   " + fn)

    fnFun = fnFun.composeAfter(fromIncluded,toIncluded,add)
    println("functional:" + fnFun)

    reverse = new PiecewiseLinearBijectionNaive(fnFun)

    println("reverse:   " + reverse.backward + "\n")

    reverse.checkBijection()

    compare()
  }

  println("init:" + fn)

  update(6, 20, new LinearTransform(26,true))
  println(fn)
  println("f(10) = " +fn(10))
  println

  update(7, 14, new LinearTransform(31,true))
  println(fn)
  println("f(10) = " +fn(10))
  println

  update(22,40, new LinearTransform(62,true))
  println(fn)
  println("f(10) = " +fn(10))
  println

}
*/