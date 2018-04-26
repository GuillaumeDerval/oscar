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

import oscar.cbls.algo.fun.{PiecewiseLinearFunNaive, IdentityNaive, PiecewiseLinearFun, LinearTransform}



/*
problème: on se base ici sur un index non brisé, or il a éé brisé par les mouvements opérés...)
le bon index linéaire est celui après transformation
 */


object TestUpdateableFunction extends App{
  val maxVal = 100
  var fnFun = new PiecewiseLinearFun()
  var fnNaive:PiecewiseLinearFunNaive = IdentityNaive

  def compare(): Unit ={
    for(i <- 0 to maxVal){
      //println("checking " + i + ": " + fn(i))
      if(fnFun(i) != fnNaive(i)) throw new Error("mismatch " + i +  " fn(i)" + fnNaive(i) + " naiveFn(i):" + fnFun(i))
    }
  }

  def update(fromIncluded:Int,toIncluded:Int,add:LinearTransform): Unit ={
    println("BEFORE:"+
    "\nnaive:     " + fnNaive +
    "\nfunctional:" + fnFun + "\n")

    println("UPDATE: fromIncluded:" + fromIncluded + " toIncluded:" + toIncluded + " add:" + add)

    fnNaive = fnNaive.updateBefore(fromIncluded,toIncluded,add)
    println("AFTER:"+
      "\nnaive:     " + fnNaive)

    fnFun = fnFun.updateForCompositionBefore(fromIncluded,toIncluded,add)
    println("functional:" + fnFun)

    compare()
  }

  println("init:" + fnFun)

  update(6, 20, new LinearTransform(26,true))
  println(fnNaive)
  println("f(10) = " +fnNaive(10))
  println

  update(7, 14, new LinearTransform(31,true))
  println(fnNaive)
  println("f(10) = " +fnNaive(10))
  println
  update(6, 20, new LinearTransform(-3,true))
  println(fnNaive)
  println("f(10) = " +fnNaive(10))
  println



  update(16, 19, new LinearTransform(5,true))
  println(fnNaive)
  println("f(10) = " +fnNaive(10))
  println

  update(6, 16, new LinearTransform(7,true))
  println(fnNaive)
  println("f(10) = " +fnNaive(10))
  println


  update(7, 14, new LinearTransform(-3,false))
  println(fnNaive)
  println("f(10) = " +fnNaive(10))
  println

  update(6, 20, new LinearTransform(-3,true))
  println(fnNaive)
  println("f(10) = " +fnNaive(10))
  println

  update(2, 80, new LinearTransform(-30,true))
  update(10, 94, new LinearTransform(10,false))
  update(3, 70, new LinearTransform(-7,true))

  update(6, 19, new LinearTransform(-13,true))
  println(fnNaive)
  println("f(10) = " +fnNaive(10))
  println

  update(7, 19, new LinearTransform(2,false))
  println(fnNaive)
  println("f(10) = " +fnNaive(10))
  println


  update(7, 16, new LinearTransform(4,false))
  println(fnNaive)
  println("f(10) = " +fnNaive(10))
  println


  update(15, 16, new LinearTransform(-6,false))
  println(fnNaive)
  println("f(10) = " +fnNaive(10))
  println

  update(16, 16, new LinearTransform(-11,true))
  println(fnNaive)
  println("f(10) = " +fnNaive(10))
  println
}
