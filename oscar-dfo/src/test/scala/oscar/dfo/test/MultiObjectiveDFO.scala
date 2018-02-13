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
package oscar.dfo.test

import org.scalatest.FunSuite
import org.scalatest.Matchers
import oscar.dfo.multiobjective.mogen.algos._
import oscar.util.Interval
import oscar.dfo.multiobjective.MODFOptimizer
import oscar.dfo.multiobjective.MODMS

class MultiObjectiveDFO extends FunSuite with Matchers {
  
  val domain2D = Array.tabulate(2)(i => Interval(-100.0, 100.0))
  
  val bases2D = Array.tabulate(2)(i => Array.tabulate(2)(j => if (i == j) 1.0 else 0.0))
  
  val domain2DZDT = Array.tabulate(2)(i => Interval(0.0, 1.0))
  val domain11DZDT = Array.tabulate(11)(i => Interval(0.0, 1.0))
  val domain2DZDT4 = Array(Interval(0.0, 1.0), Interval(-5.0, 5.0))
  val domain30DZDT = Array.tabulate(30)(i => Interval(0.0, 1.0))
  
  val startPoint2D = Array.fill(2){42.0}
  val startPointRand2D = Array.fill(2){scala.util.Random.nextDouble}
  
  //Returns true if p is in the archive, false otherwise.
  def inArchive(p1: Array[Double], ar: List[(Array[Double], Array[Double])]): Boolean = {
    for (j <- 0 until ar.length) {
      var equalities = 0
      for (i <- 0 until p1.length) {
        if (p1(i) == ar(j)._2(i))
          equalities += 1
      }
      if (equalities == p1.length)
        return true
    }
    return false
  }
  
  //Returns the triple (#equal values, #values lower in sol1 than in sol2, #values lower in sol2 than in sol1)
  def compareSolution(sol1: Array[Double], sol2: Array[Double]): (Int, Int, Int) = {
    var eq = 0
    var lower1 = 0
    var lower2 = 0
    for (i <- 0 until sol1.length) {
      if (sol1(i) == sol2(i))
        eq += 1
      else if(sol1(i) < sol2(i))
        lower1 += 1
      else
        lower2 += 1
    }
    (eq, lower1, lower2)
  }
  
  //Returns the ar1 without points dominated by ar2.
  def compareArchive(ar1: List[(Array[Double], Array[Double])], ar2: List[(Array[Double], Array[Double])]): List[(Array[Double], Array[Double])] = {
    var newArch = List[(Array[Double], Array[Double])]()
    for (i <- 0 until ar1.length) {
      if(!inArchive(ar1(i)._2, newArch)) {
        var j = 0
        var stop = false
        while(j < ar2.length && !stop) {
    	  val comp = compareSolution(ar1(i)._2, ar2(j)._2)
    	  if (comp._3 != 0 && comp._2 == 0)
    	    stop = true
    	  j += 1
        }
        if (!stop)
    	  newArch = newArch ::: List((ar1(i)._1, ar1(i)._2))
      }
    }
    newArch
  }

  //Returns (maxF1, maxF2, minF1, minF2)
  def getExtremeValues(arch: List[(Array[Double], Array[Double])]): (Double, Double, Double, Double) = {
    var maxF1 = Double.MinValue
    var maxF2 = Double.MinValue
    var minF1 = Double.MaxValue
    var minF2 = Double.MaxValue
    for (i <- 0 until arch.length) {
      maxF1 = math.max(maxF1, arch(i)._2(0))
      maxF2 = math.max(maxF2, arch(i)._2(1))
      minF1 = math.min(minF1, arch(i)._2(0))
      minF2 = math.min(minF2, arch(i)._2(1))
    }
    (maxF1, maxF2, minF1, minF2)
  }

  //Computes the Gamma value of an archive.
  def gammaValue(arch: List[(Array[Double], Array[Double])], maxF1: Double, maxF2: Double, minF1: Double, minF2: Double): Double = {
    var l = List((Array(Double.MinValue, Double.MinValue), Array(Double.MinValue, Double.MinValue)))
    for (i <- 0 until arch.length) {
      var stop = false
      for(j <- 0 until l.length)
      {
        if(arch(i)._2(0) < l(j)._2(0) && !stop)
        {
          l = l.take(j) ::: List((arch(i)._1, arch(i)._2)) ::: l.drop(j)
          stop = true
        }
      }
      if(!stop)
        l = l ::: List((arch(i)._1, arch(i)._2))
    }
    l = l.drop(1)
    var gamma = Double.MinValue
    for (i <- 0 until (l.length - 1)) {
      gamma = math.max(gamma, math.max(l(i+1)._2(0) - l(i)._2(0), l(i)._2(1) - l(i+1)._2(1)))
    }
    gamma = math.max(gamma, math.max(math.max(l(0)._2(0) - minF1, maxF2 - l(0)._2(1)), math.max(maxF1 - l(l.length - 1)._2(0), l(l.length - 1)._2(1) - minF2)))
    gamma
  }
  
  //Computes the purity of an archive.
  def purity(arch: List[(Array[Double], Array[Double])], archUnion: List[(Array[Double], Array[Double])]): Double = {
    var nbIn = 0
    for(i <- 0 until arch.length) {
      if(inArchive(arch(i)._2, archUnion))
        nbIn += 1
    }
    nbIn.toDouble / archUnion.length.toDouble
  }
  
  //Updates the current total archive.
  def createArchive(newArchive: List[(Array[Double], Array[Double])], formerArchive: List[(Array[Double], Array[Double])], nbObjectives: Int): List[(Array[Double], Array[Double])] = {
    var totalArchive = formerArchive
    for (i <- 0 until newArchive.length) {
      if(!inArchive(newArchive(i)._2, totalArchive)) {
        var j = 0
        var stop = false
        while(j < totalArchive.length && !stop) {
    	  val comp = compareSolution(newArchive(i)._2, totalArchive(j)._2)
    	  if (comp._1 != nbObjectives) {
    	    if (comp._1 + comp._2 == nbObjectives) {
    		  totalArchive = totalArchive.take(j) ::: totalArchive.drop(j+1)
    		  j -= 1
            }
    	    else if (comp._1 + comp._3 == nbObjectives) {
    	      stop = true
    	    }
    	  }
    	  j += 1
        }
        if (!stop) {
    	  totalArchive = totalArchive ::: List((newArchive(i)._1, newArchive(i)._2))
        }
      }
    }
    totalArchive
  }
  
  def printArchiveOverview(optimizer: MODFOptimizer, archive: List[(Array[Double], Array[Double])]) = {
	println("Number of evaluations: " + optimizer.evalCount)
    println("Size of the pareto set: " + archive.length)
  }
  
  def printArchive(optimizer: MODFOptimizer, archive: List[(Array[Double], Array[Double])]) = {
	println("Number of evaluations: " + optimizer.evalCount)
    println("Size of the pareto set: " + archive.length)
    var l = List((Array(Double.MinValue, Double.MinValue), Array(Double.MinValue, Double.MinValue)))
    for (i <- 0 until archive.length) {
      var stop = false
      for(j <- 0 until l.length)
      {
        if(archive(i)._2(0) < l(j)._2(0) && !stop)
        {
          l = l.take(j) ::: List((archive(i)._1, archive(i)._2)) ::: l.drop(j)
          stop = true
        }
      }
      if(!stop)
        l = l ::: List((archive(i)._1, archive(i)._2))
    }
    l = l.drop(1)
    for(i <- 0 until l.length)
      println("Solution#" + (i + 1) + ": " + optimizer.pointToStr(l(i)._2) + " from point " + optimizer.pointToStr(l(i)._1))
  }
  
  //f(X) = Sum(1, N) x(i)^2
  def f1(x: Array[Double]): Double = {
    var eval = 0.0
    x.foreach(element=> eval += element*element)
    eval
  }
  
  //f(X) = Sum(1, N-1)(1-x_(i))^2 + 100(x_(i+1) - x_(i)^2)^2
  def f2(x: Array[Double]): Double = {
    var eval = 0.0
    for (i <- 0 to x.length - 2)
      eval += (1 - x(i)) * (1 - x(i)) + 100 * (x(i+1) - x(i) * x(i)) * (x(i+1) - x(i) * x(i))
    eval
  }
  
  //f(x,y) = (x^2 + y - 11)^2 + (x + y^2 -7)^2
  def f3(x: Array[Double]): Double = {
    (x(0)*x(0) + x(1) - 11)*(x(0)*x(0) + x(1) - 11) + (x(0) + x(1)*x(1) -7)*(x(0) + x(1)*x(1) -7)
  }
  
  //potential energy of a two-link truss function
  def f4(x: Array[Double]): Double = {
    val F1 = 0.0
    val F2 = -3.0/10.0
    val a = scala.math.sqrt(x(0) * x(0) + x(1)* x(1)) / scala.math.sqrt(2)
    val wa = scala.math.pow(a, 4.0) / 24.0 + 1.0 / (12.0 * a * a) - 1.0 / 8.0
    val b = scala.math.sqrt((x(0) - 1.0)*(x(0) - 1.0) + x(1) * x(1))
    val wb = scala.math.pow(b, 4.0) / 24.0 + 1.0 / (12.0 * b * b) - 1.0 / 8.0;
    scala.math.sqrt(2) * wa + wb - F1*(x(0) - 1.0) - F2 * (x(1) - 1.0);
  }
  
  def dumRos(x: Array[Double]): Array[Double] = {
    Array(f1(x), f2(x))
  }
  
  def rosHim(x: Array[Double]): Array[Double] = {
    Array(f2(x), f3(x))
  }
  
  def rosHimTwo(x: Array[Double]): Array[Double] = {
    Array(f2(x), f3(x), f4(x))
  }
  
  def allButZDT(x: Array[Double]): Array[Double] = {
    Array(f1(x), f2(x), f3(x), f4(x))
  }
  
  //The first function of the ZDT1 test
  def zdt1F1(x: Array[Double]): Double = {
    x(0)
  }
  
  //The second function of the ZDT1 test
  def zdt1F2(x: Array[Double]): Double = {
    val g = 1.0 + (9.0 / (x.length - 1)) * (x.sum - x(0))
    g * (1.0 - math.sqrt(x(0) / g))
  }
  
  //The ZDT1 test
  def zdt1(x: Array[Double]): Array[Double] = {
    Array(zdt1F1(x), zdt1F2(x))
  }
  
  //The first function of the ZDT2 test
  def zdt2F1(x: Array[Double]): Double = {
    x(0)
  }
  
  //The second function of the ZDT2 test
  def zdt2F2(x: Array[Double]): Double = {
    val g = 1.0 + (9.0 / (x.length - 1)) * (x.sum - x(0))
    g * (1.0 - math.pow((x(0) / g), 2))
  }
  
  //The ZDT2 test
  def zdt2(x: Array[Double]): Array[Double] = {
    Array(zdt2F1(x), zdt2F2(x))
  }
  
  //The first function of the ZDT3 test
  def zdt3F1(x: Array[Double]): Double = {
    x(0)
  }
  
  //The second function of the ZDT3 test
  def zdt3F2(x: Array[Double]): Double = {
    val g = 1.0 + (9.0 / (x.length - 1)) * (x.sum - x(0))
    g * (1.0 - math.sqrt(x(0) / g) - (x(0) / g) * math.sin(10 * math.Pi * x(0)))
  }
  
  //The ZDT3 test
  def zdt3(x: Array[Double]): Array[Double] = {
    Array(zdt3F1(x), zdt3F2(x))
  }
  
  //The first function of the ZDT4 test
  def zdt4F1(x: Array[Double]): Double = {
    x(0)
  }
  
  //The second function of the ZDT4 test
  def zdt4F2(x: Array[Double]): Double = {
    var g = 1.0 + 10.0
    for (i <- 1 until x.length)
      g += (math.pow(x(i), 2) - 10 * math.cos(4 * math.Pi * x(i)))
    g * (1.0 - math.pow((x(0) / g), 2))
  }
  
  //The ZDT4 test
  def zdt4(x: Array[Double]): Array[Double] = {
    Array(zdt4F1(x), zdt4F2(x))
  }
  
  //The first function of the ZDT6 test
  def zdt6F1(x: Array[Double]): Double = {
    1.0 - math.exp(-4.0 * x(0)) * math.pow(math.sin(6.0 * math.Pi * x(0)), 6.0)
  }
  
  //The second function of the ZDT6 test
  def zdt6F2(x: Array[Double]): Double = {
    val g = 1.0 + 9.0 * math.pow(((x.sum - x(0))/(x.length - 1)), 0.25)
    1.0 - math.pow((zdt6F1(x) / g), 2)
  }
  
  //The ZDT6 test
  def zdt6(x: Array[Double]): Array[Double] = {
    Array(zdt6F1(x), zdt6F2(x))
  }
  

  test("findPareto => Dummy 2D & Rosenbrock 2D functions") {
    val modms = MODMS(dumRos, Array(200.0, 200.0), domain2D, bases2D)
    val modmsAns = modms.findPareto(100, math.pow(10, -2))
    printArchiveOverview(modms, modmsAns)
    println("-" * 80)
  }
  
  test("findPareto => Rosenbrock 2D & Himmelbau 2D functions") {
    val modms = MODMS(rosHim, Array(200.0, 200.0), domain2D, bases2D)
    val modmsAns = modms.findPareto(100, math.pow(10, -2))
    printArchiveOverview(modms, modmsAns)
    println("-" * 80)
  }
  
  test("findPareto => Rosenbrock 2D & Himmelbau 2D & Two-link truss functions") {
    val modms = MODMS(rosHimTwo, Array(200.0, 200.0, 200.0), domain2D, bases2D)
    val modmsAns = modms.findPareto(100, math.pow(10, -2))
    printArchiveOverview(modms, modmsAns)
  }
  
  test("findPareto => All functions but ZDT's") {
    val modms = MODMS(allButZDT, Array(200.0, 200.0, 200.0, 200.0), domain2D, bases2D)
    val modmsAns = modms.findPareto(100, math.pow(10, -2))
    printArchiveOverview(modms, modmsAns)
  }
  
  test("findPareto => findPareto => ZDT1 functions") {
    val modms = MODMS(zdt1, Array(200.0, 200.0), domain2DZDT, bases2D)
    val modmsAns = modms.findPareto(100, math.pow(10, -2))
    printArchiveOverview(modms, modmsAns)
  }
  
  test("findPareto => findPareto => ZDT2 functions") {
    val modms = MODMS(zdt2, Array(200.0, 200.0), domain2DZDT, bases2D)
    val modmsAns = modms.findPareto(100, math.pow(10, -2))
    printArchiveOverview(modms, modmsAns)
  }
  
  test("findPareto => findPareto => ZDT3 functions") {
    val modms = MODMS(zdt3, Array(200.0, 200.0), domain2DZDT, bases2D)
    val modmsAns = modms.findPareto(100, math.pow(10, -2))
    printArchiveOverview(modms, modmsAns)
  }
  
  test("findPareto => findPareto => ZDT4 functions") { 
    val modms = MODMS(zdt4, Array(200.0, 200.0), domain2DZDT4, bases2D)
    val modmsAns = modms.findPareto(100, math.pow(10, -2))
    printArchiveOverview(modms, modmsAns)
  }
  
  test("findPareto => findPareto => ZDT6 functions") {
    val modms = MODMS(zdt6, Array(200.0, 200.0), domain2DZDT, bases2D)
    val modmsAns = modms.findPareto(100, math.pow(10, -2))
    printArchiveOverview(modms, modmsAns)
  }
}
