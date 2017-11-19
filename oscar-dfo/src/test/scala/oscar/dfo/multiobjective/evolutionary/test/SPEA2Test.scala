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
package oscar.dfo.multiobjective.evolutionary.test

import org.scalatest.FunSuite
import org.scalatest.Matchers
import oscar.dfo.multiobjective.evolutionary.EvolutionaryElement
import oscar.dfo.utils.MOOPoint
import oscar.dfo.utils.MOEvaluator
import oscar.dfo.utils.FeasibleRegion
import oscar.dfo.multiobjective.evolutionary.SPEA2

/**
 * @author Cyrille Dejemeppe cyrille.dejemeppe@gmail.com
 */
class SPEA2Test extends FunSuite with Matchers {
  def identity(values: Array[Double]) = Array.tabulate(values.length)(i => values(i))
  
  val evaluator = MOEvaluator(identity, Array.fill(2)(Double.MaxValue))
  val feasibleReg = FeasibleRegion()

  test("rawFitness should be 0 for non-dominated elements") {
    val dummyCoordArray = Array(Array(0.0, 2.0), Array(1.0, 1.0), Array(2.0, 0.0))
    val allPoints = Array.tabulate(dummyCoordArray.length)(i =>
      EvolutionaryElement(MOOPoint(dummyCoordArray(i), identity(dummyCoordArray(i)))))
    val spea2 = SPEA2(evaluator, 3, 3)
    val fitnessMap = spea2.rawFitness(allPoints)
    for ((point, fitVal) <- fitnessMap) {
      fitVal should be (0)
    }
  }
  
  test("rawFitness should be the sum of dominated elements by dominators") {
    val dummyCoordArray = Array(Array(1.0, 2.0), Array(2.0, 1.0),
    							Array(2.0, 3.0), Array(3.0, 3.0))
    val allPoints = Array.tabulate(dummyCoordArray.length)(i =>
      EvolutionaryElement(MOOPoint(dummyCoordArray(i), identity(dummyCoordArray(i)))))
    val spea2 = SPEA2(evaluator, 3, 3)
    val fitnessMap = spea2.rawFitness(allPoints)
    fitnessMap(allPoints(0)) should be (0)
    fitnessMap(allPoints(1)) should be (0)
    fitnessMap(allPoints(2)) should be (4)
    fitnessMap(allPoints(3)) should be (5)
  }
  
  test("densities should be correct") {
    val dummyCoordArray = Array(Array(0.0, 2.0), Array(1.0, 1.0), Array(3.0, 0.0))
    val allPoints = Array.tabulate(dummyCoordArray.length)(i =>
      EvolutionaryElement(MOOPoint(dummyCoordArray(i), identity(dummyCoordArray(i)))))
    val spea2 = SPEA2(evaluator, 3, 3)
    val densityAndDistanceMap = spea2.densityAndDistance(allPoints)
    densityAndDistanceMap(allPoints(0))._1 should be (1.0 / (math.sqrt(2.0) + 2.0))
    densityAndDistanceMap(allPoints(1))._1 should be (1.0 / (math.sqrt(2.0) + 2.0))
    densityAndDistanceMap(allPoints(2))._1 should be (1.0 / (math.sqrt(5.0) + 2.0))
  }
  
  test("distances should be correct") {
    val dummyCoordArray = Array(Array(0.0, 2.0), Array(1.0, 1.0), Array(3.0, 0.0))
    val allPoints = Array.tabulate(dummyCoordArray.length)(i =>
      EvolutionaryElement(MOOPoint(dummyCoordArray(i), identity(dummyCoordArray(i)))))
    val spea2 = SPEA2(evaluator, 3, 3)
    val densityAndDistanceMap = spea2.densityAndDistance(allPoints)
    densityAndDistanceMap(allPoints(0))._2 should be (Array(0.0, math.sqrt(2.0), math.sqrt(13.0)))
    densityAndDistanceMap(allPoints(1))._2 should be (Array(0.0, math.sqrt(2.0), math.sqrt(5.0)))
    densityAndDistanceMap(allPoints(2))._2 should be (Array(0.0, math.sqrt(5.0), math.sqrt(13.0)))
  }
  
  test("getFitnessValues should return correct fitness, distance and density values") {
    val dummyCoordArray = Array(Array(0.0, 2.0), Array(1.0, 1.0), Array(3.0, 0.0))
    val allPoints = Array.tabulate(dummyCoordArray.length)(i =>
      EvolutionaryElement(MOOPoint(dummyCoordArray(i), identity(dummyCoordArray(i)))))
    val spea2 = SPEA2(evaluator, 3, 3)
    spea2.population = allPoints.toList
    val (orderedPointList, densityAndDistanceMap) = spea2.getFitnessValues
    var minFitValue = Double.MinValue
    for ((point, fitnessValue) <- orderedPointList) {
      assert(fitnessValue >= minFitValue)
      minFitValue = fitnessValue
    }
    densityAndDistanceMap(allPoints(0))._1 should be (1.0 / (math.sqrt(2.0) + 2.0))
    densityAndDistanceMap(allPoints(1))._1 should be (1.0 / (math.sqrt(2.0) + 2.0))
    densityAndDistanceMap(allPoints(2))._1 should be (1.0 / (math.sqrt(5.0) + 2.0))
    densityAndDistanceMap(allPoints(0))._2 should be (Array(0.0, math.sqrt(2.0), math.sqrt(13.0)))
    densityAndDistanceMap(allPoints(1))._2 should be (Array(0.0, math.sqrt(2.0), math.sqrt(5.0)))
    densityAndDistanceMap(allPoints(2))._2 should be (Array(0.0, math.sqrt(5.0), math.sqrt(13.0)))
  }
  
}
