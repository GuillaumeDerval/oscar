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

package oscar.modeling.perf



import oscar.modeling.algebra.integer.Abs
import oscar.modeling.constraints.AllDifferent
import oscar.modeling.solvers.cp.{Branchings, CPApp}
import oscar.modeling.vars.IntVar

/*

  All intervals problem in Oscar.

  CSPLib problem number 7
  http://www.cs.st-andrews.ac.uk/~ianm/CSPLib/prob/prob007/index.html
  """
  Given the twelve standard pitch-classes (c, c , d, ...), represented by 
  numbers 0,1,...,11, find a series in which each pitch-class occurs exactly 
  once and in which the musical intervals between neighbouring notes cover 
  the full set of intervals from the minor second (1 semitone) to the major 
  seventh (11 semitones). That is, for each of the intervals, there is a 
  pair of neigbhouring pitch-classes in the series, between which this 
  interval appears. The problem of finding such a series can be easily 
  formulated as an instance of a more general arithmetic problem on Z_n, 
  the set of integer residues modulo n. Given n in N, find a vector 
  s = (s_1, ..., s_n), such that (i) s is a permutation of 
  Z_n = {0,1,...,n-1}; and (ii) the interval vector 
  v = (|s_2-s_1|, |s_3-s_2|, ... |s_n-s_{n-1}|) is a permutation of 
  Z_n-{0} = {1,2,...,n-1}. A vector v satisfying these conditions is 
  called an all-interval series of size n; the problem of finding such 
  a series is the all-interval series problem of size n. We may also be 
  interested in finding all possible series of a given size. 
  """


  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/
 
*/
object AllIntervals extends CPApp[String] with App {
    //
    // data
    //
    val n = 16

    println("n: " + n)

    //
    // variables
    //

    val x = Array.fill(n)(IntVar(0, n - 1))
    val diffs = Array.fill(n - 1)(IntVar(1, n - 1))

    //
    // constraints
    //
    var numSols = 0

    add(AllDifferent(diffs))
    add(AllDifferent(x))

    for (k <- 0 until n - 1) {
      add(diffs(k) === Abs(x(k + 1) - (x(k))))
    }

    // symmetry breaking
    add(x(0) < x(n - 1))
    add(diffs(0) < diffs(1))

    setSearch{
      Branchings.binaryStatic(x)
    }

    onSolution {
      "x:" + x.map(_.evaluate()).mkString("") +
      "  diffs:" + diffs.map(_.evaluate()).mkString("")
    } 

  val results = solve(1)

  println(results._2.head)
  println(results._1)
}
