/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v3
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 *  
 * Contributors:
 *      Hakan Kjellerstrand (hakank@gmail.com)
 ******************************************************************************/
package oscar.example.cp.hakank

import oscar.cp.modeling._
import oscar.cp.search._
import oscar.cp.core._
import scala.io.Source._
import scala.math._

/*

 Young tableaux and partition in Oscar.

  See 
  http://mathworld.wolfram.com/YoungTableau.html
  and
  http://en.wikipedia.org/wiki/Young_tableau
  """
  The partitions of 4 are
   {4}, {3,1}, {2,2}, {2,1,1}, {1,1,1,1}

  And the corresponding standard Young tableaux are:

  1.   1 2 3 4

  2.   1 2 3         1 2 4    1 3 4
       4             3        2

  3.   1 2           1 3
       3 4           2 4

  4    1 2           1 3      1 4 
       3             2        2 
       4             4        3

  5.   1
       2
       3
       4
  """  

 @author Hakan Kjellerstrand hakank@gmail.com
 http://www.hakank.org/oscar/
 
*/
object YoungTableaux extends CPModel {

  def main(args: Array[String]) {

    val cp = CPSolver()

    val n = 4


    // variables

    // grid
    val x = Array.tabulate(n)(i => 
                 Array.tabulate(n)(j =>
                       CPVarInt(cp, 1 to n+1)))

    val x_flatten = x.flatten

    // the partition structure
    val p = List.tabulate(n)(i=> CPVarInt(cp, 0 to n+1)) 

    //
    // constraints
    //
    var numSols = 0
    cp.solveAll subjectTo {

      // 1..n is used exactly once
      for(i <- 1 until n+1) {
        cp.add(gcc(x_flatten, i to i, 1, 1), Strong) 
      }
      
      cp.add(x(0)(0) == 1)

      // rows
      for(i <- 0 until n) {
        for(j <- 1 until n) {
          cp.add(x(i)(j) >= x(i)(j-1))
        }
      }

      // columns
      for(j <- 0 until n) {
        for(i <- 1 until n) {
          cp.add(x(i)(j) >= x(i-1)(j))
        }
      }

      // calculate the structure (the partition)
      for(i <- 0 until n) {
        val b = List.tabulate(n)(j=> CPVarBool(cp))
        val nn = CPVarInt(cp, n to n)
        for(j <- 0 until n) {
          cp.add((b(j)===1) === (nn >== (x(i)(j))))
        }
        cp.add(p(i) == sum(b))
      }
      
      cp.add(sum(p) == n)

      for(i <- 1 until n) {
        cp.add(p(i-1) >= p(i))
      }


    } exploration {
       
      cp.binaryFirstFail(x.flatten)

      println("\nSolution:")
      print("p: ")
      for(i <- 0 until n) {
        print(p(i) + " ")
      }
      println()
      for(i <- 0 until n) {
        var c = 0 // number of non-empty items
        for(j <- 0 until n) {
          val v = x(i)(j).getValue()
          if (v <= n) {
            print(v + " ")
            c += 1
          } else {
            print("  ")
          }
        }
        // just print non-empty lines
        if (c > 0) {
          println()
        }
      }

      numSols += 1

   }

    println("\nIt was " + numSols + " solutions.")
    cp.printStats()

  }

}
