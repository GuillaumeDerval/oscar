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

  Costas Array in Oscar.
    
  From http://mathworld.wolfram.com/CostasArray.html:
  """
  An order-n Costas array is a permutation on {1,...,n} such
  that the distances in each row of the triangular difference
  table are distinct. For example, the permutation {1,3,4,2,5}
  has triangular difference table {2,1,-2,3}, {3,-1,1}, {1,2},
  and {4}. Since each row contains no duplications, the permutation
  is therefore a Costas array.
  """
  
  Also see
  - http://en.wikipedia.org/wiki/Costas_array
  - My MiniZinc model: http://www.hakank.org/minizinc/costas_array.mzn


  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/
 
*/

object CostasArray extends CPModel {


  def main(args: Array[String]) {

    val cp = CPSolver()

    //
    // data
    //
    var n = 6

    if (args.length > 0) {
      n = args(0).toInt
    }



    //
    // variables
    //
    val costas = Array.fill(n)(CPVarInt(cp, 1 to n))
    // Matrix of differences
    val differences = Array.fill(n)(Array.fill(n)(CPVarInt(cp, -n+1 to n-1)))


    //
    // constraints
    //
    var numSols = 0

    cp.solveAll subjectTo {

      // Fix the values in the lower triangle in the
      // difference matrix to -n+1. This removes variants
      // of the difference matrix for the the same Costas array.
      for(i <- 0 until n; j <- 0 to i) {
        cp.add(differences(i)(j) == -n+1)
      }
      
      cp.add(alldifferent(costas), Strong)

      
      // hakank: All the following constraints (and comments)
      // are from Barry O'Sullivans's original MiniZinc model
      // which my own are based on:
      // http://www.hakank.org/minizinc/costas_array.mzn
 

      // "How do the positions in the Costas array relate
      //  to the elements of the distance triangle."
      for(i <- 0 until n; j <- 0 until n if i < j) {
        cp.add( differences(i)(j) == (costas(j) - costas(j-i-1)))
      }


      // "All entries in a particular row of the difference
      //  triangle must be distint."
      for(i <- 0 until n-2) {
        cp.add(alldifferent(
                            for(j <- 0 until n if j > i) yield differences(i)(j)
                            ), Strong)
      }
      
      //
      // "All the following are redundant - only here to speed up search."
      //
      
      // "We can never place a 'token' in the same row as any other."
      for(i <- 0 until n; j <- 0 until n if i < j) {
        cp.add(differences(i)(j) != 0, Strong)
        cp.add(differences(i)(j) != 0, Strong)
      }
      
      for(k <- 2 until n; l <- 2 until n if k < l) {
        cp.add(
               (differences(k-2)(l-1) + differences(k)(l)) - 
               (differences(k-1)(l-1) + differences(k-1)(l)) == 0
               )
      }
      



    } exploration {
       
      // cp.binary(costas)
      cp.binaryFirstFail(costas)
      // cp.binaryMaxDegree(costas)

      println("\nSolution:")

      println("costas:" + costas.mkString(""))
      println("differences:");
      for(i <- 0 until n) {
        for(j <- 0 until n) {
          val v = differences(i)(j).getValue()
          if (v == -n+1) {
            print("   ");
          } else {
            print("%3d".format(v))
          }
        }
        println()
      }
      println()
 

      numSols += 1

   }

    println("\nIt was " + numSols + " solutions.")
    cp.printStats()

  }

}
