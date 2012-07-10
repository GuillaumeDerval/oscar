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

  Set covering problem in Oscar.

  Problem from OPL.

  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/
 
*/
object CoveringOpl extends CPModel {

  // Simple decomposition of scalarProduct
  def scalarProduct(t: Array[CPVarInt], cost: Array[Int]) = 
    sum(Array.tabulate(t.length)(i=>t(i)*cost(i)))


  def main(args: Array[String]) {

    val cp = CPSolver()

    //
    // data
    //
    val num_workers = 32
    val num_tasks = 15

    // Which worker is qualified for each task.
    // Note: This is 1-based and will be made 0-based below.
    val qualified =  Array(Array( 1,  9, 19,  22,  25,  28,  31 ),
                           Array( 2, 12, 15, 19, 21, 23, 27, 29, 30, 31, 32 ),
                           Array( 3, 10, 19, 24, 26, 30, 32 ),
                           Array( 4, 21, 25, 28, 32 ),
                           Array( 5, 11, 16, 22, 23, 27, 31 ),
                           Array( 6, 20, 24, 26, 30, 32 ),
                           Array( 7, 12, 17, 25, 30, 31 ) ,
                           Array( 8, 17, 20, 22, 23  ),
                           Array( 9, 13, 14,  26, 29, 30, 31 ),
                           Array( 10, 21, 25, 31, 32 ),
                           Array( 14, 15, 18, 23, 24, 27, 30, 32 ),
                           Array( 18, 19, 22, 24, 26, 29, 31 ),
                           Array( 11, 20, 25, 28, 30, 32 ),
                           Array( 16, 19, 23, 31 ),
                           Array( 9, 18, 26, 28, 31, 32 ))

    val cost = Array(1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 3,
                     3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 6, 6, 6, 7, 8, 9)


    //
    // variables
    //

    // the digits
    val hire = Array.fill(num_workers)(CPVarInt(cp, 0 to 1))
    val total_cost = scalarProduct(hire, cost)

    //
    // constraints
    //
    var numSols = 0
    cp.minimize(total_cost) subjectTo {

      // Sum the costs for hiring the qualified workers
      // and ensure that each task is covered.
      // (Also, make 0-base.).
      qualified.foreach(task=>
                        cp.add(sum(
                                   for {
                                     c <- 0 until task.length
                                   } yield hire(task(c)-1)
                                   ) >= 1
                               )
                        )

    } exploration {
       
      // cp.binary(hire)
      // cp.binaryFirstFail(hire)
      cp.binaryMaxDegree(hire)

      println("\nSolution:")

      println("total_cost: " + total_cost)
      println("hire: " + hire.zipWithIndex.filter(_._1.getValue() == 1).map(_._2).mkString(" "))

      numSols += 1

   }

    println("\nIt was " + numSols + " solutions.")
    cp.printStats()

  }

}
