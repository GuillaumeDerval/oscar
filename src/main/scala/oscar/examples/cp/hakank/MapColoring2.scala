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


/**
 *
 * Map coloring in Oscar
 *
 *
 * @author Hakan Kjellerstrand hakank@gmail.com
 * http://www.hakank.org/oscar/
 *
 */
object MapColoring2 extends CPModel {


  def main(args: Array[String]) {

    val cp = CPSolver()

    // data
    var Belgium     = 0
    val Denmark     = 1
    val France      = 2
    val Germany     = 3
    val Netherlands = 4
    val Luxembourg  = 5

    val n = 6
    val num_colors = 4
      
    // variables
    val color = Array.fill(n)(CPVarInt(cp, 1 to num_colors))

    //
    // constraints
    //
    var numSols = 0

    cp.solveAll subjectTo {

      cp.add(color(France) != color(Belgium))
      cp.add(color(France) != color(Luxembourg))
      cp.add(color(France) != color(Germany))
      cp.add(color(Luxembourg) != color(Germany))
      cp.add(color(Luxembourg) != color(Belgium))
      cp.add(color(Belgium) != color(Netherlands))
      cp.add(color(Belgium) != color(Germany))
      cp.add(color(Germany) != color(Netherlands))
      cp.add(color(Germany) != color(Denmark))


      // Symmetry breaking: Belgium has color 1
      cp.add(color(Belgium) == 1)


     } exploration {
       
       cp.binaryFirstFail(color)

       println("color:" + color.mkString(" "))

       numSols += 1
       
     }
     println("\nIt was " + numSols + " solutions.\n")

     cp.printStats()
   }

}
