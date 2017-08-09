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
/**
 * @author Jean-Noël Monette
 */
package oscar.flatzinc.cbls

import oscar.cp.core.NoSolutionException
import oscar.flatzinc._

object FznOscarCbls extends FznOscarMain {
  checkAntlr()   
  withCheck{
    val opts = options("fzn-oscar-cbls",cbls=true)
    try {
      val solutions = new FZCBLSBuilder().solve(opts)
    }catch{
      case e: NoSolutionException => {
        System.out.println("=====UNSATISFIABLE=====")
        System.out.flush()
      }
      case e: Exception => {//catch-all clause...
        //System.err.println(e.getMessage())
        e.printStackTrace()
        System.err.println("\tPlease report the error to "+mail+" with all relevant info and data.")
      }
    }
  }
}