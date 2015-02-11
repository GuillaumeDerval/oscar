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

package oscar.flatzinc

import java.io.PrintStream

//TODO: Replace with a real solution (e.g., log4j)
class Log(val level: Int, val out:PrintStream = Console.err, val pre: String= "%"){
  def apply(s:String) = {
    if (level > 0) println(pre+" "+s)
  }
  def apply(i:Int, s:String) = {
    if(i <= level) println((pre*math.max(1,i))+" "+s)
  }
}