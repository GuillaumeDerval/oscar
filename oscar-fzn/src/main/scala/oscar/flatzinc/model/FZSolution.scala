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
 * @author Gustav Björdal
 * @author Jean-Noël Monette
 */
package oscar.flatzinc.model

import scala.collection.mutable.{ Map => MMap, Set => MSet }

class FZSolution {
    var output:MMap[String, (String => String) => String]= MMap.empty
    
    def addOutputVarInt(label: String, id:String) = {
      def k(c:(String => String)) = c(id)
      output += label -> k
    }
    def addOutputVarBool(label: String, id:String) = {
      def k(c:(String => String)) = if (c(id).equals("0")) "false" else "true"
      output += label -> k
    }
    def addOutputArrayVarInt(label:String,vars:Array[String],dimRanges: List[Range]) = {
      def k(c:(String => String)) = {
        "array"+dimRanges.length+"d("+dimRanges.map(r => if(r.isEmpty) "1..0" else (r.min+".."+r.max)).mkString(", ")+", ["+vars.map(c(_)).mkString(", ")+" ])"
      }
      output += label -> k
    }
    def addOutputArrayVarBool(label:String,vars:Array[String],dimRanges: List[Range]) = {
      def k(c:(String => String)) = {
        "array"+dimRanges.length+"d("+dimRanges.map(r => if(r.isEmpty) "1..0" else (r.min+".."+r.max)).mkString(", ")+", ["+vars.map(v => if(c(v).equals("0")) "false" else "true").mkString(", ")+" ])"
      }
      output += label -> k
    }
    
    def getSolution(converter:String=>String):String = {
      output.map{case (id,f) => id+" = "+f(converter)+";\n"}.mkString("")+"----------\n"
    }
    
	def handleSolution(converter:(String)=>String) = {
	  println(getSolution(converter));
	}
}