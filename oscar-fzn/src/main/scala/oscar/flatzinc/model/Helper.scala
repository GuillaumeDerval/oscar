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
package oscar.flatzinc.model

import scala.collection.mutable.{ Map => MMap}

object Helper {
  def getCName(c: Constraint): String = {
    val names = c.getClass().getName().split("\\.")
    val name = names(names.length-1)
    if(name=="reif"){
      val n2 = c.asInstanceOf[reif].c.getClass().getName().split("\\.")
      n2(n2.length-1)+"_reif"
    }else name
  }
  
  def getCstrsByName(cstrs: List[Constraint]): MMap[String,List[Constraint]] = {
    cstrs.foldLeft(MMap.empty[String,List[Constraint]])((acc,c) => { 
      val name = getCName(c)
      acc(name) = c :: acc.getOrElse(name,List.empty[Constraint]); 
      acc})
  }
}