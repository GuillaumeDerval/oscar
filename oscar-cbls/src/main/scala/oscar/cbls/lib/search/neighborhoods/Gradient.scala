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
/*
package oscar.cbls.lib.search.neighborhoods

import oscar.cbls.core.computation.CBLSIntVar
import oscar.cbls.core.search.{EasyNeighborhoodMultiLevel, First, LoopBehavior}

case class GradientDescent(vars:Array[CBLSIntVar],
                           name:String = "GradientDescent",
                           selectedVars:() => Iterable[Int] = null,
                           domain:(CBLSIntVar,Int) => Iterable[Int] = (v,i) => v.domain,
                           hotRestart:Boolean = true)
  extends EasyNeighborhoodMultiLevel[AssignMove](name) {


  //step1: interroger le gradient dans toutes les directions de selectedVars


  //step2: newton-raphson pour trouver le pas




}

*/