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


package oscar.cp.core.domains

import oscar.algo.search.Outcome
import oscar.algo.search.Outcome._
import oscar.algo.reversible.ReversibleSparseSubset
import oscar.cp.core.CPStore


/**
 * @author Pierre Schaus
 */
class SetDomain(s: CPStore, min: Int, max: Int) {
  
   val values = new ReversibleSparseSubset(s,min,max)
   
   def requires(value: Int): Outcome = {
     if (!values.isPossible(value)) Failure
     else {
       values.requires(value)
       Suspend
     }
   }
   
   
   def excludes(value: Int): Outcome = {
     if (values.isRequired(value)) Failure
     else {
       if (value <= max && value >= min)
    	   values.excludes(value)
       Suspend
     }
   }
   
   def requiresAll(): Outcome = {
     values.requiresAll()
     Suspend
   }
   
   def excludesAll(): Outcome = {
     values.excludesAll()
     Suspend
   }
   
   def possibleSize = values.possibleSize
   def requiredSize = values.requiredSize
   
   def isPossible(v: Int) = values.isPossible(v)
   def isRequired(v: Int) = values.isRequired(v)
   
   def possibleSet = values.possibleSet
   def requiredSet = values.requiredSet
   
   def possibleNotRequiredValues: Iterator[Int] = values.possibleNotRequiredValues
   
   def requiredValues: Iterator[Int] = values.requiredValues
   
   def arbitraryPossibleNotRequired: Int = values.arbitraryPossibleNotRequired
  
   def randomPossibleNotRequired: Int = values.randomPossibleNotRequired  
   

   def deltaRequired(oldRequiredSize: Int): Iterator[Int] = values.deltaRequired(oldRequiredSize)
   
   def deltaPossible(oldPossibleSize: Int): Iterator[Int] = values.deltaPossible(oldPossibleSize)
	
}
