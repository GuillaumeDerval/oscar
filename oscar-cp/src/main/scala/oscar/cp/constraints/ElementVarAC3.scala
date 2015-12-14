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

package oscar.cp.constraints;

import oscar.cp.core.CPOutcome
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.variables.CPIntVar
import oscar.cp.core.Constraint
import oscar.cp.util.ArrayUtils
import oscar.algo.reversible.ReversibleInt
import scala.math.min
import scala.math.max
import oscar.cp.core._
import oscar.cp.core.CPOutcome._
import oscar.cp.core.CPSolver
import oscar.algo.reversible.ReversibleInt
import oscar.algo.reversible.ReversibleSparseSet
import oscar.cp.core.watcher.Watcher


/**
 * A full Arc-Consistent Element Constraint: y(x) == z based on AC3 only
 * @author Pierre Schaus pschaus@gmail.com
 * @author Renaud Hartert
 */
class ElementVarAC3(y: Array[CPIntVar], x: CPIntVar, z: CPIntVar) extends Constraint(y(0).store, "ACElementVar") {
    
  private[this] val xRange = max(0, x.min) to min(x.max, y.size)
  private[this] val zRange = (z.min max (y.map(_.min).min)) to (z.max min (y.map(_.max).max))
  
  // supporty(i) is a value that is in both dom(y(i)) and dom(z)
  // if not possible to find a supporty(i) satisfying this condition, 
  // then i can be removed from x
  
  // to update supporty
  private[this] val supporty = Array.fill(y.length)(0)
  
  // supportz(v) is an index i such that 1) i is in dom(x) and 2) v in dom(y(i))
  // if not possible to find a supportz(v) then v can be removed from z
  private[this] val supportz = Array.fill(zRange.size)(0)
  private[this] val minZ = if (zRange.isEmpty) 0 else zRange.min
  
  idempotent = true
  
  private[this] val xvalues = Array.ofDim[Int](y.length)
  private[this] val zvalues = Array.ofDim[Int](zRange.size)
  
  
  
  override def setup(l: CPPropagStrength): CPOutcome = {
    if (zRange.isEmpty) return Failure
    //println("setup:"+x.mkString(","))
    
    if (z.updateMax((y.map(_.max).max)) == Failure) return Failure
    if (z.updateMin((y.map(_.min).min)) == Failure) return Failure
    
    //if (x.updateMin(0) == Failure) return Failure
    //if (x.updateMax(y.size - 1) == Failure) return Failure

    if (adjustX() == Failure) Failure
    else {

      if (x.isBound) {
        return s.post(new Eq(y(x.min),z))
      } else {
        
        // Replaces this constraint by an Equality constraint.
        val equality = new ElementEq(y, x, z)

        x.filterWhenBind(true,CPStore.MaxPriorityL2) {
          if (s.post(equality) == Failure) Failure
          else {
            deactivate()
            Success 
          }            
        }
        //println("x::::::::::>"+x)
        x.callPropagateWhenDomainChanges(this)
        z.callPropagateWhenDomainChanges(this)
        for (i <- x.min to x.max; if x hasValue i) {
          if (z.size > 50) {
            y(i).callPropagateWhenDomainChanges(this, x.hasValue(i))
          } else {
            y(i).callPropagateWhenDomainChanges(this)
          }
        }
        propagate()
      }

    }
  }
  
  
   
  @inline private[this] def updateSupporty(i: Int, zvalues: Array[Int], m: Int): Boolean = {
    if (y(i).hasValue(supporty(i)) && z.hasValue(supporty(i))) return true
    else {
      var k = 0
      while (k < m) {
        if (y(i).hasValue(zvalues(k))) {
          supporty(i) = zvalues(k)
          
          //supportz(zvalues(k)-minZ) = i
          // for some reasons, it seems to be not such a good idea for perf
          
          return true
        }
        k += 1
      }
      return false
    }
    
  }
  
  
  
  @inline private[this] def updateSupportz(v: Int, xvalues: Array[Int], m: Int): Boolean = {
    if (x.hasValue(supportz(v-minZ)) && y(supportz(v-minZ)).hasValue(v)) return true
    else {
      var i = 0
      while (i < m) {
        //println("xval:"+xvalues(i)+" y.size:"+y.size+" x="+x+" xsize:"+x.size)
        if (y(xvalues(i)).hasValue(v)) {
          supportz(v-minZ) = xvalues(i)
          
          //supporty(xvalues(i)) = v 
          // for some reasons, it seems to be not such a good idea for perf
          
          
          return true
        }
        i += 1
      }
      return false
    }
  }  

  override def propagate(): CPOutcome = {
    //println("propagate:"+x.mkString(","))
    var mz = z.fillArray(zvalues)
    val mx = x.fillArray(xvalues)
    
    //println("mx:"+mx+" x:"+x+" y.size:"+y.size+" xvalues:"+xvalues.mkString(","))

    var i = 0

    while (i < mz) {
      if (!updateSupportz(zvalues(i),xvalues,mx)) {
        if (z.removeValue(zvalues(i)) == Failure) {
          //println("failure")
           return Failure 
        }
        else {
          zvalues(i) = zvalues(mz-1)
          mz -= 1
          i -= 1
        }
      }
      i += 1
    }    
    
    i = 0
    while (i < mx) {
      if (!updateSupporty(xvalues(i),zvalues,mz)) {
        if (x.removeValue(xvalues(i)) == Failure) {
           //println("failure")
           return Failure 
        }
      }
      i += 1
    }    
    

    
    

    Suspend
  }
  
  // Removes each value i in x that is not a valid id in y
  @inline private def adjustX(): CPOutcome = {
    if (x.updateMin(0) == Failure) Failure
    else if (x.updateMax(y.size - 1) == Failure) Failure
    else if (x.isBound) valBind(x)
    else Suspend
  }

  private class ElementEq(ys: Array[CPIntVar], x: CPIntVar, z: CPIntVar) extends Constraint(x.store, "ElementEq") {

    // Used to iterate on the domain of the variables
    private[this] val values = new Array[Int](ys.map(_.size).max max x.size max z.size)

    private[this] var y: CPIntVar = null

    final override def setup(l: CPPropagStrength): CPOutcome = {
      y = ys(x.min)
      if (propagate() == Failure) Failure
      else {
        y.callValRemoveWhenValueIsRemoved(this)
        z.callValRemoveWhenValueIsRemoved(this)
        Suspend
      }
    }

    final override def propagate(): CPOutcome = {
      var i = y.fillArray(values)
      while (i > 0) {
        i -= 1
        val value = values(i)
        if (!z.hasValue(value) && y.removeValue(value) == Failure) return Failure
      }
      i = z.fillArray(values)
      while (i > 0) {
        i -= 1
        val value = values(i)
        if (!y.hasValue(value) && z.removeValue(value) == Failure) return Failure
      }
      Suspend
    }

    // FIXME: should be idempotent (not allowed yet for L1 events)
    final override def valRemove(intVar: CPIntVar, value: Int): CPOutcome = {
      if (intVar == y) z.removeValue(value)
      else y.removeValue(value)
    }
  }

}





