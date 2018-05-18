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
package oscar.flatzinc.cbls.support

import oscar.cbls.core.computation._
import oscar.cbls.core.constraint.{Constraint, ConstraintSystem}
import oscar.cbls.lib.constraint._
import oscar.flatzinc.model.FzDomain
import oscar.flatzinc.model.FzDomainRange
import oscar.flatzinc.model.FzDomainSet

import scala.util.Random
import oscar.flatzinc.model.FzDomainRange

import scala.collection.immutable.SortedSet
import oscar.cbls.lib.invariant.numeric.Prod2
import oscar.flatzinc.model.FzDomainRange

object Helpers {
  implicit class FznChangingIntValue(c:ChangingIntValue){

    /**
      * Intersect the domain of a ChangingIntValue with d.
      * If the current value of the ChangingIntValue is outside the resulting domain,
      * then it is reassigned to a random value in the domain.
      * @param d Domain to intersect with
      */

    def intersectDomain(d:Domain) = {
      if(c.isControlledVariable && !d.contains(c.newValue)){
        System.err.println("% Warning: reduced domain of controlled variable resulted in current value outside domain.")
      }else if(!d.contains(c.newValue)){
        val rndIdx = Random.nextInt(d.size)
        c.setValue(d.iterator.drop(rndIdx).next)
      }
      c.restrictDomain(d)
    }

    /**
      * Relax the domain of a ChangingIntValue by including the values in d.
      * Note that this is only intended to be used to restore values there were
      * removed from the domain using intersectDomain.
      * If the domain is relaxed to a include values that were not in the
      * domain when the model was closed, then things can explode.
      * @param d
      */
    def relaxDomain(d:Domain) = {
      c.overrideDomain(c.domain.union(d))
    }
  }
}

class CBLSBoolVar(model: Store, initialValue: Int, initialDomain:Domain, n: String = null)
  extends CBLSIntVar(model,initialValue, initialDomain, n){
  override def toString:String = s"CBLSBoolVar($name) := " + value
  def truthValue:Int = if (value > 0 ) 0 else 1
  def assignTruthValue(v:Int): Unit =
    if( v > 1 || v < 0)
      throw new RuntimeException("Assigning a CBLSBoolVar a truth value that is not in {0, 1}")
    else
      this := 1-v
}

//TODO: Should not extend it anymore!
//Need the store while it extends CBLSIntVar, as sometimes it is requested (e.g., to find the Model in some invariants)
class StoredCBLSIntConst(model:Store,_value:Int) extends CBLSIntVar(model,_value, DomainRange(_value, _value), _value.toString()){
  override def value:Int = _value
  override def toString:String = "IntConst("+ _value + ") := " + value
}


/*
object CBLSIntVarDom {
  def apply(model: Store, value: Int, domain: FzDomain, name: String) = {
    new CBLSIntVarDom(model, value, domain, name)
  }
}
*/

object EnsureDomain{
  val weight = CBLSIntConst(20);
  def apply(i:IntValue, d: FzDomain, c: ConstraintSystem) = {
    //System.err.println("% Using variables for in domain weights")
    //val weight = CBLSIntVar(c.model,10,1 to 1000000, "in domain weight")
    d match{
      case FzDomainRange(min, max) =>{
        if(min == max){
          c.add(EQ(i,min),weight)
        }else{
          if(i.min < min){
  //          Console.err.println("added "+i)
            c.add(GE(i, min),weight)
          }
          if(i.max > max){
  //          Console.err.println("added "+i)
            c.add(LE(i, max),weight)
          }
        }
      } 
      case FzDomainSet(vals) => {
        if(i.min < d.min){
          c.add(GE(i, d.min),weight)
//          Console.err.println("added "+i)
        }
        if(i.max > d.max){
          c.add(LE(i, d.max),weight)
//          Console.err.println("added "+i)
        }
//        Console.err.println("addedX "+i)
        c.add(BelongsToConst(i,vals) /*.nameConstraint("EnsureDomain constraint of " + c)*/)//no weight
      }
    }
  }
}

/*
  object RelaxAndEnsureDomain{
    def apply(v: CBLSIntVarDom,newMin:Int,newMax:Int,c: ConstraintSystem) = {
      v.relaxDomain(oscar.cbls.invariants.core.computation.FzDomainRange(newMin,newMax))
      EnsureDomain(v,v.dom,c)
    }
  }*/


  //TODO: Not really tested
/*
 * returns a constraint that has the same semantics but with scaled violation weight
 */
  object Weight{
    def apply(c: Constraint,w:Int):Constraint = {
        EQ(0,Prod2(c.violation,CBLSIntConst(w)))
    }
  }
  

