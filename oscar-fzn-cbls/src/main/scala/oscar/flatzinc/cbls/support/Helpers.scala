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

import oscar.cbls.core.computation.Store
import oscar.cbls.core.constraint.ConstraintSystem
import oscar.cbls.lib.constraint.{BelongsTo, EQ, GE, LE}
import oscar.flatzinc.model.Domain
import oscar.flatzinc.model.DomainRange
import oscar.flatzinc.model.DomainSet
import oscar.cbls.core.computation.CBLSIntVar
import scala.util.Random
import oscar.flatzinc.model.DomainRange
import oscar.cbls.core.computation.IntValue
import oscar.cbls.core.computation.CBLSIntConst
import scala.collection.immutable.SortedSet
import oscar.cbls.core.computation.CBLSSetConst
import oscar.cbls.lib.invariant.numeric.Prod2
import oscar.flatzinc.model.DomainRange

/*trait IntValueDom extends IntValue{
  def inDomain(v:Int): Boolean
  def getDomain():Iterable[Int]
  def getRandValue():Int
  def domainSize: Int
}*/

/**
 * CBLSIntVarDom extends the normal CBLSIntVar with more precise Domain information.
 */
class CBLSIntVarDom(model: Store,Value: Int,  val dom: Domain, n: String = null)
  extends CBLSIntVar(model, Value, dom.min to dom.max,  n) //with IntValueDom
{
  def inDomain(v:Int): Boolean = {
    dom match {
      case DomainRange(min, max) => v >= min && v <= max
      case DomainSet(values) => values.contains(v)
    } 
  }
  def getDomain():Iterable[Int] = {
    dom match {
      case DomainRange(min, max) => min to max
      case DomainSet(values) => values
    }
  }
  def getRandValue():Int = {
    dom match {
      case DomainRange(min, max) => (min to max)(Random.nextInt(max-min+1))
      case DomainSet(values) => values.toIndexedSeq(Random.nextInt(values.size))
    }
  }
  def domainSize = dom match {
    case DomainRange(min, max) => math.max(max-min+1,0)
    case DomainSet(values) => values.size
  }
}



//TODO: Should not extend it anymore!
//Need the store while it extends CBLSIntVar, as sometimes it is requested (e.g., to find the Model in some invariants)
class CBLSIntConstDom(model:Store,_value:Int) extends CBLSIntVarDom(model,_value,DomainRange(_value,_value),_value.toString()){
  override def value:Int = _value //pour pas avoir de propagation
  override def toString:String = "IntConst("+ _value + ")"
}
/*  extends CBLSIntConst(value) with IntValueDom{
  override def inDomain(v:Int): Boolean = v==value 
  override def getDomain():Iterable[Int] = List(value)
  override def getRandValue():Int = value
  override def domainSize: Int = 1
}*/


object CBLSIntVarDom {
  def apply(model: Store,value: Int, domain: Domain,  name: String) = {
    new CBLSIntVarDom(model, value, domain, name)
  }
}

object EnsureDomain{
  val weight = CBLSIntConst(10);
  def apply(i:IntValue,d: Domain,c: ConstraintSystem) = {
    d match{
      case DomainRange(min, max) =>{
        if(i.min < min){
//          Console.err.println("added "+i)
          c.add(GE(i, min),weight)
        }
        if(i.max > max){
//          Console.err.println("added "+i)
          c.add(LE(i, max),weight)
        } 
      } 
      case DomainSet(vals) => {
        if(i.min < d.min){
          c.add(GE(i, d.min),weight)
//          Console.err.println("added "+i)
        }
        if(i.max > d.max){
          c.add(LE(i, d.max),weight)
//          Console.err.println("added "+i)
        }
//        Console.err.println("addedX "+i)
        c.add(BelongsTo(i,CBLSSetConst(SortedSet[Int]()++vals)))//no weight
      }
    }
  }
}

/*
  object RelaxAndEnsureDomain{
    def apply(v: CBLSIntVarDom,newMin:Int,newMax:Int,c: ConstraintSystem) = {
      v.relaxDomain(oscar.cbls.invariants.core.computation.DomainRange(newMin,newMax))
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
  

