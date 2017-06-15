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
  * @author Leonard Debroux
  * @author Gustav Björdal
  * @author Jean-Noël Monette
  */


package oscar.flatzinc.model

import oscar.flatzinc.UnsatException


//TODO: CheckEmpty should go to the variables, as the domains are also used for normal sets that can be empty.
//TODO: differentiate between Int and Bool
//TODO: Add set variables
abstract class Variable(val id: String, val anns: Iterable[oscar.flatzinc.model.Annotation] = List.empty) {
  def isDefined: Boolean = {
    definingConstraint.isDefined//annotations.foldLeft(false)((acc,x) => x.name=="is_defined_var" || acc)
  }
  var definingConstraint: Option[Constraint] = Option.empty[Constraint]
  var cstrs:Set[Constraint] = Set.empty[Constraint]
  def addConstraint(c:Constraint) = {
    cstrs = cstrs + c
  }
  def removeConstraint(c:Constraint) = {
    cstrs = cstrs - c
    //cstrs = cstrs.filterNot(c.eq(_))//might be made more efficient if cstrs was a set.
  }
  def domainSize: Int;
  def isBound: Boolean;
}

case class BooleanVariable(i: String,
                           private var _value: Option[Boolean] = None,
                           a: Iterable[oscar.flatzinc.model.Annotation] = List.empty) extends Variable(i,a) {
  def this(s:String, dom: Domain) = this(s, {if (dom.min==dom.max) Some(dom.min==1) else None})
  def this(s:String, dom: Domain, anns: Iterable[oscar.flatzinc.model.Annotation]) =
    this(s, {if (dom.min==dom.max) Some(dom.min==1) else None},anns)
  def isTrue: Boolean = _value.getOrElse(false)
  def isFalse: Boolean = !_value.getOrElse(true)
  override def isBound: Boolean = _value.isDefined
  override def domainSize: Int = if(isBound) 1 else 2
  def bind(v: Boolean) = if(isBound && v!=_value.get) throw new UnsatException("Empty Domain"); else _value = Some(v)
  def boolValue: Boolean = _value.get
  def intValue: Int = if(_value.get) 1 else 0
  override def toString = {this.id + (if(isBound) "="+_value.get else "") /*+ (if(!anns.isEmpty) " :: " + anns.mkString(" :: ") else "" )*/}
}

case class IntegerVariable(i: String,
                           private var dom: Domain,
                           a: Iterable[oscar.flatzinc.model.Annotation] = List.empty) extends Variable(i,a) {
  def this(i: String, v: Int) = this(i,DomainRange(v,v));
  def domain = dom
  override def domainSize = dom.size
  def min = dom.min
  def max = dom.max
  def geq(v:Int) = dom.geq(v)
  def leq(v:Int) = dom.leq(v)
  def inter(d:Domain) = (dom,d) match {
    case (DomainRange(_,_),DomainRange(_,_)) => dom.inter(d)
    case (DomainSet(_),DomainRange(_,_)) => dom.inter(d)
    case (DomainSet(_),DomainSet(_)) => dom.inter(d)
    case (DomainRange(l,u),DomainSet(values)) => dom = DomainSet(values.filter(v => v>=l && v <= u)); dom.checkEmpty();
  }
  def neq(v:Int) = {
    if(v==min) geq(v+1)
    else if(v==max) leq(v-1)
    else if(v >min && v < max){
      dom match {
        case DomainSet(values) => dom = DomainSet(values - v); dom.checkEmpty();
        case DomainRange(l,u) => dom = DomainSet((l to u).toSet - v); dom.checkEmpty();
      }
    }
  }
  /*def is01: Boolean = min >= 0 && max <= 1
  def isTrue: Boolean = this.is01 && min == 1
  def isFalse: Boolean = this.is01 && max == 0 */
  override def isBound: Boolean = min == max
  def bind(v: Int) = {geq(v); leq(v);}
  def value:Int = {if(isBound) min else throw new Exception("Asking for the value of an unbound variable")}
  override def toString = {this.id + (if(isBound) "="+value else "") /*+ (if(!anns.isEmpty) " :: " +  anns.mkString(" :: ") else "" )*/}
}