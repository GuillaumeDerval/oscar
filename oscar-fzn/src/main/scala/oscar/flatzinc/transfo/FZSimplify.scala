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
package oscar.flatzinc.transfo

import oscar.flatzinc.model._
import oscar.flatzinc.Log
import scala.collection.mutable.{ Map => MMap, Set => MSet }
import scala.collection.mutable.Queue
import oscar.flatzinc.UnsatException

object FZSimplify{
  
  def propagate(model: FZProblem)(implicit log: Log):Unit = {
    for(c <- model.constraints){
      val unsat = isUnsatisfiable(c)
      if(unsat){
        log(c+"")
        throw new UnsatException(c.toString())
      }
      if(log.level > 0){
        val bef = c.variables.map(v => v.domainSize).fold(1)((a,b) => a * b)
        propagate(c)
        val aft = c.variables.map(v => v.domainSize).fold(1)((a,b) => a * b)
        if(bef != aft){
          log(2,"propagated "+c)
        }
      }else{
    	propagate(c)
      }
    }
  }
  
  def simplify(model: FZProblem)(implicit log: Log):Unit = {
    propagate(model)(log)
    var newcstrs = Set.empty[Constraint]
    var mod = false
    for(c <- model.constraints){
      val unsat = isUnsatisfiable(c)
      if(unsat){
        log(c+"")
        throw new UnsatException(c.toString())
      } 
      val ent = isEntailed(c)
      if(!ent){
	    val rew = rewrite(c)
	    newcstrs += rew
	    if(rew!=c){
	      log(2,c + " -> " + rew)
	      mod = true
	      c.retract()
	      rew.insert()
	    }
      }else{
        mod = true
        c.retract()
      }
    }
    if(mod){
      model.constraints.clear()
      model.constraints ++= newcstrs
    }
  }
  
  
  //Iterates rewriting (not sure this is actually needed)
  def rewrite(c: Constraint): Constraint = {
    val rew = rewriteStep(c)
    if(rew!=c) rewrite(rew)
    else c
  }
  
  def rewriteStep(c: Constraint):Constraint = {
    c match{
      case reif(bool_eq(x,y,ann),b) if x.isFalse => bool_not(y,b,ann)
      case reif(bool_eq(x,y,ann),b) if y.isFalse => bool_not(x,b,ann)
      case bool_xor(x,y,z,ann) => 
        if (x.isTrue) bool_not(y,z,ann)
        else if (y.isTrue) bool_not(x,z,ann)
        else if (x.isFalse) bool_eq(y,z,ann)
        else if (y.isFalse) bool_eq(x,z,ann)
        else if (z.isTrue) bool_not(x,y,ann)
        else if (z.isFalse) bool_eq(x,y,ann)
        else c
      case reif(c1,b) => 
        if(b.isTrue) c1
        else if(b.isFalse){
          val neg = negate(c1)
          if(neg!=null) neg else c
        }else c
      case _ => c
    }
  }
  
  def isBound(c: Constraint) = c.variables.forall(_.isBound)

  def isEntailed(c: Constraint) = if(isBound(c)){
    c match {
      case bool2int(x,y,_) => x.intValue==y.value
      case int_eq(x,y,_) => x.value ==y.value
      case int_ne(x,y,_) => x.value !=y.value
      case int_lt(x,y,_) => x.value < y.value
      case int_le(x,y,_) => x.value <= y.value
      case bool_eq(x,y,_) => x.intValue ==y.intValue
      case bool_lt(x,y,_) => x.intValue < y.intValue
      case bool_le(x,y,_) => x.intValue <= y.intValue
      case int_lin_eq(c,x,y,_) => c.zip(x).foldLeft(0)((acc,cur) => acc + cur._1.value*cur._2.value)==y.value
      case int_lin_le(c,x,y,_) => c.zip(x).foldLeft(0)((acc,cur) => acc + cur._1.value*cur._2.value)<=y.value
      case int_lin_ne(c,x,y,_) => c.zip(x).foldLeft(0)((acc,cur) => acc + cur._1.value*cur._2.value)!=y.value
      case bool_clause(x,y,_) => x.exists(_.isTrue) || y.exists(_.isFalse)
      case set_in(x,y,_) => y.contains(x.value)
      case array_bool_and(x,y,_) => (x.exists(_.isFalse) && y.isFalse) || (x.forall(_.isTrue) && y.isTrue)
      case array_bool_or(x,y,_) => (x.exists(_.isTrue) && y.isTrue) || (x.forall(_.isFalse) && y.isFalse)
      case int_max(x,y,z,_) => x.value.max(y.value) == z .value
      case int_min(x,y,z,_) => x.value.min(y.value) == z .value
      case _ => false
    }
  }else c match{
    case int_ne(x,y,_) => 
        if(x.isBound)!y.domain.contains(x.value)
        else if(y.isBound)!x.domain.contains(y.value)
        else x.max < y.min || x.min > y.max
    case int_lt(x,y,_) => x.max < y.min
    case int_le(x,y,_) => x.max <= y.min
    case set_in(x,y,_) => x.domainSize <= y.size && x.domain.toSortedSet.forall(y.contains(_))
    case all_different_int(x,_) => x.length<=1
    case int_max(a,b,c,_) if a.isBound && c.isBound => 
      (a.value==c.value && b.max <= c.value) 
    case int_max(a,b,c,_) if b.isBound && c.isBound => 
      (b.value==c.value && a.max <= c.value)
    case int_min(a,b,c,_) if a.isBound && c.isBound => 
      (a.value==c.value && b.min >= c.value) 
    case int_min(a,b,c,_) if b.isBound && c.isBound => 
      (b.value==c.value && a.min >= c.value)
    case _ => false
  }
  
  def isUnsatisfiable(c: Constraint) = //if(isBound(c)) !isEntailed(c) else 
    false
  
  def negate(c: Constraint): Constraint = { 
    c match {
      case int_eq(x,y,a) => int_ne(x,y,a)
      case int_ne(x,y,a) => int_eq(x,y,a)
      case int_lt(x,y,a) => int_le(y,x,a)
      case int_le(x,y,a) => int_lt(y,x,a)
      case _ => null
    }
  }
  
  //returns true if some domain was potentially modified
  //this will create an infinite loop actually
  def propagate(c: Constraint) = !isEntailed(c) && {
    c match {
        case int_le(x, y, _) if x.isBound => y.geq(x.value); true
        case int_le(x, y, _) if y.isBound => x.leq(y.value); true
        case int_lt(x, y, _) if x.isBound=> y.geq(x.value+1); true
        case int_lt(x, y, _) if y.isBound=> x.leq(y.value-1); true
        case bool_le(x, y, _) if x.isBound => if(x.boolValue) y.bind(true); true
        case bool_le(x, y, _) if y.isBound => if(!y.boolValue) x.bind(false); true
        case int_eq(x, y, _) if x.isBound => y.bind(x.value); true
        case int_eq(x, y, _) if y.isBound => x.bind(y.value); true
        case bool_eq(x, y, _) if x.isBound => y.bind(x.boolValue); true
        case bool_eq(x, y, _) if y.isBound => x.bind(y.boolValue); true
        case bool_not(x, y, _) if x.isBound => y.bind(!x.boolValue); true
        case bool_not(x, y, _) if y.isBound => x.bind(!y.boolValue); true
        case bool2int(x, y, _) if x.isBound => y.bind(x.intValue); true
        case bool2int(x, y, _) if y.isBound => x.bind(y.value==1); true
        case reif(c2,b) => 
          if(isEntailed(c2)){ b.bind(true); true}
          else if(isUnsatisfiable(c2)){ b.bind(false); true}
          else false
        case array_bool_and(x,y,_) if x.forall(_.isBound) => y.bind(x.forall(v => v.boolValue)); true
        case array_bool_and(x,y,_) if x.exists(_.isFalse) => y.bind(false); true
        case array_bool_or(x,y,_) if x.forall(_.isBound) => y.bind(x.exists(v => v.boolValue)); true
        case array_bool_or(x,y,_) if x.exists(_.isTrue) => y.bind(true); true
        case bool_clause(x,y,_) if x.count(!_.isBound) + y.count(!_.isBound) == 1 =>
          x.foreach(v => if(!v.isBound)v.bind(true))
          y.foreach(v => if(!v.isBound)v.bind(false))
          true
        case int_ne(x,y,_) if y.isBound => x.neq(y.value); true
        case int_ne(x,y,_) if x.isBound => y.neq(x.value); true
        case set_in(x,d,_) => x.inter(d); true
        case int_max(a,b,c,_) if a.isBound && b.isBound => c.bind(a.value.max(b.value)); true
        case int_max(a,b,c,_) if a.isBound && c.isBound => 
          	if(a.value==c.value) b.leq(c.value) 
        	else if(a.value < c.value) b.bind(c.value)
        	else throw new UnsatException(c.toString())
          	true
        case int_max(a,b,c,_) if b.isBound && c.isBound => 
          	if(b.value==c.value) a.leq(c.value) 
        	else if(b.value < c.value) a.bind(c.value)
        	else throw new UnsatException(c.toString())
          	true
        case int_min(a,b,c,_) if a.isBound && b.isBound => c.bind(a.value.min(b.value)); true
        case int_min(a,b,c,_) if a.isBound && c.isBound => 
          	if(a.value==c.value) b.geq(c.value) 
        	else if(a.value > c.value) b.bind(c.value)
        	else throw new UnsatException(c.toString()) //TOMOVE
          	true
        case int_min(a,b,c,_) if b.isBound && c.isBound => 
          	if(b.value==c.value) a.geq(c.value) 
        	else if(b.value > c.value) a.bind(c.value)
        	else throw new UnsatException(c.toString()) //TOMOVE
          	true
        case int_lin_eq(c,x,v,_) if x.length==1 && math.abs(c(0).value) == 1 => x(0).bind(v.value/c(0).value); true
        case int_lin_eq(c,x,v,_) if x.count(!_.isBound) == 1 =>{
          val (rest,one) = x.zip(c).partition(_._1.isBound);
          if(math.abs(one(0)._2.value)==1){
            val sumrest = rest.foldLeft(0)((acc,xc) => acc + xc._1.value*xc._2.value)
            one(0)._1.bind((v.value - sumrest)/one(0)._2.value);
            true
          }else false
        }
        case int_lin_eq(c,x,v,_) if x.forall(_.isBound) =>{
          val sum = x.zip(c).foldLeft(0)((acc,xc) => acc + xc._1.value*xc._2.value)
          if(sum!=v.value) throw new UnsatException(c.toString()) //TOMOVE
          true
        }
        //added for wwtpp with Mzn 2.0.2
        //TODO: avoid overflows!
        case int_lin_eq(c,x,v,_) if c.forall(_.value.abs <= 1) && x.filter(_.domainSize==Helper.FznMaxInt).length==1 =>{
          val (theone,others) = x.zip(c).partition(_._1.domainSize==Helper.FznMaxInt)
          val one = theone(0)
          val summin = others.foldLeft(0)((acc,xc) => acc + xc._2.value*(if(xc._2.value > 0){xc._1.min}else{xc._1.max}))
          val summax = others.foldLeft(0)((acc,xc) => acc + xc._2.value*(if(xc._2.value > 0){xc._1.max}else{xc._1.min}))
          if(one._2.value==1){
            one._1.geq(v.value - summax)
            one._1.leq(v.value - summin)
          }else{
            one._1.geq(summin - v.value)
            one._1.leq(summax - v.value)
          }
          true
        } 
        case int_lin_le(c,x,v,_) if x.length==1 && c(0).value == 1 => x(0).leq(v.value); true
        case int_lin_le(c,x,v,_) if x.count(!_.isBound) == 1 =>{
          val (rest,one) = x.zip(c).partition(_._1.isBound);
          if(one(0)._2.value==1){
            val sumrest = rest.foldLeft(0)((acc,xc) => acc + xc._1.value*xc._2.value)
            one(0)._1.leq((v.value - sumrest));
            true
          }else if(one(0)._2.value== -1){
            val sumrest = rest.foldLeft(0)((acc,xc) => acc + xc._1.value*xc._2.value)
            one(0)._1.geq((v.value - sumrest));
            true
          }else false
        }
        case int_lin_ne(c,x,v,_) if x.length==1 => if (v.value %c(0).value==0){x(0).neq(v.value /c(0).value)}; true
        case all_different_int(x,_) if x.length==1 => false //TOMOVE
        case count_eq(x,v,n,_) if v.isBound && n.isBound && x.forall(_.isBound) => //TOMOVE
          if(x.count(_.value == v.value)!=n.value)throw new UnsatException(c.toString()) 
          false
        case count_eq(x,v,n,_) if v.isBound && n.isBound && x.count(!_.isBound) == 1 =>
          val cnt = x.count(vv => vv.isBound && vv.value==v.value)
          if(cnt == n.value)x.foreach(vv => if (!vv.isBound) vv.neq(v.value))
          else if(cnt + 1 == n.value)x.foreach(vv => if (!vv.isBound) vv.bind(v.value))
          else throw new UnsatException(c.toString()) //TOMOVE
          true
        //added this line for ill-defined variables in Circuit. Thanks to Emil Kajgaard
        case circuit(x,_) => x.foreach(_.inter(DomainRange(1,x.length))); true
        case int_abs(a,b,_) if a.isBound => b.bind(math.abs(a.value)); true
        case int_abs(a,b,_) if b.isBound => a.inter(DomainSet(Set(b.value,-b.value))); true
        case array_int_element(x,y,z,_) if x.isBound => z.bind(y(x.value-1).value); true
        case array_int_element(x,y,z,_) if z.isBound => x.inter(DomainSet(y.zipWithIndex.filter{case (v,i) => v.value == z.value}.map{case(v,i) => i+1}.toSet)); true
        case bool_lt(x, y, _) => x.bind(false); y.bind(true); true
        //The cstrs below might need to be iterated until fixpoint... They should be removed if we use CP.
        case int_le(x, y, _ ) => y.geq(x.min); x.leq(y.max); true
        case int_lt(x, y, _ ) => y.geq(x.min+1); x.leq(y.max-1); true
        case int_eq(x, y, _ ) => y.geq(x.min); y.leq(x.max); x.geq(y.min); x.leq(y.max); true
        case _ => false
    }
  }
  
  def propagateDomainBounds(model: FZProblem)(implicit log: Log) = {
    //TODO: Also reduce the domains from GCC_closed and other such constraints. Or do it from the minizinc to flatzinc level
     //TODO: Use a proper CP solver to do this bit!
     def prop() = model.constraints.partition(c => {
      val ret = c match {
        case int_le(x, y, _) if x.isBound => y.geq(x.value); false
        case int_le(x, y, _) if y.isBound => x.leq(y.value); false
        case int_lt(x, y, _) if x.isBound=> y.geq(x.value+1); false
        case int_lt(x, y, _) if y.isBound=> x.leq(y.value-1); false
        case bool_le(x, y, _) if x.isBound => if(x.boolValue) y.bind(true); false
        case bool_le(x, y, _) if y.isBound => if(!y.boolValue) x.bind(false); false
        case int_eq(x, y, _) if x.isBound => y.bind(x.value); false
        case int_eq(x, y, _) if y.isBound => x.bind(y.value); false
        case bool_eq(x, y, _) if x.isBound => y.bind(x.boolValue); false
        case bool_eq(x, y, _) if y.isBound => x.bind(y.boolValue); false
        case bool_not(x, y, _) if x.isBound => y.bind(!x.boolValue); false
        case bool_not(x, y, _) if y.isBound => x.bind(!y.boolValue); false
        case bool2int(x, y, _) if x.isBound => y.bind(x.intValue); false
        case bool2int(x, y, _) if y.isBound => x.bind(y.value==1); false
        case reif(c2,b) => c2 match {
          case int_eq(x,y,_) if x.isBound && y.isBound => b.bind(x.value ==y.value); false
          case int_ne(x,y,_) if x.isBound && y.isBound => b.bind(x.value !=y.value); false
          case int_lt(x,y,_) if x.isBound && y.isBound => b.bind(x.value < y.value); false
          case int_le(x,y,_) if x.isBound && y.isBound => b.bind(x.value <= y.value); false
          case bool_eq(x,y,_) if x.isBound && y.isBound => b.bind(x.intValue ==y.intValue); false
          case bool_lt(x,y,_) if x.isBound && y.isBound => b.bind(x.intValue < y.intValue); false
          case bool_le(x,y,_) if x.isBound && y.isBound => b.bind(x.intValue <= y.intValue); false
          case int_lin_eq(c,x,y,_) if x.forall(_.isBound) => b.bind(c.zip(x).foldLeft(0)((acc,cur) => acc + cur._1.value*cur._2.value)==y.value); false
          case int_lin_le(c,x,y,_) if x.forall(_.isBound) => b.bind(c.zip(x).foldLeft(0)((acc,cur) => acc + cur._1.value*cur._2.value)<=y.value); false
          case int_lin_ne(c,x,y,_) if x.forall(_.isBound) => b.bind(c.zip(x).foldLeft(0)((acc,cur) => acc + cur._1.value*cur._2.value)!=y.value); false
          case bool_clause(x,y,_) if x.forall(_.isBound) && y.forall(_.isBound) => b.bind(x.exists(_.isTrue) || y.exists(_.isFalse)); false
          case set_in(x,y,_) if x.isBound => b.bind(y.contains(x.value)); false
          case _ => true;
        }
        case array_bool_and(x,y,_) if x.forall(_.isBound) => y.bind(x.forall(v => v.boolValue)); false
        case array_bool_and(x,y,_) if x.exists(_.isFalse) && y.isFalse => false
        case array_bool_or(x,y,_) if x.forall(_.isBound) => y.bind(x.exists(v => v.boolValue)); false
        case array_bool_or(x,y,_) if x.exists(_.isTrue) && y.isTrue => false
        case bool_clause(x,y,_) if x.exists(_.isTrue) || y.exists(_.isFalse) => false
        case bool_clause(x,y,_) if x.count(!_.isBound) + y.count(!_.isBound) == 1 =>
          x.foreach(v => if(!v.isBound)v.bind(true))
          y.foreach(v => if(!v.isBound)v.bind(false))
          false
        case int_ne(x,y,_) if y.isBound => x.neq(y.value); false
        case int_ne(x,y,_) if x.isBound => y.neq(x.value); false
        case set_in(x,d,_) => x.inter(d); false
        case int_max(a,b,c,_) if a.isBound && b.isBound => c.bind(a.value.max(b.value)); false
        case int_max(a,b,c,_) if a.isBound && c.isBound => 
          	if(a.value==c.value) b.leq(c.value) 
        	else if(a.value < c.value) b.bind(c.value)
        	else throw new UnsatException(c.toString())
          	false
        case int_max(a,b,c,_) if b.isBound && c.isBound => 
          	if(b.value==c.value) a.leq(c.value) 
        	else if(b.value < c.value) a.bind(c.value)
        	else throw new UnsatException(c.toString())
          	false
        case int_min(a,b,c,_) if a.isBound && b.isBound => c.bind(a.value.min(b.value)); false
        case int_min(a,b,c,_) if a.isBound && c.isBound => 
          	if(a.value==c.value) b.geq(c.value) 
        	else if(a.value > c.value) b.bind(c.value)
        	else throw new UnsatException(c.toString())
          	false
        case int_min(a,b,c,_) if b.isBound && c.isBound => 
          	if(b.value==c.value) a.geq(c.value) 
        	else if(b.value > c.value) a.bind(c.value)
        	else throw new UnsatException(c.toString())
          	false
        case int_lin_eq(c,x,v,_) if x.length==1 && math.abs(c(0).value) == 1 => x(0).bind(v.value/c(0).value); false
        case int_lin_eq(c,x,v,_) if x.count(!_.isBound) == 1 =>{
          val (rest,one) = x.zip(c).partition(_._1.isBound);
          if(math.abs(one(0)._2.value)==1){
            val sumrest = rest.foldLeft(0)((acc,xc) => acc + xc._1.value*xc._2.value)
            one(0)._1.bind((v.value - sumrest)/one(0)._2.value);
            false
          }else true
        }
        case int_lin_eq(c,x,v,_) if x.forall(_.isBound) =>{
          val sum = x.zip(c).foldLeft(0)((acc,xc) => acc + xc._1.value*xc._2.value)
          if(sum!=v.value) throw new UnsatException(c.toString())
          false
        }
        //added for wwtpp with Mzn 2.0.2
        //TODO: avoid overflows!
        case int_lin_eq(c,x,v,_) if c.forall(_.value.abs <= 1) && x.filter(_.domainSize==Helper.FznMaxInt).length==1 =>{
          val (theone,others) = x.zip(c).partition(_._1.domainSize==Helper.FznMaxInt)
          val one = theone(0)
          val summin = others.foldLeft(0)((acc,xc) => acc + xc._2.value*(if(xc._2.value > 0){xc._1.min}else{xc._1.max}))
          val summax = others.foldLeft(0)((acc,xc) => acc + xc._2.value*(if(xc._2.value > 0){xc._1.max}else{xc._1.min}))
          if(one._2.value==1){
            one._1.geq(v.value - summax)
            one._1.leq(v.value - summin)
          }else{
            one._1.geq(summin - v.value)
            one._1.leq(summax - v.value)
          }
          true
        } 
        case int_lin_le(c,x,v,_) if x.length==1 && c(0).value == 1 => x(0).leq(v.value); false
        case int_lin_le(c,x,v,_) if x.count(!_.isBound) == 1 =>{
          val (rest,one) = x.zip(c).partition(_._1.isBound);
          if(one(0)._2.value==1){
            val sumrest = rest.foldLeft(0)((acc,xc) => acc + xc._1.value*xc._2.value)
            one(0)._1.leq((v.value - sumrest));
            false
          }else if(one(0)._2.value== -1){
            val sumrest = rest.foldLeft(0)((acc,xc) => acc + xc._1.value*xc._2.value)
            one(0)._1.geq((v.value - sumrest));
            false
          }else true
        }
        case int_lin_ne(c,x,v,_) if x.length==1 => if (v.value %c(0).value==0){x(0).neq(v.value /c(0).value)}; false
        case all_different_int(x,_) if x.length==1 => false
        case count_eq(x,v,n,_) if v.isBound && n.isBound && x.forall(_.isBound) =>
          if(x.count(_.value == v.value)!=n.value)throw new UnsatException(c.toString())
          false
        case count_eq(x,v,n,_) if v.isBound && n.isBound && x.count(!_.isBound) == 1 =>
          val cnt = x.count(vv => vv.isBound && vv.value==v.value)
          if(cnt == n.value)x.foreach(vv => if (!vv.isBound) vv.neq(v.value))
          else if(cnt + 1 == n.value)x.foreach(vv => if (!vv.isBound) vv.bind(v.value))
          else throw new UnsatException(c.toString())
          false
        //added this line for ill-defined variables in Circuit. Thanks to Emil Kajgaard
        case circuit(x,_) => x.foreach(_.inter(DomainRange(1,x.length))); true
        case int_abs(a,b,_) if a.isBound => b.bind(math.abs(a.value)); false
        case int_abs(a,b,_) if b.isBound => a.inter(DomainSet(Set(b.value,-b.value))); false
        case array_int_element(x,y,z,_) if x.isBound => z.bind(y(x.value-1).value); false
        case array_int_element(x,y,z,_) if z.isBound => x.inter(DomainSet(y.zipWithIndex.filter{case (v,i) => v.value == z.value}.map{case(v,i) => i+1}.toSet)); false
        case bool_lt(x, y, _) => x.bind(false); y.bind(true); false
        //The cstrs below might need to be iterated until fixpoint... They should be removed if we use CP.
        case int_le(x, y, _ ) => y.geq(x.min); x.leq(y.max); true
        case int_lt(x, y, _ ) =>{
          //println(x+x.dom.toString()+"\t"+y+y.dom);
          y.geq(x.min+1); x.leq(y.max-1); 
          //println(x+x.dom.toString()+"\t"+y+y.dom);
          true
        } 
        case int_eq(x, y, _ ) => y.geq(x.min); y.leq(x.max); x.geq(y.min); x.leq(y.max); true
        //case bool_eq(x, y, _ ) => y.geq(x.min); y.leq(x.max); x.geq(y.min); x.leq(y.max); true
        case _ => true 
      };
      if(!ret)log(3,"Simplified "+c)
      ret});
      var (cstrs,retract)= prop()
      while(!retract.isEmpty){
        log(1,"Looping")
        //model.constraints = cstrs
        for(c <- retract){
          c.retract()
        }
        //TODO: This step is only done if some constraint was retracted... Should move it!
        val (c2,r2) = cstrs.partition{
          case reif(c,b) if b.isBound => false
          case _ => true
        }
        model.constraints.clear()
        model.constraints ++= c2;
        for(c<-r2){
          c match {
            case reif(c2,b) =>{
              var c3 = c2;
              if(!b.boolValue){
                c3 = c2 match {
                  case int_eq(x,y,a) => int_ne(x,y,a)
                  case int_ne(x,y,a) => int_eq(x,y,a)
                  case int_lt(x,y,a) => int_le(y,x,a)
                  case int_le(x,y,a) => int_lt(y,x,a)
                  case _ => null
                }
              }
              if(c3!=null){
                model.constraints += c3
                log(3,"Rewrote "+c+ " => "+c3)
                c3.insert()
                c.retract()
              }else{
                model.constraints += c
              }
            } 
          }
          
        }
        val (c,r) = prop()
        cstrs = c;
        retract = r;
       // retract = List.empty
      }
  }
  
}