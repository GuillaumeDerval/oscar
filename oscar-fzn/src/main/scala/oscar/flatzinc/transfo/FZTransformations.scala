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

object FZModelTransfo {
  
  def areAllIneq(cstrs: Set[Constraint]): Boolean = {
    cstrs.forall{ 
      case bool_le(x,y,_) => true
      case bool_lin_le(x,y,z,_) => true
      case bool_lt(x,y,_) => true
      case int_le(x,y,_) => true
      case int_lin_le(x,y,z,_) => true
      case int_lt(x,y,_) => true
      case _ => false
    }
  }
  
  def findInvariantsFromObjective(model: FZProblem, log: Log): Unit = {
    val visited = MSet.empty[Variable] 
    var front: Queue[(Variable,Constraint,Objective.Value)] = Queue.empty[(Variable,Constraint,Objective.Value)]//Not sure the Constraint is useful
    var front2: Queue[(Variable,Constraint,Objective.Value)] = Queue.empty[(Variable,Constraint,Objective.Value)]
    model.search.variable.foreach(v => front.enqueue((v,null,model.search.obj)))//foreach on Option adds only if there is such a variable.
    var cnt = 0;
    var first = true
    while(!front.isEmpty){
     // println(front.size + " "+ visited.size+ " " +model.variables.length)
      val (v,c,obj) = front.dequeue()
     // visited.add(v)
      if(!v.isDefined){
        val cand = v.cstrs.toList.filter((c: Constraint) => c.definedVar.isEmpty && c.canDefineVar && c.getCandidateDefVars().contains(v))
        val cand2 = cand//.filter(c => ! dependsOn(c,v,false))
        if(cand2.length > 1){
          log(2,"! Found a variable that could be defined by more than one invariant (from Objective):"+v+" "+cand2.toString)
          
        }else{
          log(2,"!!Otherwise "+v+" "+cand2.toString)
        }
        //Select the first suitable constraint
        if(!cand2.isEmpty){
          if(cand2.length==1 || !first){
            val cc = cand2.head
            cc.setDefinedVar(v)
            cnt+=1
          }else{
            front2.enqueue((v,c,obj))
          }
        }else{
          if(areAllIneq(v.cstrs)){
            log("Found a possibility to make a min/max constraint for "+v);
            //v.cstrs.foreach(println(_))
          } 
        }
      }
      if(v.isDefined){
        v.definingConstraint.get.getVariables().foreach(vv => if(!visited.contains(vv) && !vv.isBound) {visited.add(vv); front.enqueue((vv,v.definingConstraint.get,Objective.SATISFY))})
      }
      if(front.isEmpty && !front2.isEmpty){
        first = false
        front = front2
        front2 = Queue.empty[(Variable,Constraint,Objective.Value)]
      }
    }
    log(1,"Found "+cnt+" new invariants from the objective.")
  }
  
  def findInvariants(model: FZProblem, log:Log):Unit = {
    findInvariantsFromObjective(model,log)//Start from the objective and reach as far as possible.
    findInvariants(model,log,0)//First only variables that are directional by nature
    findInvariants(model,log,1)//Then all of them
  }
  def findInvariants(model: FZProblem, log:Log,step:Int) = {
    //Find all free variables of the model.
    var freeVariables: List[Variable] =
      model.variables.toList.filter((variable: Variable) =>
        //model.map(id).id == id  //to take each variable only once (as there may be aliases)
        //&&
        !variable.isDefined //to remove the ones already defined
        && !variable.isBound //to remove de facto constants
          //JNM: Removed the following because the CP search variables are maybe not the CBLS search variables. 
          //This makes a huge difference for the BACP for instance.
          //&&
          //!model.search.heuristics.exists((h) => h._1.exists((v: Variable) => v == model.map(id) && (v.max - v.min) < 1000)) // I am not sure if this is needed...
          )
    
          
    //Remove free variables which are defined by a eq constraint with a constant argument. 
    //Replaced this code by checking whether the domain is a singleton. above (after propagation of the domains)
    
          
    //Find all constraints which do not already define a variable and can define a variable (be turned into an invariant)
    
    def heuristicAccept(c: Constraint): Boolean = {
      step > 0 || c.isInstanceOf[SimpleDefiningConstraint]
    }
    //log(freeVariables.toString)
    //For all free variables
    for (v <- freeVariables.sortBy((x:Variable) => -x.domainSize)){//With((x: Variable, y: Variable) => x.max.toLong - x.min.toLong > y.max.toLong - y.min.toLong)) {
      //println(v)
      //println(v.cstrs)
      val cand = v.cstrs.filter((c: Constraint) => heuristicAccept(c) && c.definedVar.isEmpty && c.canDefineVar && c.getCandidateDefVars().contains(v))
      val cand2 = cand//.filter(c => ! dependsOn(c,v,false))
      if(cand2.size > 1){
        log(2,"! Found a variable that could be defined by more than one invariant:"+v+" "+cand2.toString)
      }
      //Select the first suitable constraint with smallest number of variables (heuristic for Costas Array, might not work for others, actually a proxy for the smallest resulting output domain)
      if(!cand2.isEmpty){
        //log(cand2.toString)
        cand2.minBy(c => c.getVariables().length).setDefinedVar(v)
      }
      
    }
  }
  def dependsOn(c: Constraint, v: Variable,test: Boolean = true): Boolean = {
    c.getVariables().exists(w => if (w.isDefined) c.definedVar!= Some(w) && dependsOn(w.definingConstraint.get,v) 
                                 else test && w == v)
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
          case int_lin_eq(c,x,y,_) if x.forall(_.isBound) => b.bind(c.zip(x).foldLeft(0)((acc,cur) => acc + cur._1.value*cur._2.value)==y.value); false
          case int_lin_le(c,x,y,_) if x.forall(_.isBound) => b.bind(c.zip(x).foldLeft(0)((acc,cur) => acc + cur._1.value*cur._2.value)<=y.value); false
          case _ => true;
        }
        case array_bool_and(x,y,_) if x.forall(_.isBound) => y.bind(x.forall(v => v.boolValue)); false
        case array_bool_and(x,y,_) if x.exists(_.isFalse) && y.isFalse => false
        case array_bool_or(x,y,_) if x.forall(_.isBound) => y.bind(x.exists(v => v.boolValue)); false
        case array_bool_or(x,y,_) if x.exists(_.isTrue) && y.isTrue => false
        case int_ne(x,y,_) if y.isBound => x.neq(y.value); false
        case int_ne(x,y,_) if x.isBound => y.neq(x.value); false
        case set_in(x,d,_) => x.inter(d); false
        case int_lin_eq(c,x,v,_) if x.length==1 && math.abs(c(0).value) == 1 => x(0).bind(v.value/c(0).value); false
        case int_lin_eq(c,x,v,_) if x.filterNot(_.isBound).length == 1 =>{
          val (rest,one) = x.zip(c).partition(_._1.isBound);
          if(math.abs(one(0)._2.value)==1){
            val sumrest = rest.foldLeft(0)((acc,xc) => acc + xc._1.value*xc._2.value)
            one(0)._1.bind((v.value - sumrest)/one(0)._2.value);
            false
          }else true
        }
        case int_lin_eq(c,x,v,_) if x.filterNot(_.isBound).length==0 =>{
          val sum = x.zip(c).foldLeft(0)((acc,xc) => acc + xc._1.value*xc._2.value)
          if(sum!=v.value) throw new UnsatException(c.toString())
          false
        }
        //added for wwtpp with Mzn 2.0.2
        //TODO: avoid overflows!
        case int_lin_eq(c,x,v,_) if c.forall(_.value.abs <= 1) && x.filter(_.domainSize==Int.MaxValue).length==1 =>{
          val (theone,others) = x.zip(c).partition(_._1.domainSize==Int.MaxValue)
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
        case int_lin_le(c,x,v,_) if x.filterNot(_.isBound).length == 1 =>{
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
        case all_different_int(x,_) if x.length==1 => false
        //added this line for ill-defined variables in Circuit. Thanks to Emil Kajgaard
        case circuit(x,_) => x.foreach(_.inter(DomainRange(1,x.length))); true
        case int_abs(a,b,_) if a.isBound => b.bind(math.abs(a.value)); false
        case int_abs(a,b,_) if b.isBound => a.inter(DomainSet(Set(b.value,-b.value))); false
        case array_int_element(x,y,z,_) if x.isBound => z.bind(y(x.value-1).value); false
        case array_int_element(x,y,z,_) if z.isBound => x.inter(DomainSet(y.zipWithIndex.filter{case (v,i) => v.value == z.value}.map{case(v,i) => i+1}.toSet)); false
        case bool_lt(x, y, _) => x.bind(false); y.bind(true); false
        //The cstrs below might need to be iterated until fixpoint...
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
                for(v <- c3.getVariables()){
                  v.addConstraint(c3);
                }
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
  
  
  
  def getSortedInvariants(invariants: List[Constraint])(implicit log: Log): (List[Constraint],List[Constraint]) = {
    //val invariants = inv;
    var sorted = List.empty[Constraint];
    //mapping maps each constraint to the number of defined variables the constraint depends on.
    val mapping = MMap.empty[Constraint, Int];
    var heads = List.empty[Constraint]
    var removed = List.empty[Constraint]
    var sortedend = List.empty[Constraint];
    for (i <- invariants) {
      mapping += i -> i.getVariables.toSet.filter((v) => v.isDefined && (v.definingConstraint.get != i)).size;
      if(mapping(i)==0){
        heads = i :: heads;
        mapping.remove(i)
      }
    }
    val mappingB = MMap.empty[Constraint, Int];
    var tails = List.empty[Constraint]
    for(i <- mapping.keys){
      mappingB += i -> i.definedVar.get.cstrs.filter((c) => c!=i && c.definedVar.isDefined && mapping.contains(c)).size
      
    }
    def explore() = {
      while (!heads.isEmpty) {
        val k = heads.head
        heads = heads.tail
        sorted = k::sorted
        mappingB.remove(k)
        for(j <- k.definedVar.get.cstrs){
          if(mapping.contains(j) ){
            mapping(j) = mapping(j)-1
            if(mapping(j)==0){
              heads = j :: heads;
              mapping.remove(j)
            }
          }
        }
      }
    }
    def exploreBackward() = {
      for(i <- mappingB.keys){
        if(mappingB(i)==0){
          tails = i :: tails;
          mappingB.remove(i);
        }
      }
      while(!tails.isEmpty){
        val k = tails.head
        tails = tails.tail
        sortedend = k :: sortedend
        mapping.remove(k)//to avoid searching it again
        //The toSet is necessary when some variables appear several times in the same constraint. Then we only want to substract 1.
        for(j <- k.getVariables.toSet.filter(v => v.isDefined && (v.definingConstraint.get != k)).map(v => v.definingConstraint.get)){
          if(mappingB.contains(j)){
            mappingB(j) = mappingB(j)-1
            if(mappingB(j)==0){
              tails = j:: tails
              mappingB.remove(j)
            }
          }/*else{
            println(j.definedVar+ " ")
          }*/
        }
      }
    }
    explore()
    exploreBackward()
    if(!mapping.isEmpty){
      log("There is a cycle in the set of invariants.!"+mapping.size)
      //print(mapping.mkString("\n"))
    }
    while(!mapping.isEmpty){
      val remc = mapping.keys.minBy(c => (c.definedVar.get.domainSize,-mapping(c),-mappingB(c)))//foldLeft((null.asInstanceOf[Constraint],Int.MinValue))((best,cur) => {val curval = - cur.definedVar.get.domainSize/*mapping(cur)*//*cur.definedVar.get.cstrs.filter(c => c!=cur && mapping.contains(c) && mapping(c)==1).length*/; if(curval > best._2) (cur,curval) else best;});
      mapping.remove(remc)
      mappingB.remove(remc)
      for(j <- remc.definedVar.get.cstrs){
        if(mapping.contains(j) ){
          mapping(j) = mapping(j) -1
          if(mapping(j)==0){
            heads = j :: heads;
            mapping.remove(j)
          }
        }
      }
      for(j <- remc.getVariables.filter(v => v.isDefined && (v.definingConstraint.get != remc)).map(v => v.definingConstraint.get)){
        if(mappingB.contains(j)){
          mappingB(j) = mappingB(j) -1
        }
      }
      log(2,"Removed "+remc+ " for "+remc.definedVar.get)
      removed = remc :: removed
      remc.unsetDefinedVar(remc.definedVar.get)
      explore()
      exploreBackward()
     // println(mapping.map{case (c,i) => (c,i,c.getVariables.filter(v => {val cc = v.definingConstraint.getOrElse(c); /*mapping.contains(cc) &&*/ cc!=c}).toList.map(v => v.definingConstraint.get )) }.mkString("\n"))      
    }
    log("Had to remove "+removed.length+" invariants to be acyclic.")
    //println((sorted.reverse++sortedend).map(_.definedVar.get))
    return (sorted.reverse++sortedend,removed);
  }
}