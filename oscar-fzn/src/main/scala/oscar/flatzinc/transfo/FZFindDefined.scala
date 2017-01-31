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

object FZFindDefined {
  
  def findInvariants(model: FZProblem, log:Log, searchVars: Iterable[Variable]):Unit = {
    val mysearchVars = searchVars.toSet
    findInvariantsFromObjective(model,log,mysearchVars)//Start from the objective and reach as far as possible.
    findInvariants(model,log,0,mysearchVars)//First only constraints that are directional by nature
    findInvariants(model,log,1,mysearchVars)//Then all of them
  }
  
  
  
  def findInvariantsFromObjective(model: FZProblem, log: Log, searchVars: Set[Variable]): Unit = {
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
      if(!v.isDefined && !searchVars.contains(v)){
        val cand = v.cstrs.toList.filter((c: Constraint) => c.definedVar.isEmpty && c.canDefineVar && c.getCandidateDefVars().contains(v))
        val cand2 = cand//.filter(c => ! dependsOn(c,v,false))
        if(cand2.length > 1){
          log(2,"! Found a variable that could be defined by more than one invariant (from Objective):"+v+" "+cand2.toString)
          
        }
        //else{
        //  log(2,"!!Otherwise "+v+" "+cand2.toString)
        //}
        
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
        v.definingConstraint.get.getVariables().foreach(vv =>
          if(!visited.contains(vv) && !vv.isBound) {
            visited.add(vv)
            front.enqueue((vv,v.definingConstraint.get,Objective.SATISFY))
          })
      }
      if(front.isEmpty && !front2.isEmpty){
        first = false
        front = front2
        front2 = Queue.empty[(Variable,Constraint,Objective.Value)]
      }
    }
    log(1,"Found "+cnt+" new invariants from the objective.")
  }
  
  //TODO: This should go to the simplification bit.
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
  
  private def findInvariants(model: FZProblem, log:Log,step:Int, searchVars: Set[Variable]) = {
    //Find all free variables of the model.
    val freeVariables: List[Variable] =
      model.variables.toList.filter((variable: Variable) =>
        //model.map(id).id == id  //to take each variable only once (as there may be aliases)
        //&&
        !variable.isDefined //to remove the ones already defined
        && !variable.isBound //to remove de facto constants
        && ! searchVars.contains(variable)
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