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

import oscar.cbls.invariants.core.computation.CBLSIntVar
import oscar.cbls.invariants.core.computation.CBLSIntConst
import oscar.cbls.invariants.core.computation.CBLSSetVar
import oscar.cbls.invariants.lib.logic._
import oscar.cbls.invariants.lib.minmax._
import oscar.cbls.invariants.core.computation.Store
import oscar.cbls.constraints.core.ConstraintSystem
import oscar.cbls.objective.{Objective => CBLSObjective}
import oscar.cbls.search.SearchEngine
import oscar.flatzinc.Log
import oscar.flatzinc.model.Domain
import oscar.flatzinc.model.DomainRange
import oscar.flatzinc.model.DomainSet
import oscar.flatzinc.cbls.FZCBLSModel
import oscar.cbls.constraints.core.Constraint
import oscar.cbls.invariants.core.computation.Variable
import oscar.cbls.invariants.lib.set.Inter
import oscar.cbls.invariants.core.computation.IntValue
import oscar.cbls.invariants.core.computation.SetValue




abstract class SearchProcedure extends SearchEngine {
  
  def run(): Unit
  def selectMinImb[R,S](r: Iterable[R] , s: R => Iterable[S],f: (R,S) => Int, st: ((R,S) => Boolean) = ((r:R, s:S) => true)): (R,S) = {
    //TODO: check that it is fine
    val flattened:Iterator[(R,S)] = for (rr <- r.toIterator; ss <- s(rr).toIterator) yield (rr,ss)
    selectMin[(R,S)](flattened.toIterable)((rands:(R,S)) => {/*Console.err.println(rands);*/ f(rands._1,rands._2)}, (rands:(R,S)) => st(rands._1,rands._2))
  }
  
  def showViolatedConstraints(c: ConstraintSystem){
    for(cc <- c.violatedConstraints){
      println(cc + " "+cc.violation.value)
    }
  }
}
class Chain(val a: SearchProcedure*) extends SearchProcedure {
  def run() = {
    a.foreach(_.run())
  }
}

class ActionSearch(action:() => Unit) extends SearchProcedure {
  def run() = action()
}
class FakeSearch extends SearchProcedure {
  def run()={}
}

class GreedySearch(val level: Int = 1, val m: FZCBLSModel,val sc: SearchControl) extends SearchProcedure {
  assert(level > 0 && level < 4)
  val log = m.log
  def run(){
    var it = 0;
    log("Starting Greedy Search level "+level)
    log("Starting Violation: "+m.objective.violation.value)
    //val varval = for (x <- m.vars; v <- x.getDomain()) yield (x,v)
    if(m.vars.length>0){
      sc.handlePossibleSolution();
      var cont = true;
      while(cont && !sc.stop()){
        val varval = for (x <- m.vars.filter(m.c.violation(_).value>0); v <- x.getDomain()) yield (x,v)
        val cur = m.objective.objective.value
        val move = level match {
          case 1 => selectFirst(varval.map(List(_)),(vv:List[(CBLSIntVarDom,Int)]) => cur > m.objective.objective.assignVal(vv))
          case 2 => val x:Iterator[List[(CBLSIntVarDom,Int)]] = varval.iterator.flatMap(v => varval.iterator.map(v2 => List(v,v2)))
            selectFirst(x.toIterable,(vv:List[(CBLSIntVarDom,Int)]) => cur > m.objective.objective.assignVal(vv))
          case 3 => val x:Iterator[List[(CBLSIntVarDom,Int)]] = varval.iterator.flatMap(v1 => varval.iterator.flatMap(v => varval.iterator.map(v2 => List(v1,v,v2))))
            selectFirst(x.toIterable,(vv:List[(CBLSIntVarDom,Int)]) => cur > m.objective.objective.assignVal(vv))
        }
        if(move != null && m.objective.objective.assignVal(move)<cur){
          move.foreach{ case (x,v) => x := v}
          sc.handlePossibleSolution
          it +=1;
        }else cont = false
      }
    }
    log("Done Greedy Search at "+m.getWatch())
    log("Ending Violation: "+m.objective.violation.value)
    log("Nb Moves: "+it)
  }
}
class SimpleLocalSearch(val m:FZCBLSModel,val sc: SearchControl) extends SearchProcedure {
  val violation: Array[IntValue] = m.vars.map(m.c.violation(_)).toArray;
  val log = m.log
  def run(){
    var improving = 3;
    var lastImproved = 0;
    var i = 0;
    var it = 0;
    log("Starting Simple Local Search")
    log("Starting Violation: "+m.objective.violation.value)
    if(m.vars.length>0){
      sc.handlePossibleSolution();
      while(improving > 0 && !sc.stop()){
        val currentVar = m.vars(i);
        if(violation(i).value > 0){//TODO??
          val k = selectMin(currentVar.getDomain())(k=> m.objective.objective.assignVal(currentVar,k))
          if(k!=currentVar.value){
            val obj = m.objective.objective.value
            currentVar := k;
            sc.handlePossibleSolution();
            it+=1;
            if(m.objective.objective.value < obj){
              lastImproved = i;
              improving = 3
            }
          }
        }
        i+=1;
        if(i==m.vars.length){
          i=0;
          //log(2,"turned around "+m.objective.objective.value)
        }
        if(i==lastImproved)improving -= 1;
      }
    }
    log("Done Simple Local Search at "+m.getWatch())
    log("Ending Violation: "+m.objective.violation.value)
    log("Nb Moves: "+it)
  }
}

class SearchControl(val m: FZCBLSModel, val objLB:Int, val MaxTimeMilli: Int,val stopOnSat:Boolean){
  def stop(): Boolean = {
    m.getWatch() >= MaxTimeMilli || //ran out of time
    (m.objective.violation.value==0 && stopOnSat) || //sat problem
    (m.objective.violation.value==0 && m.objective.objectiveWeight.value==0) ||
    (m.objective.violation.value==0 && m.objective.getObjectiveValue() == objLB) //reached the lower bound
  }
  var bestKnownObjective = Int.MaxValue 
  var bestKnownViolation = Int.MaxValue
  def handlePossibleSolution(){
    if(m.objective.violation.value==0 && m.objective.getObjectiveValue() < bestKnownObjective){
      bestKnownViolation = 0
      bestKnownObjective = m.objective.getObjectiveValue();
      m.handleSolution();
      if(bestKnownObjective==objLB && !stopOnSat)println("==========")//added !stopOnSat to not print it on Satisfaction problems.
    }else if(m.objective.violation.value < bestKnownViolation){
      bestKnownViolation = m.objective.violation.value
      m.log("Best Violation: "+bestKnownViolation+ "\tat "+m.getWatch()+ " ms")
    }
  }
  def cancelObjective() = {
    m.objective.objectiveWeight := 0;
  }
  def restoreObjective() = {
    m.objective.objectiveWeight := 1;
  }
  def weightedBest = bestKnownViolation * m.objective.violationWeight.value + bestKnownObjective * m.objective.objectiveWeight.value
}


abstract class NeighbourhoodSearch(val m: FZCBLSModel,val sc: SearchControl) extends SearchProcedure {
  val log = m.log
  val neighbourhoods: List[Neighbourhood] = m.neighbourhoods 
  val searchVariables = neighbourhoods.foldLeft(Set.empty[CBLSIntVarDom])((acc: Set[CBLSIntVarDom], x: Neighbourhood) => acc ++ x.getVariables().filterNot(_.isInstanceOf[CBLSIntConstDom])).toArray

  
}

/*
class NeighbourhoodCBSearch(m: FZCBLSModel, sc: SearchControl) extends NeighbourhoodTabuSearch(m,sc){
  val varsOfCstr = m.c.getPostedConstraints.foldLeft(Map.empty[Constraint,Set[CBLSIntVarDom]])((acc,c) => 
                   acc + (c._1 -> c._1.constrainedVariables.foldLeft(Set.empty[Variable])((acc,v) =>
                   acc ++ m.m.getSourceVariables(v)).intersect(searchVariables.toSet[Variable]).map(v => v.asInstanceOf[CBLSIntVarDom])))
  val cstrs = m.c.getPostedConstraints.map(_._1).toArray
  val viols = cstrs.map(_.violation)
  //Do not register for partial propagation as it leads to to much memory for large problems
  //viols.foreach(v => m.m.registerForPartialPropagation(v))
  val violatedConstraints:SetValue = Filter(viols) 
  val tabuC: Array[CBLSIntVar] = cstrs.map(c => CBLSIntVar(sc.m.m, 0, 0 to Int.MaxValue, "Tabu_" + c.toString())).toArray;
  val nonTabuConstraints: SetValue = SelectLEHeapHeap(tabuC.asInstanceOf[Array[IntValue]], it);
  val nonTabuAndViolated = Inter(nonTabuConstraints,violatedConstraints);
  var tenureC = 2
  val maxTenureC = cstrs.length/2
  //println(varsOfCstr)
  //TODO: Make constraints TABU
  def assign(c: Constraint, x: CBLSIntVar, v: Int):Int = {
    val o = x.value
    x:=v
    val res = c.violation.value
    x:=o
    res
  }
  /*{
    val avar = variableMap.toList.head._1
    varsOfCstr.valuesIterator.filter(vs => vs.contains(avar.asInstanceOf[CBLSIntVarDom])).foreach(println(_))
    throw new Exception()
  }*/
  def run() = {
    
    log("Starting Constraint-Based Search")
    while(!sc.stop()){
      it++;
      if(nonTabuAndViolated.value.nonEmpty){
        val ci = nonTabuAndViolated.value.toArray.apply(RandomGenerator.nextInt(nonTabuAndViolated.value.size))
        val c = cstrs(ci)
        val xv = selectMinImb(varsOfCstr(c), (x:CBLSIntVarDom) => x.getDomain(), (x:CBLSIntVar,v:Int) => m.objective.objective.assignVal(x,v),(x:CBLSIntVar,v:Int) => x.value != v && assign(c,x,v) < c.violation.value && nonTabuVariables.value.contains(variableMap(x)));
        //println()
        //println(c)
        xv match{
          case (x,v) =>
            //println(x,v)
            x := v
//            println(c.violation)
//            println(violatedConstraints.value.size)
//            println(violatedConstraints.value.foldLeft(List.empty[String])((acc,ci) => varsOfCstr(cstrs(ci)).toList.sortBy(v => v.name).mkString("[",",","]"):: acc).sorted.mkString(", "))
            tabuC(ci) := it.value + Math.min(maxTenureC, tenureC + RandomGenerator.nextInt(violatedConstraints.value.size+1));
            tabu(variableMap(x)) := tabuC(ci).value + 1;
            tenureC +=1
            //tenure += 1
          case _ => 
//            println("skip");
//            println(varsOfCstr(c).intersect(nonTabuVariables.value.map(searchVariables(_))))
            tenureC = math.max(tenureC-1,1)
        }
      }else if(m.c.violation.value==0){
        val (x,v) = selectMinImb(searchVariables,(x:CBLSIntVarDom) => x.getDomain(),(x:CBLSIntVar,v:Int) => m.objective.objective.assignVal(x,v),(x:CBLSIntVar,v:Int) => x.value != v);
//        println("==========================")
//        //print (".")
//        println(x,v)
        x := v
        //tenureC = math.max(tenureC-1,1)
        tabu(variableMap(x)) := it.value + tenureC + RandomGenerator.nextInt(100);
        //tenure -= 1
        //tenure = math.max(tenure,1)
      }else{
//        println("skip2")
      }
      //TODO: update the weights
      sc.handlePossibleSolution();
      
    }
    log("Completed Constraint-Based Search at "+m.getWatch())
  }
}
*/

abstract class NeighbourhoodTabuSearch(m: FZCBLSModel, sc: SearchControl) extends NeighbourhoodSearch(m,sc){
  
  val variableMap = (0 until searchVariables.length).foldLeft(Map.empty[CBLSIntVar, Int])((acc, x) => acc + (searchVariables(x) -> x));  
  
  val it = CBLSIntVar(m.m, 1, 0 to Int.MaxValue, "it");
  val tabu: Array[CBLSIntVar] = searchVariables.map(v => CBLSIntVar(sc.m.m, 0, 0 to Int.MaxValue,  "Tabu_" + v.name)).toArray;
  val nonTabuVariables = SelectLEHeapHeap(tabu.asInstanceOf[Array[IntValue]], it);
  val MaxTenure = (searchVariables.length * 0.6).toInt;
  val MinTenure = 2 + 0 * (searchVariables.length * 0.1).toInt;
  val tenureIncrement = Math.max(1, (MaxTenure - MinTenure) / 10);
  var tenure = MinTenure
  var ecnt = 0;
  var bcnt = 0;
  val baseSearchSize = 100;
  val searchFactor = 20;
  
  def acceptMove(best: Int,nonTabuSet: Set[CBLSIntVar])(m:Move): Boolean = {
    //changed forall to exists after a suggestion of Gustav.
    m.getModified.exists(nonTabuSet.contains(_)) || m.value < best 
  }
  
  def makeMove(extendedSearch: Boolean){
    //if(it.value%10==0){
    //    log(2,"it: "+it.value+" violation: "+m.c.violation.value+" objective: "+m.objective.getObjectiveValue())
    //  }
    //log(3,"it: "+it.value+" violation: "+m.c.violation.value+" objective: "+m.objective.getObjectiveValue())
    //if(log.level>=4) for(cc <- m.c.violatedConstraints){
    //    log(4,cc + " "+cc.violation.value)
    //}
    //log(3,"Non tabu variables: "+nonTabuVariables.value.size)
    val nonTabuSet = nonTabuVariables.value.map(searchVariables(_).asInstanceOf[CBLSIntVar]);
    val bestValue = sc.weightedBest
    if(extendedSearch) ecnt+=1 else bcnt+=1
    val bestNeighbour = selectMin(neighbourhoods.map((n: Neighbourhood) =>
      if (extendedSearch) {
        //log(3,"E "+n)
        n.getExtendedMinObjective(it.value, acceptMove(bestValue,nonTabuSet)/*, bestNow*/)
      } else {
        //log(3,"S "+n)
        n.getMinObjective(it.value, acceptMove(bestValue,nonTabuSet))
      }))(_.value)
//      println("X")
    if(bestNeighbour!=null){
      //TODO: Aspiration sometimes accepts moves that do not improve but seem to improve because of changing weights. 
      //if(log.level > 0 && bestNeighbour.getModified.exists(!nonTabuSet.contains(_)))log("Aspiration");
      //log(3,bestNeighbour.toString)
      bestNeighbour.commit();
      sc.handlePossibleSolution()
    }else
      log("No move exists!");
    val modifiedVars = if(bestNeighbour!=null) bestNeighbour.getModified else Set.empty[CBLSIntVar]
    for (v <- modifiedVars) {
      val index = variableMap(v);
      //This could be it.value + tenure + random(tenureIncrement) to introduce more randomness
      //tabu(index) := it.value + tenure;
      tabu(index) := it.value + Math.min(MaxTenure, tenure + RandomGenerator.nextInt(tenureIncrement));
    }
    it++;
  }
}

class NeighbourhoodSearchOPT(m:FZCBLSModel, sc: SearchControl) extends NeighbourhoodTabuSearch(m,sc) {
  
  override def run()= {
    log("Starting Optimization Search")
    
    var bestNow = Int.MaxValue;
    var best = bestNow;
    var itSinceBest = 0;
    var numOfMaxTenure = 0;

    var timeOfBest = m.getWatch();
    var itOfBalance = 0;
    var minViolationSinceBest = Int.MaxValue;
    var minObjectiveSinceBest = Int.MaxValue;
    var lastMinObjective = Int.MinValue;
    
    
    

    var wait = 0;
    val waitDec = 1;
  

    //m.objective.objectiveWeight := 0;//TODO: What is this??? Remove it?
    while (!sc.stop()) {
      makeMove(true)

      if (wait > 0) {
        wait -= waitDec;
      } else {
        itSinceBest += 1;
      }
      
      // Minimize the problem
      // There are two special cases to look out for here.
      // 1) The violation is within such a small range (compared with the objective) that the violation is ignored by the search.
      //	- This shows when the violation is above 0 for a long time (possibly forever) and the objective is at a "good" low value
      // 2) The violation can grow so quickly that it overshadows the objective (ie the opposite of 1).
      //  - This shows when the violation is 0 for a long time (possibly forever) and the objective does not decrease
      //
      // There is of course also the problem of the dynamic tenure behaving badly but that is waaaaay harder to detect and do something about.
      minViolationSinceBest = Math.min(minViolationSinceBest, m.c.violation.value)
      minObjectiveSinceBest = Math.min(minObjectiveSinceBest, m.objective.getObjectiveValue())
      if (m.objective.getObjectiveValue() < bestNow || (m.c.violation.value == 0 && m.objective.getObjectiveValue() < best)) {
        bestNow = m.objective.getObjectiveValue()
        tenure = Math.max(MinTenure, tenure - 1)
        if (m.c.violation.value == 0 && bestNow < best) {
          best = bestNow;
          timeOfBest = m.getWatch();
          itOfBalance = it.value
          minViolationSinceBest = Int.MaxValue
          minObjectiveSinceBest = Int.MaxValue
          lastMinObjective = bestNow;
          tenure = Math.max(MinTenure, tenure / 2)
        }
        itSinceBest = 0;
      }
      //println(it.value - itOfBalance + " " + objectiveWeight.value + " " + violationWeight.value)
      if (it.value - itOfBalance > baseSearchSize * 2 && wait == 0) {
        if (minViolationSinceBest > 0) { // 1)
          m.objective.increaseViolationWeight(minObjectiveSinceBest)
        } else if (bestNow <= lastMinObjective) { // 2)
          m.objective.increaseObjectiveWeight(minObjectiveSinceBest)
        }
        lastMinObjective = bestNow;
        minViolationSinceBest = Int.MaxValue
        minObjectiveSinceBest = Int.MaxValue

        itOfBalance = it.value;
      }
      if (itSinceBest > tenure + baseSearchSize + searchFactor * (tenure / tenureIncrement)) {
        itSinceBest = 0;
        tenure = Math.min(MaxTenure, tenure + tenureIncrement);
        if (tenure == MaxTenure) {
          //Wait will be long enough to clear the tabu list.
          if (m.getWatch() - timeOfBest > sc.MaxTimeMilli / 4) {
            //println("% Reset");
            timeOfBest = m.getWatch();
            log("Reset neighourhoods")
            for (n <- neighbourhoods)
              n.reset();
          }
          wait = tenure + baseSearchSize;
          tenure = MinTenure;
          bestNow = m.objective.getObjectiveValue()
        }
      }
      if(m.getWatch() > timeOfBest + 300000){
        timeOfBest = m.getWatch()
        log("Reset neighourhoods 5 minutes")
        for (n <- neighbourhoods)
          n.reset();
      }
    }
    log("Completed Optimization Search at "+m.getWatch())
    log("nb moves "+ecnt+"\t"+bcnt)
  }
}

class NeighbourhoodSearchSAT(m:FZCBLSModel, sc: SearchControl) extends NeighbourhoodTabuSearch(m,sc) {
  override def run()= {
    log("Starting Satisfaction Search")
    var extendedSearch = true;
    var roundsWithoutSat = 0;
    val maxRounds = 5;

    var timeOfBest = m.getWatch();
    var itSinceBest = 0;
    var bestViolation = Int.MaxValue


    var wait = 0;
    val waitDec = 1;
    
    while (!sc.stop()) {
      makeMove(extendedSearch)
      
      if (wait > 0) {
        wait -= waitDec;
      } else {
        itSinceBest += 1;
      }
      if (m.c.violation.value < bestViolation) {
        bestViolation = m.c.violation.value
        itSinceBest = 0;
        timeOfBest = m.getWatch()
        tenure = Math.max(MinTenure, tenure - 1)
        if (tenure == MinTenure) {
          extendedSearch = false;
        }
      }
      if (m.c.violation.value > bestViolation * 10) {
        extendedSearch = true;
      }
      if (itSinceBest > tenure + baseSearchSize + searchFactor * (tenure / tenureIncrement)) {
        extendedSearch = true;
        itSinceBest = 0;
        tenure = Math.min(MaxTenure, tenure + tenureIncrement);
        if (tenure == MaxTenure) {
          //Wait will be long enough to clear the tabu list.
          wait = tenure + baseSearchSize;
          bestViolation = Int.MaxValue
          tenure = MinTenure;
          roundsWithoutSat += 1;
          if (roundsWithoutSat >= maxRounds) {
            log("Reset neighourhoods")
            for (n <- neighbourhoods)
              n.reset();
            roundsWithoutSat = 0;
            bestViolation = m.c.violation.value
          }
        }
      }
      if(m.getWatch() > timeOfBest + 300000){
        timeOfBest = m.getWatch()
        log("Reset neighourhoods 5 minutes")
        for (n <- neighbourhoods)
          n.reset();
      }
    }
    log("Completed Satisfaction Search at "+m.getWatch())
    log("nb moves "+ecnt+"\t"+bcnt)
  }
}