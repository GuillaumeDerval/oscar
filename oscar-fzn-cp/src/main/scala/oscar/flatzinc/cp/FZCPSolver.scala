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
 * @author Jean-Noel Monette
 */
package oscar.flatzinc.cp

import oscar.cp._
import oscar.flatzinc.Options
import oscar.flatzinc.parser.FZParser
import scala.collection.mutable.{Map => MMap}
import oscar.flatzinc.model._
import oscar.flatzinc.UnsatException
import oscar.cp.core.CPPropagStrength
import oscar.flatzinc.transfo.FZModelTransfo

class FZCPModel(val model:oscar.flatzinc.model.FZProblem, val pstrength: oscar.cp.core.CPPropagStrength = oscar.cp.Medium, val ignoreUnkownConstraints: Boolean = false) {
  implicit val solver: CPSolver = CPSolver(pstrength)
  solver.silent = true
  val poster = new CPConstraintPoster(pstrength);
  val dicoVars = MMap.empty[String,CPIntVar]
  def getIntVar(v:Variable):CPIntVar = {
    dicoVars.get(v.id) match {
      case None if v.isBound =>
        val c = v match{
          case v:IntegerVariable => CPIntVar(v.value);
          case v:BooleanVariable => CPBoolVar(v.boolValue);
        }
        dicoVars += v.id -> c;
        c
	  case Some(c) => c;

	}
  }
  def getBoolVar(v:Variable):CPBoolVar = {
	dicoVars.get(v.id) match {
	  case None if v.isBound =>
	    val c = v match{
	      case v:BooleanVariable => CPBoolVar(v.boolValue);
	    }
	    dicoVars += v.id -> c;
	    c
	  case Some(c) => c.asInstanceOf[CPBoolVar];
	}
  }
  def createVariables(){
    for(v <- model.variables){
      dicoVars(v.id) = v match{
        case bv:BooleanVariable => CPBoolVar()
        case iv:IntegerVariable => iv.domain match{
          case DomainRange(min, max) => CPIntVar(min, max)
          case DomainSet(v) => CPIntVar(v)
          case _ => throw new RuntimeException("unknown domain")
        }
      }
    }
  }
  def createConstraints(){
    //TODO: Add a try catch for if the problem fails at the root.
    //TODO: Put all the added cstrs in a ArrayBuffer and then post them all at once.
    //try{
        for(c <- model.constraints){
          //TODO: Take consistency annotation to post constraints.
          try{
            add(poster.getConstraint(c,getIntVar,getBoolVar))
          }catch{
            case e: scala.MatchError if ignoreUnkownConstraints => Console.err.println("% ignoring in CP: "+c)
          }
        }
    //}catch{
    //  case e => //throw new UnsatException(e.getMessage());
    //}
  }
  //TODO: why do we need a separate method?
  def add(c:Array[(oscar.cp.Constraint,oscar.cp.core.CPPropagStrength)]){
    c.foreach(cs => solver.add(cs._1,cs._2));
  }
  def createObjective(){
    model.search.obj match{
     case Objective.SATISFY => 
     case Objective.MAXIMIZE => maximize(getIntVar(model.search.variable.get))
     case Objective.MINIMIZE => minimize(getIntVar(model.search.variable.get))
    }
  }
  def updateBestObjectiveValue(value: Int): Boolean = {
    model.search.obj match{
     case Objective.SATISFY => 
     case Objective.MAXIMIZE => getIntVar(model.search.variable.get).updateMin(value+1)
     case Objective.MINIMIZE => getIntVar(model.search.variable.get).updateMax(value-1)
    }
    if(solver.propagate()==oscar.cp.core.CPOutcome.Failure) false
    else true
  }
  def getMinFor(v:IntegerVariable): Int = {
    getIntVar(v).getMin
  }
  def getMaxFor(v:IntegerVariable): Int = {
    getIntVar(v).getMax
  }
  def getMinFor(v:BooleanVariable): Int = {
    getBoolVar(v).getMin
  }
  def getMaxFor(v:BooleanVariable): Int = {
    getBoolVar(v).getMax
  }
  def handleSolution() = {
     model.solution.handleSolution(
      (s: String) => dicoVars.get(s) match {
        case Some(intVar) =>
          intVar.value + ""
        case r => if(s=="true" || s=="false") s 
        else try{
          s.toInt.toString()
        }catch{
          case e: NumberFormatException => {
            throw new Exception("Unhappy: "+r+ " "+s)
          }
        }
     });
   }
  def createSearch() = {
    //TODO: Take into account the search annotations
    solver.search(oscar.cp.binaryLastConflict((model.variables/*-model.search.variable.get*/).map(getIntVar).toArray[CPIntVar]))
  }
  
  def updateModelDomains(): Boolean ={
    try{
      for(v <- model.variables){
        val domSizeBefore = v.domainSize

        v match{
          case bv:BooleanVariable =>
            if(getMinFor(bv)>=1)bv.bind(true)
            if(getMaxFor(bv)<=0)bv.bind(false)
          case iv:IntegerVariable => 
            iv.geq(getMinFor(iv));
            iv.leq(getMaxFor(iv));
        }
        if(v.domainSize < domSizeBefore)
          Console.err.println("% Reducing for " + v + " from " + domSizeBefore + " to " + v.domainSize)
      }
    }catch{
      case e:UnsatException => false
    }
    true
  }
}

class FZCPSolver {
  val pstrength = oscar.cp.Medium 
  
  def solve(opts: Options){
    val log = opts.log();
    log("start")
    val model = FZParser.readFlatZincModelFromFile(opts.fileName,log, false).problem;
     
    
    log("Parsed.")
    FZModelTransfo.propagate(model)(log)
    log("initial variable reduction (to avoid overflows)")
    
    //TODO: Find binary constraints that can be used for views.
    val cpmodel = new FZCPModel(model)
    
    cpmodel.createVariables();
    log("created variables")
    
    cpmodel.createConstraints();
    log("constraints posted")
    
    cpmodel.createObjective();
    log("objective created")
    
    cpmodel.createSearch()
    log("search created")
    
    cpmodel.solver.onSolution{
      //println("found")
      cpmodel.handleSolution()
    }
    
    //TODO: search for more than one solution for optimisation
    //TODO: Remove the time spent in parsing and posting
    val stats = cpmodel.solver.start(nSols= Int.MaxValue,timeLimit=60*15)
    if(stats.completed) println("==========")
    log(stats.toString)

    

  }
}
