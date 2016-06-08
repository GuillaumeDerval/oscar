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
package oscar.flatzinc.cbls

import scala.util.Random
import scala.collection.mutable.{ Map => MMap}
import scala.language.implicitConversions
import oscar.cbls.search._
import oscar.cbls.objective.{ Objective => CBLSObjective }
import oscar.cbls.constraints.core._
import oscar.cbls.constraints.lib.basic._
import oscar.cbls.constraints.lib.global._
import oscar.cbls.invariants.lib.logic._
import oscar.cbls.invariants.lib.minmax._
import oscar.cbls.invariants.core.computation._
import oscar.cbls.invariants.lib.numeric._
import oscar.flatzinc.Options
import oscar.flatzinc.model._
import oscar.flatzinc.model.Constraint
import oscar.flatzinc.model.Variable
import oscar.flatzinc.cbls.support._
import oscar.cbls.invariants.lib.numeric.Sum2
import oscar.flatzinc.transfo.FZModelTransfo
import java.io.PrintWriter
import oscar.flatzinc.parser.FZParser
import oscar.util.RandomGenerator
import oscar.flatzinc.NoSuchConstraintException
import oscar.cbls.objective.IntVarObjective
import oscar.flatzinc.Log
import oscar.flatzinc.cp.FZCPModel
import oscar.flatzinc.cp.FZCPModel




class FZCBLSObjective(cblsmodel:FZCBLSModel,log:Log){
  private val opt = cblsmodel.model.search.obj
  private val objectiveVar = cblsmodel.model.search.variable.map(cblsmodel.getCBLSVar(_)).getOrElse(null)
  val violation = cblsmodel.c.violation
  val violationWeight = CBLSIntVar(cblsmodel.c.model, 1, 0 to (if(violation.max!=0)math.max(1,Int.MaxValue/(violation.max/500000)) else 1) , "violation_weight")
  //TODO: The division does not seem right... why max and not min?
  val objectiveWeight = CBLSIntVar(cblsmodel.c.model, 1, 0 to (if(objectiveVar!=null && objectiveVar.max > 0)math.max(1,Int.MaxValue/(objectiveVar.max/200)) else 1) , "objective_weight")
  private val objective2 = opt match {
        case Objective.SATISFY => violation
        case Objective.MAXIMIZE => Minus(Prod2(violation, violationWeight), Prod2(objectiveVar, objectiveWeight))
        case Objective.MINIMIZE => Sum2(Prod2(violation, violationWeight), Prod2(objectiveVar, objectiveWeight))
      }
  val objective: CBLSObjective = new IntVarObjective(objective2.asInstanceOf[ChangingIntValue])
  def apply() = objective
  def getObjectiveValue(): Int = {
   opt match {
        case Objective.SATISFY => 0
        case Objective.MAXIMIZE => -objectiveVar.value
        case Objective.MINIMIZE => objectiveVar.value
      }
  }
  def increaseViolationWeight(minViolationSinceBest: Int){
    if (objectiveWeight.value > 1) {
      correctWeights(objectiveWeight.value / 2,violationWeight.value)
    } else {
      correctWeights(objectiveWeight.value,violationWeight.value + Math.max(10, Math.abs(minViolationSinceBest / 2)))
    }
  }
  def increaseObjectiveWeight(minObjectiveSinceBest: Int){
    if (violationWeight.value > 1) {
      correctWeights(objectiveWeight.value,violationWeight.value / 2)
    } else {
      correctWeights(objectiveWeight.value + Math.max(10, Math.abs(minObjectiveSinceBest / 2)),violationWeight.value)
    }
  }
  def correctWeights(newObjW: Int,newVioW: Int){
    val minWeight = math.min(newObjW, newVioW)
    objectiveWeight := math.min(newObjW/minWeight,objectiveWeight.max)
    violationWeight := math.min(newVioW/ minWeight,violationWeight.max)
    //violationWeight := 1000 + RandomGenerator.nextInt(10)
    log("Changed Violation Weight to "+violationWeight.value+(if(violationWeight.value==violationWeight.max)"(max)"else ""))
    log("    And Objective Weight to "+objectiveWeight.value+(if(objective.value==objectiveWeight.max)"(max)"else ""))

  }
}


class FZCBLSModel(val model: FZProblem, val c: ConstraintSystem, val m: Store, val log:Log, val getWatch: () => Long) {
  val cblsIntMap: MMap[String, IntValue] = MMap.empty[String, IntValue]
  var vars: List[CBLSIntVarDom] = createVariables();
  var objective = null.asInstanceOf[FZCBLSObjective]
  var neighbourhoods: List[Neighbourhood] = List.empty[Neighbourhood];
  var neighbourhoodGenerator: List[(CBLSObjective,ConstraintSystem) => Neighbourhood] = List.empty[(CBLSObjective,ConstraintSystem) => Neighbourhood]
  def addNeighbourhood(n: (CBLSObjective,ConstraintSystem) => Neighbourhood,removeVars: Array[CBLSIntVarDom]){
    neighbourhoodGenerator = n :: neighbourhoodGenerator 
    //neighbourhoods = n :: neighbourhoods
    vars = vars.filterNot(removeVars.contains(_))
  }
  def addDefaultNeighbourhouds(){
    if (vars.length > 0) {
      addNeighbourhood((o,c) => new MaxViolating(vars.toArray, o, c),Array.empty[CBLSIntVarDom])
      val boolVars = vars.filter((v: CBLSIntVar) => v.min == 0 && v.max == 1)
      if (boolVars.length > 1)
        addNeighbourhood((o,c) => new MaxViolatingSwap(boolVars.toArray, o, c),Array.empty[CBLSIntVarDom]) 
    }
  }
 
  def createNeighbourhoods(){
    neighbourhoods = neighbourhoodGenerator.map(_(objective(),c))
  }
  def createVariables() = {
    var variables: List[CBLSIntVarDom] = List.empty[CBLSIntVarDom];
     //Only create variables that are not fixed by an invariant.
    for (parsedVariable <- model.variables if !parsedVariable.isDefined) {
      parsedVariable match {
        case IntegerVariable(id, dom) =>
          //TODO: Put this in a method! or make it deterministic as the neighbourhoods should take care of the assignments!
          val initialValue = (dom match {
            case oscar.flatzinc.model.DomainRange(min, max) =>
              if(max.toLong - min.toLong > Int.MaxValue) Random.nextInt()
              else{
                val range = (min to max);
                range(Random.nextInt(range.length))
              }
            case DomainSet(values) =>
              val v = values.toArray;
              v(Random.nextInt(v.length))
          })
          val sc = parsedVariable.cstrs.map{ 
          	case c:subcircuit => c.variables.length; 
          	case c:circuit => c.variables.length;
          	case _ => 0}.fold(0)((x, y) => if (x > y) x else y)
          val thedom = if(sc > 0){oscar.flatzinc.model.DomainRange(1, sc)}else{dom}
          val cblsVariable = CBLSIntVarDom(m, initialValue, thedom,  id);
          //TODO: handle constant variables here.
          cblsIntMap += id -> cblsVariable;
          //Removed this test and filtered for search variables only later
          //if (!parsedVariable.isDefined) {
            variables = cblsVariable :: variables;
          //}
       case bv:BooleanVariable =>
         val dom = oscar.flatzinc.model.DomainRange(if(bv.isTrue) 1 else 0, if(bv.isFalse) 0 else 1)
          //TODO: Put this in a method! or make it deterministic as the neighbourhoods should take care of the assignments!
          val initialValue = {
                val range = (dom.min to dom.max);
                range(Random.nextInt(range.length))
              }
          val cblsVariable = CBLSIntVarDom(m, initialValue, dom,  bv.id);
          //TODO: handle constant variables here.
          cblsIntMap += bv.id -> cblsVariable;
          //Removed this test and filtered for search variables only later
          //if (!parsedVariable.isDefined) {
            variables = cblsVariable :: variables;
          //}
       
       // case _ => ()//TODO: DO something for the concrete constants?
      }
    }
    variables;
  }
  def getCBLSVarDom(v: Variable) = {
    getCBLSVar(v).asInstanceOf[CBLSIntVarDom]
  }
    implicit def getCBLSVar(v: Variable) = {
      v match {
      /*  case ConcreteConstant(_, value, _) =>
          
        cblsIntMap.get(value + "") match {
          case None =>
            val c = CBLSIntConstDom(value, m);
            cblsIntMap += value + "" -> c;
            c;
          case Some(c) => c;
        }
    */
      case v:IntegerVariable =>
        cblsIntMap.get(v.id) match {
          case None if v.isBound =>
            /*cblsIntMap.get(v.min.toString) match {
              case Some(c) => c;
              case None => {*/
            //From Gustav: All constants need to have a store, otherwise they won't have a UniqueID (from PropagationElement) and constraints will start throwing exceptions
            //JNM: I removed ",m " to avoid introducing a lot of useless "variables" in the model in the hope of making it more efficient.
            //JNM: restored the "m," as one need it to find the model sometimes.
            val c = new CBLSIntConstDom(m,v.value);
            //println("ICI "+id + " "+v.min)
            cblsIntMap += v.id -> c; //was id -> 
            c;//}}
          case Some(c) => c;
        }
      case v:BooleanVariable =>
        cblsIntMap.get(v.id) match {
          case None if v.isBound =>
            /*cblsIntMap.get(v.min.toString) match {
              case Some(c) => c;
              case None => {*/
            //From Gustav: All constants need to have a store, otherwise they won't have a UniqueID (from PropagationElement) and constraints will start throwing exceptions
            //JNM: I removed ",m " to avoid introducing a lot of useless "variables" in the model in the hope of making it more efficient.
            //JNM: restored the "m," as one need it to find the model sometimes.
            val c = new CBLSIntConstDom(m,v.intValue);
            //println("ICI "+id + " "+v.min)
            cblsIntMap += v.id -> c; //was id -> 
            c;//}}
          case Some(c) => c;
        }
      }
    }
  def handleSolution() = {
    println("% time from start: "+getWatch())
    model.solution.handleSolution(
      (s: String) => cblsIntMap.get(s) match {
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
    if(cpmodel!=null && model.search.obj != Objective.SATISFY){
      log("Calling the CP solver")
      cpmodel.updateBestObjectiveValue(getCBLSVar(model.search.variable.get).value)
    //TODO: ignore variables whose domain should not be reduced (e.g. variables in the Circuit constraint)
      cpmodel.updateModelDomains();
      updateVarDomains();
      log("Variable domains updated")
    }
  }
  var cpmodel = null.asInstanceOf[FZCPModel]
  def useCPsolver(cpm: FZCPModel){
    assert(cpm.model == model);
    cpmodel = cpm
  }
  def updateVarDomains(){
    //TODO: Make sure this is necessary,as it is not clear from which domain the moves are drawn from.
    //Might want to get rid of CBLSIntVarDom
    for(vm<-model.variables if !vm.isDefined && !vm.cstrs.exists{ 
        case c:subcircuit => true; 
        case c:circuit => true;
        case _ => false}){
      val vls = getCBLSVarDom(vm)
      vls.restrictDomain(vls.dom.min to vls.dom.max)
    }
  }
  /*
  def getSolution():String = {
    model.solution.getSolution(
      (s: String) => cblsIntMap.get(s) match {
        case Some(intVar) =>
          intVar.value + "";
        case _ => throw new Exception("Unhappy")
      });
  }*/
}
class FZCBLSSolver extends SearchEngine with StopWatch {

  
  def solve(opts: Options) {
    startWatch()
    val log = opts.log();
    log("start")
    
    val useCP = opts.is("usecp")
    
    val model = FZParser.readFlatZincModelFromFile(opts.fileName,log, false).problem;
    
    Helper.getCstrsByName(model.constraints).map{ case (n:String,l:List[Constraint]) => l.length +"\t"+n}.toList.sorted.foreach(log(_))
    
    log("Parsed. Parsing took "+getWatch+" ms")
    val cpmodel = new FZCPModel(model,oscar.cp.Strong )
    //println(model.variables.toList.map(v => v.domainSize))
    
    if(useCP){
      FZModelTransfo.propagate(model)(log);
      log("Reduced Domains before CP")
      //println(model.variables.toList.map(v => v.domainSize))
      cpmodel.createVariables()
      cpmodel.createConstraints()
      cpmodel.updateModelDomains()
      log("Reduced Domains with CP")
      //println(model.variables.toList.map(v => v.domainSize))
    }
    if(!opts.is("no-simpl")){
      //TODO: check which part of the following is still necessary after using CP for bounds reduction.
      FZModelTransfo.simplify(model)(log);
      log("Reduced Domains")
    }else{
      log("No domain reduction")
    }
    model.constraints.foreach(c => if(c.getVariables().length <=1) log("Remaining Unary Constraint "+c)
    else if(c.getVariables().filter(v => !v.isBound).length <= 1){
      log("De facto Unary Constraint "+c); 
      //log(2,c.getVariables().map(v => v.min+".."+v.max).mkString(" , "))
    })
    model.constraints.foreach{ case reif(c,b) => if(b.isBound) log("Fixed reified constraint: "+b.boolValue); case _ => {}}
    
    
    
    //Hack for the subcircuit constraints:
    model.variables.foreach(v => if(v.isDefined && v.cstrs.exists{ 
        case c:subcircuit => true; 
        case c:circuit => true;
        case c:inverse => true;
        case _ => false}) v.definingConstraint.get.unsetDefinedVar(v))    
    
    
    if(!opts.is("no-find-inv")){
      FZModelTransfo.findInvariants(model,log,List.empty[Variable]);
      log("Found Invariants")
    }else{
      log("Did not search for new invariants")
    }
    
    //
    //added this loop to remove invariants targeting a bound variable.
    //TODO: Actually, one might want to put this as part of the invariant discovery...
    //Moved this line after invariant discovery to avoid problem in Nonogram but then it is maybe useless? What was the initial purpose?
    //But this creates problems for the nonogram, where it creates another invariant that targets the input of an element and makes an out-of-bound exception
    for(c <- model.constraints ){
      if(c.definedVar.isDefined && c.definedVar.get.isBound)c.unsetDefinedVar(c.definedVar.get)
    }
    
    if(opts.is("no-post-inv")){
      for(c <- model.constraints ){
        if(c.definedVar.isDefined)c.unsetDefinedVar(c.definedVar.get)
      }
    }
    
    //Hack for the subcircuit constraints:
    model.variables.foreach(v => if(v.isDefined && v.cstrs.exists{ 
        case c:subcircuit => true; 
        case c:circuit => true;
        case c:inverse => true;
        case _ => false}){v.definingConstraint.get.unsetDefinedVar(v)})    
    
    
    
    val allcstrs:List[Constraint] = model.constraints.toList;
    val (maybedircstrs,maybesoftcstrs) = allcstrs.partition(_.definedVar.isDefined)
    log("Possibly "+maybedircstrs.length+" invariants.")
    val (invariants,removed) = FZModelTransfo.getSortedInvariants(maybedircstrs)(log)
    log("Sorted "+invariants.length+" Invariants")
    
    // Model
    val m: Store = new Store(false, None, true)//setting the last Boolean to true would avoid calling the SCC algorithm but we have to make sure that there are no SCCs in the Graph. Is it the case in the way we build it?
    // constraint system
    val cs = ConstraintSystem(m)
    val cblsmodel = new FZCBLSModel(model,cs,m,log,() => getWatch)
    if(useCP)cblsmodel.useCPsolver(cpmodel)
    log("Created Model (Variables and Objective)")
    
    
    val softorimplcstrs = maybesoftcstrs ++ removed
    val softcstrs = 
    if(!opts.is("no-impl-cstr")){
      val implicitPoster = new FZCBLSImplicitConstraints(cblsmodel)
      val (implcstrs,softcstrs) = implicitPoster.findAndPostImplicit(softorimplcstrs);
      //TODO: Add the implcstrs to some system to ensure that they are at all time respected.
      log("Found "+cblsmodel.neighbourhoodGenerator.length+" Implicit Constraints")
      Helper.getCstrsByName(implcstrs).map{ case (n:String,l:List[Constraint]) => l.length +"\t"+n}.toList.sorted.foreach(s => log(" "+s))
      
      val hardCS = ConstraintSystem(m)
      val hardPoster: FZCBLSConstraintPoster = new FZCBLSConstraintPoster(hardCS,cblsmodel.getCBLSVar);
      for(c <- implcstrs){
        try{
        hardPoster.add_constraint(c)
        } catch {
          case e: NoSuchConstraintException => log("Warning: Do not check that "+c+" is always respected.")
        }
      }
      hardCS.close()
      Event(hardCS.violation, Unit => {if(hardCS.violation.value > 0){
        log(0,"PROBLEM: Some implicit Constraint is not satisfied during search.")
        cblsmodel.neighbourhoods .foreach(n => log(0,n.getClass().toString()+" "+n.getVariables().mkString("[",",","]")))
        throw new Exception()
      }});
      //Event(cs.violation, Unit => {log(cs.violation.toString);})
      //
       
      softcstrs
    }else{
      log("Did not try to find implicit constraints")
      softorimplcstrs
    }
    
    
    val poster: FZCBLSConstraintPoster = new FZCBLSConstraintPoster(cs,cblsmodel.getCBLSVar);
    val softConstraints = softcstrs;
    for (invariant <- invariants){
      log(2,"Posting as Invariant "+invariant)
      val inv = poster.add_invariant(invariant)
      cblsmodel.cblsIntMap += invariant.definedVar.get.id -> inv;
    }
    log("Posted "+invariants.length+" Invariants")
    Helper.getCstrsByName(invariants).map{ case (n:String,l:List[Constraint]) => l.length +"\t"+n}.toList.sorted.foreach(s => log(" "+s))
    for (constraint <- softConstraints) {
      log(2,"Posting as Soft "+constraint)
      poster.add_constraint(constraint);
    }
    log("Posted "+softConstraints.length+" Soft Constraints")
    Helper.getCstrsByName(softConstraints).map{ case (n:String,l:List[Constraint]) => l.length +"\t"+n}.toList.sorted.foreach(s => log(" "+s))
    log(softConstraints.filter(c => c.getVariables().forall(v => !v.isDefined)).size+" are only on search variables.")
    log(softConstraints.filter(c => c.getVariables().forall(v => v.isDefined)).size+" are only on defined variables.")
    
    //println(implicitConstraints.length + " implicit constraints");
    //Do not want to search on such variables!
    cblsmodel.vars = cblsmodel.vars.filterNot(v => v.domainSize==1 || v.isControlledVariable);
    cblsmodel.addDefaultNeighbourhouds()
    cblsmodel.c.close()//The objective depends on the violation of the CS, so it must be first closed before creating the Objective.
    cblsmodel.objective = new FZCBLSObjective(cblsmodel,log)//But objective is needed in neighbourhoods
    cblsmodel.createNeighbourhoods()//So we actually create the neighbourhoods only after!
    cblsmodel.neighbourhoods.foreach(n => log(2,"Created Neighbourhood "+ n+ " over "+n.searchVariables.length+" variables"))
    
    
    if(cblsmodel.neighbourhoods.length==0){
      log(0,"No neighbourhood has been created. Aborting!")
      return;
    }
    log("Using "+cblsmodel.vars.length+" Search Variables in default assign neighbourhood")
    log("Using "+cblsmodel.vars.filter(v => v.min ==0 && v.max==1).length+" Search Variables in default flip neighbourhood")
    cblsmodel.vars.foreach(v => log(2,"Search with "+v+" dom: "+v.min +".."+v.max))
    log("Created all Neighborhoods")
    
    
    //Search
    val timeout = (if(opts.timeOut>0) {opts.timeOut} else 15 * 60) * 1000
    log("Timeout is set to "+timeout+" milliseconds"); 
    val sc : SearchControl =  model.search.obj match {
          case Objective.SATISFY => new SearchControl(cblsmodel,0,timeout,true);
          case Objective.MAXIMIZE => new SearchControl(cblsmodel,-model.search.variable.get.max, timeout,false);
          case Objective.MINIMIZE => new SearchControl(cblsmodel,model.search.variable.get.min, timeout,false);
        }
    //TODO: The search should print the solution if, by chance, the initial assingnment is a solution!
    val search = new Chain(
        new ActionSearch(() => {sc.cancelObjective()}),
        if(!opts.is("no-sls"))new SimpleLocalSearch(cblsmodel,sc) else new ActionSearch(() =>{}),
        //new GreedySearch(1,cblsmodel,sc),
        //new GreedySearch(2,cblsmodel,sc),
        //if(opts.is("use-cb"))new NeighbourhoodCBSearch(cblsmodel,sc) else //new ActionSearch(() =>{}),
        //new GreedySearch(3,cblsmodel,sc),
        new NeighbourhoodSearchSAT(cblsmodel,sc),
        new ActionSearch(() => {sc.restoreObjective(); /*cblsmodel.objective.violationWeight :=1500*/}),
        //new NeighbourhoodCBSearch(cblsmodel,sc),
        //new NeighbourhoodCBSearch(cblsmodel,sc),
        model.search.obj match {
          case Objective.SATISFY => new ActionSearch(() => {}) 
          case Objective.MAXIMIZE => new NeighbourhoodSearchOPT(cblsmodel,sc);
          case Objective.MINIMIZE => new NeighbourhoodSearchOPT(cblsmodel,sc);
        });
    
    log("Search created")
    m.close();
    log("Model closed");
    if(opts.is("no-run")){
      log("Not running the search...")
    }else{
      log("Starting Search at "+getWatchString)
      search.run();
      log("Done at "+getWatchString)
      if(sc.bestKnownObjective  ==Int.MaxValue ){
        log("Did not find any solution.")
        log("Smallest violation: "+sc.bestPair._1 )
        log(cblsmodel.c.violatedConstraints.length+" violated constraints")
      }else{
        log("Best Overall Solution: "+sc.bestKnownObjective * (if(model.search.obj==Objective.MAXIMIZE) -1 else 1))
      }
    }
    System.exit(0)
  }

  
}