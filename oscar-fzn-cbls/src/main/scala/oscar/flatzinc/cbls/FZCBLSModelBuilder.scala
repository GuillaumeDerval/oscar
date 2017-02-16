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

import oscar.cbls.core.computation._
import oscar.cbls.core.constraint.ConstraintSystem
import oscar.cbls.core.objective.{Objective => CBLSObjective}
import oscar.cbls.lib.search.LinearSelector
import oscar.cbls.util.StopWatch
import oscar.flatzinc.{Log, NoSuchConstraintException, Options}
import oscar.flatzinc.cbls.support._
import oscar.flatzinc.cp.FZCPModel
import oscar.flatzinc.model.{Constraint, Variable, _}
import oscar.flatzinc.parser.FZParser
import oscar.flatzinc.transfo.FZModelTransfo

import scala.collection.mutable.{SortedSet, Map => MMap}



class FZCBLSModelBuilder extends LinearSelector with StopWatch {


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

    simplify(opts, log, useCP, model, cpmodel)


    //Hack for the subcircuit constraints:
    model.variables.foreach(v => if(v.isDefined && v.cstrs.exists{
        case c:subcircuit => true;
        case c:circuit => true;
        case c:inverse => true;
        case _ => false}) v.definingConstraint.get.unsetDefinedVar(v))


    findInvariants(model, opts, log)

    //Hack for the subcircuit constraints:
    model.variables.foreach(v => if(v.isDefined && v.cstrs.exists{
        case c:subcircuit => true;
        case c:circuit => true;
        case c:inverse => true;
        case _ => false}){v.definingConstraint.get.unsetDefinedVar(v)})



    val allConstraints:List[Constraint] = model.constraints.toList

    val (maybeDirConstraint,maybeSoftConstraint) = allConstraints.partition(_.definedVar.isDefined)
    log("Possibly "+maybeDirConstraint.length+" invariants.")
    val (invariants,removed) = FZModelTransfo.getSortedInvariants(maybeDirConstraint)(log)
    log("Sorted "+invariants.length+" Invariants")

    // Model
    val m: Store = new Store(false, None, true)//setting the last Boolean to true would avoid calling the SCC algorithm but we have to make sure that there are no SCCs in the Graph. Is it the case in the way we build it?
    // constraint system
    val cs = ConstraintSystem(m)
    val cblsmodel = new FZCBLSModel(model,cs,m,log,() => getWatch)

    if(useCP)cblsmodel.useCPsolver(cpmodel)
    log("Created Model (Variables and Objective)")


    val softConstraints: List[Constraint] = findAndPostImplicitConstraints(cblsmodel, m, maybeSoftConstraint, removed, opts, log)


    val poster: FZCBLSConstraintPoster = new FZCBLSConstraintPoster(cs,cblsmodel.getCBLSVar)

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
    cblsmodel.addDefaultNeighbourhoods()


    //Create variable violation before closing the constraint system!
    cblsmodel.vars.map(cblsmodel.c.violation(_))

    cblsmodel.c.close()//The objective depends on the violation of the CS, so it must be first closed before creating the Objective.
    cblsmodel.objective = new FZCBLSObjective(cblsmodel,log)//But objective is needed in neighbourhoods


    cblsmodel.createNeighbourhoods()//So we actually create the neighbourhoods only after!
    cblsmodel.neighbourhoods.foreach(n => log(2,"Created Neighbourhood "+ n+ " over "+n.searchVariables.length+" variables"))
    
    
    if(cblsmodel.neighbourhoods.length==0){
      log(0,"No neighbourhood has been created. Aborting!")
      return
    }
    log("Using "+cblsmodel.vars.length+" Search Variables in default assign neighbourhood")
    log("Using "+cblsmodel.vars.filter(v => v.min ==0 && v.max==1).length+" Search Variables in default flip neighbourhood")
    cblsmodel.vars.foreach(v => log(2,"Search with "+v+" dom: "+v.min +".."+v.max))
    log("Created all Neighborhoods")

    //Search
    val timeout = (if(opts.timeOut>0) {opts.timeOut} else 20 * 60) * 1000
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
        new NeighbourhoodSearchSAT(cblsmodel,sc),
        new ActionSearch(() => {sc.restoreObjective()}),
        model.search.obj match {
          case Objective.SATISFY => new ActionSearch(() => {}) 
          case Objective.MAXIMIZE => new NeighbourhoodSearchOPT(cblsmodel,sc)
          case Objective.MINIMIZE => new NeighbourhoodSearchOPT(cblsmodel,sc); //new NeighbourhoodSearchOPTbySAT(cblsmodel,sc)//
        });
    
    log("Search created")
    m.close();
    log("Model closed");
    if(opts.is("no-run")){
      log("Not running the search...")
    }else{
      log("Starting Search at "+getWatchString)
      if(cblsmodel.c.violatedConstraints.length == 0 && model.search.obj == Objective.SATISFY  ){
        cblsmodel.handleSolution()
      }else {
        search.run()
      }
      log("Done at "+getWatchString)
      if(sc.bestKnownObjective  == Int.MaxValue && cblsmodel.c.violatedConstraints.length > 0 ){
        log("Did not find any solution.")
        log("Smallest violation: "+sc.bestPair._1 )
        log(cblsmodel.c.violatedConstraints.length+" violated constraints")
      }else{
        log("Best Overall Solution: "+sc.bestKnownObjective * (if(model.search.obj==Objective.MAXIMIZE) -1 else 1))
      }
    }
    System.exit(0)
  }


  def findAndPostImplicitConstraints(cblsmodel: FZCBLSModel, m: Store, maybeSoftConstraint: List[Constraint], removed: List[Constraint], opts: Options, log: Log): List[Constraint] = {
    val softOrImplicitConstraint = maybeSoftConstraint ++ removed
    if (opts.is("no-impl-cstr")) {
      log("Did not try to find implicit constraints")
      softOrImplicitConstraint
    } else {
      val implicitPoster = new FZCBLSImplicitConstraints(cblsmodel)
      //Implicit constraints are posted in this metod call.
      val (implicitConstraints, softConstraints) = implicitPoster.findAndPostImplicit(softOrImplicitConstraint)
      //TODO: Add the implicitConstraints to some system to ensure that they are at all time respected.
      log("Found " + cblsmodel.neighbourhoodGenerator.length + " Implicit Constraints")
      Helper.getCstrsByName(implicitConstraints).map { case (n: String, l: List[Constraint]) => l.length + "\t" + n }.toList.sorted.foreach(s => log(" " + s))

      val hardCS = ConstraintSystem(m)
      val hardPoster: FZCBLSConstraintPoster = new FZCBLSConstraintPoster(hardCS, cblsmodel.getCBLSVar);
      for (c <- implicitConstraints) {
        try {
          hardPoster.add_constraint(c)
        } catch {
          case e: NoSuchConstraintException => log("Warning: Do not check that " + c + " is always respected.")
        }
      }
      hardCS.close()
      Event(hardCS.violation, Unit => {
        if (hardCS.violation.value > 0) {
          log(0, "PROBLEM: Some implicit Constraint is not satisfied during search.")
          cblsmodel.neighbourhoods.foreach(n => log(0, n.getClass().toString() + " " + n.getVariables().mkString("[", ",", "]")))
          throw new Exception()
        }
      });
      //Event(cs.violation, Unit => {log(cs.violation.toString);})
      //
      softConstraints
    }
  }

  def findInvariants(model: FZProblem, opts: Options, log: Log): Unit = {
    if (opts.is("no-find-inv")) {
      log("Did not search for new invariants")
    } else {
      FZModelTransfo.findInvariants(model, log, List.empty[Variable]);
      log("Found Invariants")
    }
    //If a variable has a domiain of size 1, then do not define it with an invariant.
    //This should be redundant as FZModelTransfo.findInvariants does not consider bound variables.
    for (c <- model.constraints) {
      if (c.definedVar.isDefined && c.definedVar.get.isBound)
        c.unsetDefinedVar(c.definedVar.get)
    }

    if (opts.is("no-post-inv")) {
      for (c <- model.constraints) {
        if (c.definedVar.isDefined)
          c.unsetDefinedVar(c.definedVar.get)
      }
    }
  }

  private def simplify(opts: Options, log: Log, useCP: Boolean, model: FZProblem, cpmodel: FZCPModel): Unit = {
    if (useCP) {
      FZModelTransfo.propagate(model)(log);
      log("Reduced Domains before CP")
      //println(model.variables.toList.map(v => v.domainSize))
      cpmodel.createVariables()
      cpmodel.createConstraints()
      cpmodel.updateModelDomains()
      log("Reduced Domains with CP")
      //println(model.variables.toList.map(v => v.domainSize))
    }
    if (opts.is("no-simpl")) {
      log("No domain reduction")
    } else {
      //TODO: check which part of the following is still necessary after using CP for bounds reduction.
      FZModelTransfo.simplify(model)(log);
      log("Reduced Domains")
    }
    model.constraints.foreach(c => if (c.getVariables().length <= 1) log("Remaining Unary Constraint " + c)
      else if (c.getVariables().filter(v => !v.isBound).length <= 1) {
        log("De facto Unary Constraint " + c);
        //log(2,c.getVariables().map(v => v.min+".."+v.max).mkString(" , "))
      })
    model.constraints.foreach { case reif(c, b) => if (b.isBound) log("Fixed reified constraint: " + b.boolValue); case _ => {} }
  }
}

