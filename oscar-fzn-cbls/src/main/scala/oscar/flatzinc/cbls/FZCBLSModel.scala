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

import oscar.cbls.core.computation.{CBLSIntVar, IntValue, Store}
import oscar.cbls.core.constraint.ConstraintSystem
import oscar.cbls.core.objective.{Objective => CBLSObjective}
import oscar.cbls.lib.constraint.GE
import oscar.flatzinc.Log
import oscar.flatzinc.cbls.support._
import oscar.flatzinc.cp.FZCPModel
import oscar.flatzinc.model.{BooleanVariable, Objective, Variable, _}

import scala.collection.mutable.{Map => MMap}
import scala.util.Random


class FZCBLSModel(val fzModel: FZProblem, val log:Log, val getWatch: () => Long) {



  // Model
  val m: Store = new Store(false, None, true) //setting the last Boolean to true would avoid calling the SCC algorithm but we have to make sure that there are no SCCs in the Graph. Is it the case in the way we build it?
  // constraint system
  val c = ConstraintSystem(m)

  val cblsIntMap: MMap[String, IntValue] = MMap.empty[String, IntValue]
  //Create variables from FlatZinc model.
  var vars: List[CBLSIntVarDom] = createVariables()

  var neighbourhoodVars: List[CBLSIntVarDom] = List.empty

  var objective = null.asInstanceOf[FZCBLSObjective]
  var neighbourhoods: List[Neighbourhood] = List.empty[Neighbourhood]
  var neighbourhoodGenerator: List[(CBLSObjective,ConstraintSystem) => Neighbourhood] = List.empty[(CBLSObjective,ConstraintSystem) => Neighbourhood]

  var bestKnownObjective = Int.MaxValue
  def close():Unit ={
    m.close()
  }
  def closeConstraintSystem():Unit ={
    c.close()
  }

  def initiateVariableViolation() = {
    vars.foreach(c.violation(_))
    neighbourhoodVars.foreach(c.violation(_))
  }

  def removeControlledVariables(condition:(CBLSIntVarDom => Boolean)) = {
    vars = vars.filterNot(condition)
  }

  def initObjective() = {
    objective = new FZCBLSObjective(this,None,log)
  }

  def initObjectiveAndCloseConstraintSystem():Unit = {

    val objectiveVar = fzModel.search.variable.map(getCBLSVar(_)).getOrElse(null)
    val bound = fzModel.search.obj match {
      case Objective.SATISFY => None
      case Objective.MAXIMIZE => Some(CBLSIntVar(c.model, objectiveVar.min, objectiveVar.domain, "Objective bound"))
      case Objective.MINIMIZE => Some(CBLSIntVar(c.model, objectiveVar.max, objectiveVar.domain, "Objective bound"))
    }

    fzModel.search.obj match {
      case Objective.SATISFY => ()
      case Objective.MAXIMIZE => c.add(GE(objectiveVar,bound.get))
      case Objective.MINIMIZE => c.add(GE(bound.get,objectiveVar))
    }

    c.close()

    objective = new FZCBLSObjective(this,bound,log)

  }


  def addNeighbourhood(n: (CBLSObjective,ConstraintSystem) => Neighbourhood,controlledVars: Array[CBLSIntVarDom]){
    neighbourhoodGenerator = n :: neighbourhoodGenerator
    //neighbourhoods = n :: neighbourhoods
    vars = vars.filterNot(controlledVars.contains(_))
    neighbourhoodVars = neighbourhoodVars ++ controlledVars.toList
  }
  def addDefaultNeighbourhoods(){
    if (vars.length > 0) {
      //val varsToSwap = vars.groupBy(v => v.dom)
      addNeighbourhood((o,c) => new MaxViolating(vars.toArray, o, c),Array.empty[CBLSIntVarDom])

      val boolVars = vars.filter((v: CBLSIntVar) => v.min == 0 && v.max == 1)
      if (boolVars.length > 1)
        addNeighbourhood((o,c) => new MaxViolatingSwap(boolVars.toArray, o, c),Array.empty[CBLSIntVarDom])
    }
  }

  def createNeighbourhoods(){
    neighbourhoods = neighbourhoodGenerator.map(_(objective(),c))
    neighbourhoods.foreach(n => log(2,"Created Neighbourhood "+ n+ " over "+n.searchVariables.length+" variables"))
  }
  def createVariables() = {
    var variables: List[CBLSIntVarDom] = List.empty[CBLSIntVarDom];
     //Only create variables that are not fixed by an invariant.
    for (parsedVariable <- fzModel.variables if !parsedVariable.isDefined) {
      parsedVariable match {
        case IntegerVariable(id, dom,ann) =>
          //TODO: Put this in a method! or make it deterministic as the neighbourhoods should take care of the assignments!
          val initialValue = dom match {
            case oscar.flatzinc.model.DomainRange(min, max) =>
              if(max.toLong - min.toLong > Int.MaxValue) Random.nextInt()
              else{
                val range = (min to max);
                range(Random.nextInt(range.length))
              }
            case DomainSet(values) =>
              val v = values.toArray;
              v(Random.nextInt(v.length))
          }
          val sc = parsedVariable.cstrs.map{
          	case c:subcircuit => c.variables.length;
          	case c:circuit => c.variables.length;
          	case _ => 0}.fold(0)((acc, v) => Math.min(acc,v)) //Min here, right?
          val thedom = if(sc > 0){oscar.flatzinc.model.DomainRange(1, sc)}else{dom}
          val cblsVariable = CBLSIntVarDom(m, initialValue, thedom,  id);
          //TODO: handle constant variables here.
          cblsIntMap += id -> cblsVariable;
          variables = cblsVariable :: variables;

       case bv:BooleanVariable =>
         //WARNING: Changed the representation of true and false so that false is 1 and true is 0
         val dom = oscar.flatzinc.model.DomainRange(if(bv.isTrue) 0 else if(bv.isFalse) 1 else 0, if(bv.isFalse) 1 else if(bv.isTrue) 0 else 1)
          //TODO: Put this in a method! or make it deterministic as the neighbourhoods should take care of the assignments!
          val initialValue = {
                val range = (dom.min to dom.max);
                range(Random.nextInt(range.length))
              }
          val cblsVariable = CBLSIntVarDom(m, initialValue, dom,  bv.id);
          //TODO: handle constant variables here.
          cblsIntMap += bv.id -> cblsVariable;
          variables = cblsVariable :: variables;

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
      case v:IntegerVariable =>
        cblsIntMap.get(v.id) match {
          case None if v.isBound =>
            val c = new CBLSIntConstDom(m,v.value);
            cblsIntMap += v.id -> c;
            c;
          case Some(c) => c;
        }
      case v:BooleanVariable =>
        cblsIntMap.get(v.id) match {
          case None if v.isBound =>
            val c = new CBLSIntConstDom(m,v.violValue);
            cblsIntMap += v.id -> c;
            c;
          case Some(c) => c;
        }
      }
    }


  def handleSolution() = {

    log("Updating objective bound")
    log("Violation before: " + c.violation.value)
    objective.bound match {
      case Some(v) => v := objective.objectiveVar.value + (fzModel.search.obj match {
        case Objective.MAXIMIZE => 1
        case Objective.MINIMIZE => -1
        case _ => 0
      })
      case _ => ()
    }

    log("Violation after: " + c.violation.value)
    log("Objective bound updated")

    log("Found solution with objective: " + objective.getObjectiveValue())
    if(objective.getObjectiveValue() < bestKnownObjective) {
      println("% time from start: "+getWatch())
      bestKnownObjective = objective.getObjectiveValue()
      fzModel.solution.handleSolution(
        (s: String) => cblsIntMap.get(s) match {
          case Some(intVar) =>
            intVar.value + ""
          case r => if (s == "true" || s == "false") s
          else try {
            s.toInt.toString()
          } catch {
            case e: NumberFormatException => {
              throw new Exception("Unhappy: " + r + " " + s)
            }
          }
        });
    }
    if(useCP && fzModel.search.obj != Objective.SATISFY){
      log("Calling the CP solver")
      cpmodel.updateBestObjectiveValue(getCBLSVar(fzModel.search.variable.get).value)
    //TODO: ignore variables whose domain should not be reduced (e.g. variables in the Circuit constraint)
      cpmodel.updateModelDomains()
      updateVarDomains()
      log("Variable domains updated")


    }
  }
  var cpmodel = null.asInstanceOf[FZCPModel]
  var useCP = false
  def useCPsolver(cpm: FZCPModel){
    assert(cpm.model == fzModel);
    cpmodel = cpm
    useCP = true
  }
  def updateVarDomains(){
    //TODO: Make sure this is necessary,as it is not clear from which domain the moves are drawn from.
    //Might want to get rid of CBLSIntVarDom
    for(vm<-fzModel.variables if !vm.isDefined && !vm.cstrs.exists{
        case c:subcircuit => true;
        case c:circuit => true;
        case _ => false}){
      val vls = getCBLSVarDom(vm)
      vls.restrictDomain(vls.dom.min to vls.dom.max)
    }
  }
 }
