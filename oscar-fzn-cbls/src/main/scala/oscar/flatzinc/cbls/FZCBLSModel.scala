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

  var objective = null.asInstanceOf[FZCBLSObjective]
  var neighbourhoods: List[Neighbourhood] = List.empty[Neighbourhood]
  var neighbourhoodGenerator: List[(CBLSObjective,ConstraintSystem) => Neighbourhood] = List.empty[(CBLSObjective,ConstraintSystem) => Neighbourhood]

  def close():Unit ={
    m.close()
  }
  def closeConstraintSystem():Unit ={
    c.close()
  }

  def initiateVariableViolation() = {
    vars.map(c.violation(_))
  }

  def removeVariablesFromNeighbourhood(condition:(CBLSIntVarDom => Boolean)) = {
    vars = vars.filterNot(condition)
  }

  def initObjective() = {
    objective = new FZCBLSObjective(this,log)
  }


  def addNeighbourhood(n: (CBLSObjective,ConstraintSystem) => Neighbourhood,removeVars: Array[CBLSIntVarDom]){
    neighbourhoodGenerator = n :: neighbourhoodGenerator
    //neighbourhoods = n :: neighbourhoods
    vars = vars.filterNot(removeVars.contains(_))
  }
  def addDefaultNeighbourhoods(){
    if (vars.length > 0) {
      val varsToSwap = vars.groupBy(v => v.dom)
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
        case IntegerVariable(id, dom) =>
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
          	case _ => 0}.fold(0)((x, y) => if (x > y) x else y)
          val thedom = if(sc > 0){oscar.flatzinc.model.DomainRange(1, sc)}else{dom}
          val cblsVariable = CBLSIntVarDom(m, initialValue, thedom,  id);
          //TODO: handle constant variables here.
          cblsIntMap += id -> cblsVariable;
          variables = cblsVariable :: variables;

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
            val c = new CBLSIntConstDom(m,v.intValue);
            cblsIntMap += v.id -> c;
            c;
          case Some(c) => c;
        }
      }
    }


  def handleSolution() = {
    println("% time from start: "+getWatch())
    fzModel.solution.handleSolution(
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
    if(cpmodel!=null && fzModel.search.obj != Objective.SATISFY){
      log("Calling the CP solver")
      cpmodel.updateBestObjectiveValue(getCBLSVar(fzModel.search.variable.get).value)
    //TODO: ignore variables whose domain should not be reduced (e.g. variables in the Circuit constraint)
      cpmodel.updateModelDomains()
      updateVarDomains()
      log("Variable domains updated")
    }
  }
  var cpmodel = null.asInstanceOf[FZCPModel]
  def useCPsolver(cpm: FZCPModel){
    assert(cpm.model == fzModel);
    cpmodel = cpm
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
