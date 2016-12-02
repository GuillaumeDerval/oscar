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
 * *****************************************************************************/

package oscar.cp.xcsp

import oscar.cp.core.variables.CPBoolVar
import oscar.cp.core.variables.CPIntVar
import oscar.cp.core.Constraint
import oscar.cp.xcsp.ast.AlternativeOperator
import oscar.cp.xcsp.ast.BooleanBinaryOperator
import oscar.cp.xcsp.ast.BooleanExpr
import oscar.cp.xcsp.ast.BooleanOperatorOnIntegers
import oscar.cp.xcsp.ast.BooleanValue
import oscar.cp.xcsp.ast.IntOperatorOnIntegers
import oscar.cp.xcsp.ast.IntUnaryOperator
import oscar.cp.xcsp.ast.IntegerExpr
import oscar.cp.xcsp.ast.IntegerFormalParameter
import oscar.cp.xcsp.ast.IntegerValue
import oscar.cp.xcsp.ast.Not
import oscar.cp._
import oscar.cp.xcsp.modeling.DefaultConstraints
import java.io.File

import oscar.algo.Inconsistency
import oscar.cp.xcsp.ast.FunctionalPredicateParser
import oscar.cp.xcsp.ast.ParameterParser
import oscar.cp.xcsp.ast.EffectiveIntegerValue
import oscar.cp.xcsp.ast.IntegerVariable
import oscar.cp.xcsp.ast.Nil
import oscar.cp.xcsp.ast.EffectiveParameter

abstract class XCSPSolver {
 
  //----------------------------- abstract global constraints-------------------------------------
  
 def table(x: Array[CPIntVar], tuples: Array[Array[Int]]) : Constraint
 def tableNe(x: Array[CPIntVar], tuples: Array[Array[Int]]) : Constraint = oscar.cp.table(x,tuples)
 def allDifferent(vars: Iterable[CPIntVar]) : Constraint
 def weightedSum(w: Array[Int], x: Array[CPIntVar], y: Int) : Constraint
 def among(n: CPIntVar, x: IndexedSeq[CPIntVar], s: Set[Int]) : Constraint
 def atLeast(n: Int, x: IndexedSeq[CPIntVar], v: Int) : Constraint
 def atMost(n: Int, x: IndexedSeq[CPIntVar], v: Int) : Constraint
 def cumulative(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar], demands: Array[CPIntVar], capacity: CPIntVar) : Constraint
 def disjunctive(starts: Array[CPIntVar], durations: Array[CPIntVar]) : Constraint
 def element(tab: IndexedSeq[CPIntVar], x: CPIntVar, z: CPIntVar) : Constraint
 def globalCardinality(x: Array[CPIntVar], valueOccurrence: Array[(Int,CPIntVar)]) : Constraint
 def minimumWeightAllDifferent(x: Array[CPIntVar], weights: Array[Array[Int]], cost: CPIntVar) : Constraint

 //-----------------------------------------------------------------
 
 def model(file : File) : (CPSolver,Array[CPIntVar]) = {
  val (domains, variables, relations, predicates, constraints) = XCSPParser.parse(file)
  model(domains, variables, relations, predicates, constraints)
 }
 
 def model(str : String) : (CPSolver,Array[CPIntVar]) = {
  val (domains, variables, relations, predicates, constraints) = XCSPParser.parse(str)
  model(domains, variables, relations, predicates, constraints)
 }
 
 protected def model(domains : Map[String,Set[Int]], variables : Map[String,String],
     relations : Option[Map[String,(String,Array[Array[Int]])]], predicates : Option[Map[String,(Array[(String,String)],Option[FunctionalPredicateParser#ParseResult[BooleanExpr]])]],
     constraints : Map[String,(Array[String],String,Option[String])]) : (CPSolver,Array[CPIntVar]) = {  
  
  implicit val cp = CPSolver()
  
  implicit val decisionVariables = variables map {variable => 
   val name = variable._1 
   (name -> CPIntVar(domains(variable._2), name))
  } toMap

  try {
   for ((constraintName, constraint) <- constraints) {
    val (scope, reference, parametersOption) = constraint
    parametersOption match {
     case None => {
      if (relations != None && relations.get.contains(reference)) {
       //constraint in extension
       val relation = relations.get(reference)
       if (relation._1 == "supports") add(table(scope map {
        decisionVariables(_)
       }, relation._2))
       else if (relation._1 == "conflicts") add(tableNe(scope map {
        decisionVariables(_)
       }, relation._2)) // modified: added constraint negative
       else throw new RuntimeException(relation._1 + " tables are not supported.")
      }
      else if (reference == "global:allDifferent") //deprecated implicit parameters for allDiff is tolerated
       allDifferentHelper(scope, "[" + scope.reduce(_ + " " + _) + "]")
      else
       throw new RuntimeException(constraintName + " : Implicit parameters are deprecated since XCSP 2.1 and therefore are non-supported.")
     }
     case Some(parameters) => {
      //constraint in intension or global
      XCSPParser.globalPrefix findFirstIn reference match {
       case Some(_) => {
        // global constraint
        imposeGlobalConstraint(reference, scope, parameters)
       }
       case None => {
        //constraint in intension
        val predicateInfos = predicates.get(reference)
        val formalParameters = predicateInfos._1 map (_._2) toArray
        val predicateFunction = predicateInfos._2.get getOrElse (throw new RuntimeException("Only functional expressions are handled for now."))
        implicit val parameterMap = formalToEffectiveParametersMap(formalParameters, parameters split (" "))
        add(booleanExpression(predicateFunction))
       }
      }
     }
    }
   }
  }
  catch {
   case _: Inconsistency => cp.fail()
  }
  (cp, decisionVariables.values toArray)
 }
 
 // -------------------- constraint in intension helper functions----------------------------

  protected def formalToEffectiveParametersMap(formalParameters : Array[String], parameters : Array[String]) = formalParameters.zip(parameters).toMap

  protected def integerExpression(value : IntegerExpr)(implicit cp : CPSolver, formalToEffective : Map[String,String], decisionVariables : Map[String,CPIntVar]) : CPIntVar = value match {
  case IntegerValue(v) => CPIntVar(v)
  case IntegerFormalParameter(p) => {
   val effectiveParam = formalToEffective(p)
   decisionVariables.getOrElse(effectiveParam,CPIntVar(effectiveParam toInt)) 
  }
  case IntUnaryOperator(name,operand) => name match {
   case "neg" => -integerExpression(operand)  
   case "abs" => integerExpression(operand).abs
  }  
  case IntOperatorOnIntegers(name,left,right) => name match {
   case "add" => integerExpression(left) + integerExpression(right)
   case "sub" => integerExpression(left) - integerExpression(right)
   case "mul" => integerExpression(left) * integerExpression(right)
   case "div" => throw new RuntimeException("div operator does not exist for now.")
   case "mod" => throw new RuntimeException("mod operator is not supported yet.") //TODO
   case "pow" => throw new RuntimeException("pow operator does not exist for now.")
   case "min" => throw new RuntimeException("min operator does not exist for now.")
   case "max" => throw new RuntimeException("max operator does not exist for now.")
  }
  case AlternativeOperator(condition,consequence,alternative) => throw new RuntimeException("if operator does not exist for now.") 
 }

  protected def booleanExpression(value : BooleanExpr)(implicit cp : CPSolver, formalToEffective : Map[String,String], decisionVariables : Map[String,CPIntVar]) : CPBoolVar = value match {
  case BooleanValue(v) => CPBoolVar(v)
  case Not(operand) => !booleanExpression(operand)
  case BooleanBinaryOperator(name,left,right) => name match {
   case "and" => booleanExpression(left) & booleanExpression(right)
   case "or" => booleanExpression(left) | booleanExpression(right)
   case "xor" => (booleanExpression(left) | booleanExpression(right)) & !((booleanExpression(left) & booleanExpression(right)) | (!booleanExpression(left) & !booleanExpression(right)))
   case "iff" => (booleanExpression(left) ==> booleanExpression(right)) & (booleanExpression(right) ==> booleanExpression(left)) 
  }
  case BooleanOperatorOnIntegers(name,left,right) => name match {
   case "eq" => integerExpression(left) ?=== integerExpression(right)
   case "ne" => integerExpression(left) ?!== integerExpression(right)
   case "ge" => integerExpression(left) ?>= integerExpression(right)
   case "gt" => integerExpression(left) ?> integerExpression(right)
   case "le" => integerExpression(left) ?<= integerExpression(right)
   case "lt" => integerExpression(left) ?< integerExpression(right)
  }
 }
 
 
 //------------------------ global constraints helper functions---------------------------

  protected implicit def effectiveParameterToCPIntVar(parameter : EffectiveParameter)(implicit cp : CPSolver, decisionVariables : Map[String,CPIntVar]) = {
   parameter match {
     case p : IntegerVariable => decisionVariables(p.name)
     case p : EffectiveIntegerValue => CPIntVar(p.value)
   }
 }

  protected implicit def iterableEffectiveParametersToIterableCPIntVar(parameters : Iterable[EffectiveParameter])(implicit cp : CPSolver, decisionVariables : Map[String,CPIntVar]) = {
   parameters map(effectiveParameterToCPIntVar(_))
 }

  protected implicit def effectiveParametersArrayToCPIntVarArray(parameters : Array[EffectiveParameter])(implicit cp : CPSolver, decisionVariables : Map[String,CPIntVar]) = {
   parameters map(effectiveParameterToCPIntVar(_))
 }

  protected implicit def effectiveParametersIndexedSeqToCPIntVarIndexedSeq(parameters : IndexedSeq[EffectiveParameter])(implicit cp : CPSolver, decisionVariables : Map[String,CPIntVar]) = {
   parameters map(effectiveParameterToCPIntVar(_))
 }

  protected implicit def effectiveParametersListToCPIntVarList(parameters : List[EffectiveParameter])(implicit cp : CPSolver, decisionVariables : Map[String,CPIntVar]) = {
   parameters map(effectiveParameterToCPIntVar(_))
 }

  protected def imposeGlobalConstraint(constraintName : String, scope : Array[String], parameters : String)(implicit cp : CPSolver, decisionVariables : Map[String,CPIntVar]) = {
   (constraintName.replaceFirst(XCSPParser.globalPrefix.regex,"")) match {
     case "weightedSum" => weightedSumHelper(scope,parameters)
     case "allDifferent" => allDifferentHelper(scope,parameters)
     case "among" => amongHelper(scope,parameters)
     case "atleast" => atleastHelper(scope,parameters)
     case "atmost" => atmostHelper(scope,parameters)
     case "cumulative" => cumulativeHelper(scope, parameters)
     case "disjunctive" => disjunctiveHelper(scope, parameters)
     case "element" => elementHelper(scope, parameters)
     case "global_cardinality" => globalCardinalityHelper(scope, parameters)
     case "minimum_weight_all_different" => miminumWeightAllDifferentHelper(scope, parameters)
     case name : String => throw new RuntimeException("Global constraint " + name + "is not allowed.")
   }
 }

  protected def allDifferentHelper(scope : Array[String], parameters : String)(implicit cp : CPSolver, decisionVariables : Map[String,CPIntVar]) : Unit = {
   val parser = new ParameterParser(scope)
   post(allDifferent(parser.parseAll(parser.parameterList,parameters).get))
 }

  protected def weightedSumHelper(scope : Array[String], parameters : String)(implicit cp : CPSolver, decisionVariables : Map[String,CPIntVar]) : Unit = {
   val parser = new ParameterParser(scope)
   val (dic,op,v) = parser.parseAll(parser.weightedSumParameters,parameters).get
   if(op != "<eq/>")
     throw new RuntimeException("weightedSum with an operator different from equality is not supported.")
   val w = dic.keys.map(_.value) toArray
   val x = dic.values.map(v => decisionVariables(v.name)) toArray
   val y = v.value
   post(weightedSum(w,x,y))
 }

  protected def amongHelper(scope : Array[String], parameters : String)(implicit cp : CPSolver, decisionVariables : Map[String,CPIntVar]) : Unit = {
   val parser = new ParameterParser(scope)
   val (v,vL,iL) = parser.parseAll(parser.amongParameters,parameters).get
   val n = decisionVariables(v.name)
   val x = vL.map(v=> decisionVariables(v.name)).toIndexedSeq
   val y = iL.map(i=>i.value).toSet
   post(among(n,x,y))
 }

  protected def atleastHelper(scope : Array[String], parameters : String)(implicit cp : CPSolver, decisionVariables : Map[String,CPIntVar]) : Unit = {
   val parser = new ParameterParser(scope)
   val (i1,vL,i2) = parser.parseAll(parser.atLeastParameters,parameters).get
   val n = i1.value 
   val x = vL.map(v=> decisionVariables(v.name)).toIndexedSeq
   val v = i2.value 
   post(atLeast(n,x,v))
 }

  protected def atmostHelper(scope : Array[String], parameters : String)(implicit cp : CPSolver, decisionVariables : Map[String,CPIntVar]) : Unit = {
   val parser = new ParameterParser(scope)
   val (i1,vL,i2) = parser.parseAll(parser.atMostParameters,parameters).get
   val n = i1.value 
   val x = vL.map(v=> decisionVariables(v.name)).toIndexedSeq
   val v = i2.value 
   post(atMost(n,x,v))
 }

  protected def cumulativeHelper(scope : Array[String], parameters : String)(implicit cp : CPSolver, decisionVariables : Map[String,CPIntVar]) : Unit = {
   val parser = new ParameterParser(scope)
   val (taskList,height) = parser.parseAll(parser.cumulativeParameters,parameters).get
   val variables = taskList.map { task =>
     val (start,duration,end,demand) = task
     (start,duration,end) match {
     	case (s : Nil, d : EffectiveParameter, e : EffectiveParameter) => (effectiveParameterToCPIntVar(e) - effectiveParameterToCPIntVar(d) : CPIntVar,d : CPIntVar ,e : CPIntVar,demand : CPIntVar)
     	case (s : EffectiveParameter, d : Nil, e : EffectiveParameter) => (s : CPIntVar,effectiveParameterToCPIntVar(e)-effectiveParameterToCPIntVar(s) : CPIntVar ,e: CPIntVar ,demand : CPIntVar)
     	case (s : EffectiveParameter, d : EffectiveParameter, e : Nil) => (s : CPIntVar,d : CPIntVar,effectiveParameterToCPIntVar(s)+effectiveParameterToCPIntVar(d) : CPIntVar,demand : CPIntVar)
     	case (s : EffectiveParameter, d : EffectiveParameter, e : EffectiveParameter) => (s: CPIntVar,d: CPIntVar,e: CPIntVar,demand: CPIntVar)
     	case (_,_,_) => throw new RuntimeException("One or less undefined parameter (<nil/>) is allowed.")
     }	
   }
   post(cumulative(variables.map(_._1).toArray,variables.map(_._2).toArray,variables.map(_._3).toArray,variables.map(_._4).toArray,CPIntVar(height.value)))
 }

  protected def disjunctiveHelper(scope : Array[String], parameters : String)(implicit cp : CPSolver, decisionVariables : Map[String,CPIntVar]) : Unit = {
   val parser = new ParameterParser(scope)
   val taskList = parser.parseAll(parser.disjunctiveParameters,parameters).get
   val starts = taskList.map(_._1).toArray
   val durations = taskList.map(_._2).toArray
   post(disjunctive(starts,durations))
 }


  protected def elementHelper(scope : Array[String], parameters : String)(implicit cp : CPSolver, decisionVariables : Map[String,CPIntVar]) : Unit = {
   val parser = new ParameterParser(scope)
   val (index , table, value ) = parser.parseAll(parser.elementParameters,parameters).get
   //index -1 is mandatory as index is going from 1 to |table| and not from 0 to |table| - 1
   post(element(table.toIndexedSeq, effectiveParameterToCPIntVar(index) - 1,value))
 }
 
 protected def globalCardinalityHelper(scope : Array[String], parameters : String)(implicit cp : CPSolver, decisionVariables : Map[String,CPIntVar]) : Unit = {
   val parser = new ParameterParser(scope)
   val (variables , values ) = parser.parseAll(parser.globalCardinalityParameters,parameters).get
   post(globalCardinality(variables.map(v => decisionVariables(v.name)).toArray, values.map(e => (e._1.value , decisionVariables(e._2.name))).toArray))
 }
 
 protected def miminumWeightAllDifferentHelper(scope : Array[String], parameters : String)(implicit cp : CPSolver, decisionVariables : Map[String,CPIntVar]) : Unit = {
   val parser = new ParameterParser(scope)
   val (variables , matrix, cost) = parser.parseAll(parser.minimumWeightAllDifferentParameters,parameters).get
   val matrixArray = Array.fill(variables.length)(Array.fill(variables.length)(0))
   for((i,j,c) <- matrix)
     matrixArray(i.value-1)(j.value-1)=c.value //matrices indices begin at 1, not 0
   post(minimumWeightAllDifferent(variables.map(v => decisionVariables(v.name)).toArray,matrixArray,cost))
 }
 // ------------------------------------------------------------------------------------------
 
}

object XCSPSolverDefault {
  def apply() = new XCSPSolver() with DefaultConstraints
}
