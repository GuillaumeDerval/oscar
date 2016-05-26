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

import scala.xml.XML
import oscar.cp.xcsp.ast.FunctionalPredicateParser
import scala.xml.Elem
import oscar.cp.xcsp.ast.BooleanExpr
import java.io.File

object XCSPParser {

  val globalPrefix = "global:".r
  val operators = Seq("<eq/>","<ge/>","<ne/>","<gt/>","<le/>","<lt/>")
 
  def parse(file : File) = {
    val instance = XML.loadFile(file)
    checkFormat(instance)
    (parseDomain(instance), parseVariables(instance),parseRelations(instance), parsePredicates(instance), parseConstraints(instance))
  }
  
  def parse(str : String) = {
   val instance = XML.loadString(str)
   checkFormat(instance)
   (parseDomain(instance), parseVariables(instance),parseRelations(instance), parsePredicates(instance), parseConstraints(instance))
  }

  private def checkFormat(instance: Elem) = {
    val format : String = (instance \ "presentation" \ "@format").text
    if(format != "XCSP 2.0" && format != "XCSP 2.1" )
     throw new RuntimeException("Only XCSP 2.0 or 2.1 format is supported.")
  }
  private def parseDomain(instance: Elem) = {
   
   val domainsXML = instance \ "domains" 
   val nbDomains = (domainsXML \ "@nbDomains").text.toInt
   val domains = domainsXML \ "domain"
  
   val domainsMap = domains map { domain =>
   val rawDomainValues = domain.text.split(" ") //split domain parts
   val rangesList = rawDomainValues map { dV : String => //transform each domain part in set of ints
    dV.split("\\.\\.") match {
     case Array(min,max) => {
      min.toInt to max.toInt //this part is a range
     }
     case Array(value) => (value.toInt to value.toInt) //this part is one value
    }
   }
   val domainValues = rangesList map {_.toSet } reduce { _ ++ _} //merge
   ((domain \ "@name").text -> domainValues )
   } toMap
   
  assert(domainsMap.size == nbDomains)
  domainsMap
  }
  
  private def parseVariables(instance : Elem) = {
   val variablesXML = instance \ "variables"
   val nbVariables = (variablesXML \ "@nbVariables").text.toInt
   val variables = variablesXML \ "variable"
   val variablesMap = variables map { variable => ((variable \ "@name").text -> (variable \ "@domain").text) } toMap  
   
   assert(variablesMap.size == nbVariables)
   variablesMap
  }
  
  private def parseRelations(instance : Elem) = {
   val relationsXML = instance \ "relations"
   var relationsMap : Option[Map[String,Tuple2[String,Array[Array[Int]]]]]= None
   if(relationsXML.text != "") { //there are constraints in extension
    val nbRelations = (relationsXML \ "@nbRelations").text.toInt
    val relations = relationsXML \ "relation"
    val relationsMapValue = relations map { relation => 
     val arity = (relation \ "@arity").text.toInt
     val rawTuples = relation.text.split("\\|")
     val tuples = rawTuples map {_.split(" ") map {_.toInt}}
     assert(tuples forall {_.size == arity})
     ((relation \ "@name").text -> ((relation \ "@semantics").text, tuples))
    } toMap
  
    relationsMap = Some(relationsMapValue)
    assert(relationsMap.get.size == nbRelations)
   }
   relationsMap
  }
  
  private def parsePredicates(instance : Elem) = {
   val predicatesXML = instance \ "predicates"
   var predicatesMap : Option[Map[String,Tuple2[Array[(String,String)],Option[FunctionalPredicateParser#ParseResult[BooleanExpr]]]]] = None
  
   if(predicatesXML.text != "") { //there are constraints in intension
    val predicates = predicatesXML \ "predicate"
    val nbPredicates = (predicatesXML \ "@nbPredicates").text.toInt
    val predicatesMapValue = predicates map { predicate =>
     val parameterArray = (predicate \ "parameters").text split(" ") filter(_ != "") grouped(2) toArray
     val parameters = parameterArray map {t=>(t(0),t(1))} //(type, formal parameter name)
     val functionalExpressionXML = predicate \ "expression" \ "functional"
     val functionalExpression =  if(functionalExpressionXML.text != "") 
      Some(FunctionalPredicateParser.parseExpression(parameters map {_._2} toSeq, functionalExpressionXML.text))
      else
    	None  
      if (functionalExpression == None) throw new RuntimeException("Only functional expressions are handled for now.")
      ((predicate \ "@name").text-> (parameters,functionalExpression))
    } toMap
    
    predicatesMap = Some(predicatesMapValue)
    
    assert(predicatesMap.get.size == nbPredicates)
   }
   predicatesMap
  }
  
  private def parseConstraints(instance : Elem) = {
   val constraintsXML = instance \ "constraints"
   val nbConstraints = (constraintsXML \ "@nbConstraints").text.toInt
   val constraints = constraintsXML \ "constraint" 
 
   val constraintsMap = constraints map { constraint =>
   	val arity = (constraint \ "@arity").text.toInt
   	val scope = (constraint \ "@scope").text split(" ")
   	assert(scope.size == arity)
   	val reference = (constraint \ "@reference").text
   	var parameters : Option[String] = None
   	val parametersXML = constraint \ "parameters"
   	val rawParameters = parametersXML.toString
   	if(rawParameters != "") { //the constraint is in intension or global
   	  parameters = Some(rawParameters.replace("<parameters>", "").replace("</parameters>", ""))
   	}
   	((constraint \ "@name").text -> (scope,reference,parameters))
   } toMap  

   assert(constraintsMap.size == nbConstraints)
   constraintsMap
  }
}
