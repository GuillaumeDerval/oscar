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
 * @author Jean-Noël Monette
 */
package oscar.flatzinc.model

import oscar.flatzinc.parser.Model
import java.io.PrintStream
import oscar.flatzinc.parser.intermediatemodel.Element
import oscar.flatzinc.parser.intermediatemodel.ArrayOfElement
import scala.collection.JavaConverters._
import java.util.ArrayList
import oscar.flatzinc.parser.intermediatemodel.Type
import java.io.PrintWriter

object FlatZincPrinter {

  def outputModelTo(model:Model, out:PrintWriter){
    val dico = model.dico
    //No Parameters (all inlined) TODO
    //Variables
    for((id,e) <- dico){
      e.value match {
        case v:Variable =>{
          //TODO: Make sure that aliases come after the corresponding variable... WHICH ALIASES?
          val s = StringBuilder
          v match{
            case IntegerVariable(i,d) => out.println("var "+toFZN(d)+": "+ id +toFZNann(model.dicoAnnot(id))+";");
            case BooleanVariable(i,v) => out.println("var bool: "+ id +toFZNann(model.dicoAnnot(id))+{if(v.isDefined) " = "+v.get.toString() else ""}+";");
          }
        }
        case _ => {}
      }
    }
    for((id,e) <- dico){//.filter(_._2.isInstanceOf[ArrayOfElement])){
      e match {
        case e:ArrayOfElement if(e.typ.isVar && model.isOutputArray(id)) =>{//TODO: Should reuse more arrays, and avoid printing the useless ones
          //println(id+" "+e)
          val a = e.elements
          out.println("array[1.."+a.size()+"] of var "+Type.typeName(e.typ.typ)+": "+id+toFZNann(model.dicoAnnot(id))+" = "+toFZN(a)+";")
          //println("array[1.."+a.size()+"] of var int: "+id+toFZNann(model.dicoAnnot(id))+"= "+toFZN(a)+";")
        }
        case _ => {}
      }
    }
    for(c <- model.problem.constraints){
      out.println("constraint "+toFZN(c)+";")
    }
    out.println(toFZN(model.problem.search))
  }
  
  def toFZN(s: Search): String = {
    "solve "+toFZNann(s.anns)+" "+{
      s.obj match{
        case Objective.SATISFY => "satisfy"
        case Objective.MAXIMIZE => "maximize "+s.variable.get.id
        case Objective.MINIMIZE => "minimize "+s.variable.get.id
      }
    }+";"
  }
  //TODO: Element should stay in the intermediate model. Annotations should be transformed before that.
  def toFZN(e: Element): String = {
    if(e.value!=null){
      toFZN(e.value)
    }else{
      e.name
    }
  }
  def toFZN(d: Domain): String = {
    d match{
      case DomainRange(min,max) => if(min==Integer.MIN_VALUE && max==Integer.MAX_VALUE) "int" else min+".."+max
      case DomainSet(set) => set.mkString("{", ", ", "}")
    }
  }
  def toFZN(ann: Annotation): String = {
    ann.name+{if(!ann.args.isEmpty) ann.args.map(toFZN(_)).mkString("(", ",", ")") else ""}
  }
  def toFZN(anns: Iterable[Annotation]): String = {
    anns.map(toFZN(_)).mkString(" ")
  }
  def toFZNann(anns: Iterable[Annotation]): String = {
    if(anns.isEmpty) "" else
    anns.map(toFZN(_)).mkString(" ::"," ::","")
  }
  def toFZN(vs: Array[Variable]): String = {
    vs.map(toFZN(_)).mkString("[",",","]")
  }
  
  def toFZN(a: Any): String = {
    a match{
      case d: Domain => toFZN(d)
      case ann: Annotation => toFZN(ann)
      case anns: Iterable[Annotation] => toFZN(anns)
      case s:String => s
      case c:Constraint => toFZN(c)
      case v:Variable => v.id
      case es: ArrayOfElement => es.elements.asScala.map(toFZN(_)).mkString("[",",","]") //In annotations
      case vs: Array[Variable] => toFZN(vs) //arrays of variables.
      //case vs: ArrayList[VarRef] => println(vs); ""//vs.asScala.map(toFZN(_)).mkString("[",", ","]")
      case vs: ArrayList[Element] => vs.asScala.map(toFZN(_)).mkString("[",",","]")
      case e: Element => toFZN(e)
      case i: Integer => i.toString()
      case b: Boolean => b.toString()
      case x => Console.err.println("Match not found:"+x.getClass()); x.toString()
      }
  }
  
  def toFZN(c: Constraint): String = {
    c match{
      case int_eq(x,y,ann) => "int_eq("+toFZN(x)+","+toFZN(y)+")"+toFZNann(ann)
      case int_le(x,y,ann) => "int_le("+toFZN(x)+","+toFZN(y)+")"+toFZNann(ann)
      case int_ne(x,y,ann) => "int_ne("+toFZN(x)+","+toFZN(y)+")"+toFZNann(ann)
      case int_lt(x,y,ann) => "int_lt("+toFZN(x)+","+toFZN(y)+")"+toFZNann(ann)
      case reif(int_eq(x,y,ann),z) => "int_eq_reif("+toFZN(x)+","+toFZN(y)+","+toFZN(z)+")"+toFZNann(ann)
      case reif(int_le(x,y,ann),z) => "int_le_reif("+toFZN(x)+","+toFZN(y)+","+toFZN(z)+")"+toFZNann(ann)
      case reif(int_lt(x,y,ann),z) => "int_lt_reif("+toFZN(x)+","+toFZN(y)+","+toFZN(z)+")"+toFZNann(ann)
      case reif(int_ne(x,y,ann),z) => "int_ne_reif("+toFZN(x)+","+toFZN(y)+","+toFZN(z)+")"+toFZNann(ann)
      case reif(set_in(x,y,ann),z) => "set_in_reif("+toFZN(x)+","+toFZN(y)+","+toFZN(z)+")"+toFZNann(ann)
      case int_lin_eq(x,y,z,ann) => "int_lin_eq("+toFZN(x)+","+toFZN(y)+","+toFZN(z)+")"+toFZNann(ann)
      case int_lin_le(x,y,z,ann) => "int_lin_le("+toFZN(x)+","+toFZN(y)+","+toFZN(z)+")"+toFZNann(ann)
      case int_lin_ne(x,y,z,ann) => "int_lin_ne("+toFZN(x)+","+toFZN(y)+","+toFZN(z)+")"+toFZNann(ann)
      case int_max(x,y,z,ann) => "int_max("+toFZN(x)+","+toFZN(y)+","+toFZN(z)+")"+toFZNann(ann)
      case int_min(x,y,z,ann) => "int_min("+toFZN(x)+","+toFZN(y)+","+toFZN(z)+")"+toFZNann(ann)
      case int_times(x,y,z,ann) => "int_times("+toFZN(x)+","+toFZN(y)+","+toFZN(z)+")"+toFZNann(ann)
      case int_div(x,y,z,ann) => "int_div("+toFZN(x)+","+toFZN(y)+","+toFZN(z)+")"+toFZNann(ann)
      case int_abs(x,y,ann) => "int_abs("+toFZN(x)+","+toFZN(y)+")"+toFZNann(ann)
      case array_int_element(x,y,z,ann) => "array_int_element("+toFZN(x)+","+toFZN(y)+","+toFZN(z)+")"+toFZNann(ann)
      case array_var_int_element(x,y,z,ann) => "array_var_int_element("+toFZN(x)+","+toFZN(y)+","+toFZN(z)+")"+toFZNann(ann)
      case array_bool_element(x,y,z,ann) => "array_bool_element("+toFZN(x)+","+toFZN(y)+","+toFZN(z)+")"+toFZNann(ann)
      case array_var_bool_element(x,y,z,ann) => "array_var_bool_element("+toFZN(x)+","+toFZN(y)+","+toFZN(z)+")"+toFZNann(ann)
      case array_bool_and(x,y, ann) => "array_bool_and("+toFZN(x)+","+toFZN(y)+")"+toFZNann(ann)
      case array_bool_or(x,y, ann) => "array_bool_or("+toFZN(x)+","+toFZN(y)+")"+toFZNann(ann)
      case array_bool_xor(x, ann) => "array_bool_xor("+toFZN(x)+")"+toFZNann(ann)
      case bool_clause(x,y, ann) => "bool_clause("+toFZN(x)+","+toFZN(y)+")"+toFZNann(ann)
      case reif(bool_clause(x,y, ann),z) => "bool_clause_reif("+toFZN(x)+","+toFZN(y)+","+toFZN(z)+")"+toFZNann(ann)
      case bool2int(x,y, ann) => "bool2int("+toFZN(x)+","+toFZN(y)+")"+toFZNann(ann)
      case bool_eq(x,y, ann) => "bool_eq("+toFZN(x)+","+toFZN(y)+")"+toFZNann(ann)
      case bool_lt(x,y, ann) => "bool_lt("+toFZN(x)+","+toFZN(y)+")"+toFZNann(ann)
      case bool_le(x,y, ann) => "bool_le("+toFZN(x)+","+toFZN(y)+")"+toFZNann(ann)
      case reif(bool_eq(x,y,ann),z) => "bool_eq_reif("+toFZN(x)+","+toFZN(y)+","+toFZN(z)+")"+toFZNann(ann)
      case reif(bool_le(x,y,ann),z) => "bool_le_reif("+toFZN(x)+","+toFZN(y)+","+toFZN(z)+")"+toFZNann(ann)
      case reif(bool_lt(x,y,ann),z) => "bool_lt_reif("+toFZN(x)+","+toFZN(y)+","+toFZN(z)+")"+toFZNann(ann)
      case bool_not(x,y, ann) => "bool_not("+toFZN(x)+","+toFZN(y)+")"+toFZNann(ann)
      case bool_xor(x,y,z,ann) => "bool_xor("+toFZN(x)+","+toFZN(y)+","+toFZN(z)+")"+toFZNann(ann)
      case bool_or(x,y,z,ann) => "bool_or("+toFZN(x)+","+toFZN(y)+","+toFZN(z)+")"+toFZNann(ann)
      case bool_and(x,y,z,ann) => "bool_and("+toFZN(x)+","+toFZN(y)+","+toFZN(z)+")"+toFZNann(ann)
      case count(x,y,z,ann) => "count("+toFZN(x)+","+toFZN(y)+","+toFZN(z)+")"+toFZNann(ann)
      case count_eq(x,y,z,ann) => "count_eq("+toFZN(x)+","+toFZN(y)+","+toFZN(z)+")"+toFZNann(ann)
      case at_least_int(x,y,z,ann) => "at_least_int("+toFZN(x)+","+toFZN(y)+","+toFZN(z)+")"+toFZNann(ann)
      case at_most_int(x,y,z,ann) => "at_most_int("+toFZN(x)+","+toFZN(y)+","+toFZN(z)+")"+toFZNann(ann)
      case reif(int_lin_eq(x,y,z,ann),w) => "int_lin_eq_reif("+toFZN(x)+","+toFZN(y)+","+toFZN(z)+","+toFZN(w)+")"+toFZNann(ann)
      case reif(int_lin_ne(x,y,z,ann),w) => "int_lin_ne_reif("+toFZN(x)+","+toFZN(y)+","+toFZN(z)+","+toFZN(w)+")"+toFZNann(ann)
      case reif(int_lin_le(x,y,z,ann),w) => "int_lin_le_reif("+toFZN(x)+","+toFZN(y)+","+toFZN(z)+","+toFZN(w)+")"+toFZNann(ann)
      case reif(count_eq(x,y,z,ann),w) => "count_eq_reif("+toFZN(x)+","+toFZN(y)+","+toFZN(z)+","+toFZN(w)+")"+toFZNann(ann)
      case reif(count(x,y,z,ann),w) => "count_reif("+toFZN(x)+","+toFZN(y)+","+toFZN(z)+","+toFZN(w)+")"+toFZNann(ann)
      case all_different_int(x,ann) => "all_different_int("+toFZN(x)+")"+toFZNann(ann)
      case global_cardinality_closed(x,y,z,ann) => "global_cardinality_closed("+toFZN(x)+","+toFZN(y)+","+toFZN(z)+")"+toFZNann(ann)
      case global_cardinality(x,y,z,ann) => "global_cardinality("+toFZN(x)+","+toFZN(y)+","+toFZN(z)+")"+toFZNann(ann)
      case global_cardinality_low_up_closed(x,y,z,w,ann) => "global_cardinality_low_up_closed("+toFZN(x)+","+toFZN(y)+","+toFZN(z)+","+toFZN(w)+")"+toFZNann(ann)
      case global_cardinality_low_up(x,y,z,w,ann) => "global_cardinality_low_up("+toFZN(x)+","+toFZN(y)+","+toFZN(z)+","+toFZN(w)+")"+toFZNann(ann)
      case member_int(x,y, ann) => "member_int("+toFZN(x)+","+toFZN(y)+")"+toFZNann(ann)
      case GeneratedConstraint(name,args,signature) => name+"("+args.map(toFZN(_)).mkString(", ")+")"
      case GenericConstraint(name,args, ann) => name+"("+args.map(toFZN(_)).mkString(", ")+")"+toFZNann(ann)
    }
  }
}