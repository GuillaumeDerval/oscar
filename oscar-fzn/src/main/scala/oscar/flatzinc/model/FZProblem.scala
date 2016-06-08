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
 * @author Leonard Debroux
 * @author Gustav Björdal
 * @author Jean-Noël Monette
 */
package oscar.flatzinc.model

import scala.Array.canBuildFrom
import scala.collection.mutable.{Map => MMap}
import scala.collection.mutable.{Set => MSet}
import scala.collection.immutable.Range
import oscar.flatzinc.UnsatException
import scala.collection.immutable.SortedSet

class FZProblem {
  val variables: MSet[Variable] = MSet.empty[Variable]
  val constraints: MSet[Constraint] = MSet.empty[Constraint]

  val solution:FZSolution = new FZSolution();
  val search = new Search();
  
  def addVariable(id: String, dom: Domain, bool: Boolean): Variable = {
    if(bool) addBooleanVariable(id,dom)
    else addIntegerVariable(id,dom)
  }
  def addIntegerVariable(id: String, dom: Domain): Variable = {
    val variable: Variable = new IntegerVariable(id, dom)
    variables += variable
    variable
  }
  def addBooleanVariable(id: String, dom: Domain): Variable = {
    val variable: Variable = new BooleanVariable(id, dom)
    variables += variable
    variable
  }
  
  def addConstraint(c: Constraint) {
    constraints += c
  }
  
  def satisfy(anns:Iterable[Annotation]) {
    search.obj = Objective.SATISFY
    search.setHeuristic(anns)
  }
  def minimize(obj: IntegerVariable,anns:Iterable[Annotation]) {
    search.obj = Objective.MINIMIZE
    search.variable = Some(obj)
    search.setHeuristic(anns)
  }
  def maximize(obj: IntegerVariable,anns:Iterable[Annotation]) {
    search.obj = Objective.MAXIMIZE
    search.variable = Some(obj)
    search.setHeuristic(anns)
  }
  
//  def addSearch(s: Array[Variable],vrh: VariableHeuristic.Value,vh: ValueHeuristic.Value) {
//    //println("search "+vrh+" "+vh+ " variables:"+s.mkString(","))
//    search.heuristics =  search.heuristics :+ (s,vrh,vh)
//  }
//  
//  def nSols(n: Int) {
//    search.nSols = n
//  }
}

sealed abstract class Domain {
  def min: Int
  def max: Int
  def contains(v:Int): Boolean
  def size: Int
  def boundTo(v: Int) = min == v && max == v
  def geq(v:Int);
  def leq(v:Int);
  def inter(d:Domain):Unit = {
    if(d.isInstanceOf[DomainRange])inter(d.asInstanceOf[DomainRange])
    else inter(d.asInstanceOf[DomainSet])
  }
  def inter(d:DomainRange):Unit = {
    geq(d.min);
    leq(d.max);
  }
  def inter(d:DomainSet):Unit = {
    throw new UnsupportedOperationException("Inter of a Set")
  }
  def checkEmpty() = {
    if (min > max) throw new UnsatException("Empty Domain");
  }
  def toSortedSet: SortedSet[Int]
}

case class DomainRange(var mi: Int, var ma: Int) extends Domain {
  //
  def min = mi
  def max = ma
  def contains(v:Int): Boolean = mi <= v && ma >= v
  def size = if(ma==Helper.FznMaxInt && mi==Helper.FznMinInt) Helper.FznMaxInt else ma-mi+1
  def geq(v:Int) = { mi = math.max(v,mi); checkEmpty() }
  def leq(v:Int) = { ma = math.min(v,ma); checkEmpty() }
  def toRange = mi to ma
  def toSortedSet: SortedSet[Int] = SortedSet[Int]() ++ (mi to ma)
}

case class DomainSet(var values: Set[Int]) extends Domain {
  override def checkEmpty() = {
    if (values.isEmpty) throw new UnsatException("Empty Domain");
  }
  def min = values.min
  def max = values.max
  def size = values.size
  def contains(v:Int): Boolean = values.contains(v)
  def geq(v:Int) = {values = values.filter(x => x>=v); checkEmpty() }
  def leq(v:Int) = {values = values.filter(x => x<=v); checkEmpty() }
  override def inter(d:DomainSet) = {values = values.intersect(d.values); checkEmpty() }
  def toSortedSet: SortedSet[Int] = SortedSet[Int]() ++ values
}

//TODO: CheckEmpty should go to the variables, as the domains are also used for normal sets that can be empty.
//TODO: differentiate between Int and Bool
//TODO: Add set variables
abstract class Variable(val id: String) {
  def isDefined: Boolean = {
    definingConstraint.isDefined//annotations.foldLeft(false)((acc,x) => x.name=="is_defined_var" || acc)
  }
  var definingConstraint: Option[Constraint] = Option.empty[Constraint]
  var cstrs:Set[Constraint] = Set.empty[Constraint]
  def addConstraint(c:Constraint) = {
    cstrs = cstrs + c
  }
  def removeConstraint(c:Constraint) = {
    cstrs = cstrs - c
   //cstrs = cstrs.filterNot(c.eq(_))//might be made more efficient if cstrs was a set.
  }
  def domainSize: Int;
  def isBound: Boolean;
}

case class BooleanVariable(i: String, private var _value: Option[Boolean] = None) extends Variable(i) {
  def this(s:String, dom: Domain) = this(s, {if (dom.min==dom.max) Some(dom.min==1) else None})
  def isTrue: Boolean = _value.getOrElse(false)
  def isFalse: Boolean = !_value.getOrElse(true) 
  override def isBound: Boolean = _value.isDefined
  override def domainSize: Int = if(isBound) 1 else 2
  def bind(v: Boolean) = if(isBound && v!=_value.get) throw new UnsatException("Empty Domain"); else _value = Some(v)
  def boolValue: Boolean = _value.get
  def intValue: Int = if(_value.get) 1 else 0
  override def toString = {this.id + (if(isBound) "="+_value.get else "");}
}

case class IntegerVariable(i: String, private var dom: Domain) extends Variable(i) {
  def this(i: String, v: Int) = this(i,DomainRange(v,v));
  def domain = dom
  override def domainSize = dom.size
  def min = dom.min
  def max = dom.max
  def geq(v:Int) = dom.geq(v)
  def leq(v:Int) = dom.leq(v)
  def inter(d:Domain) = (dom,d) match {
    case (DomainRange(_,_),DomainRange(_,_)) => dom.inter(d)
    case (DomainSet(_),DomainRange(_,_)) => dom.inter(d)
    case (DomainSet(_),DomainSet(_)) => dom.inter(d)
    case (DomainRange(l,u),DomainSet(values)) => dom = DomainSet(values.filter(v => v>=l && v <= u)); dom.checkEmpty();
  }
  def neq(v:Int) = {
    if(v==min) geq(v+1) 
    else if(v==max) leq(v-1) 
    else if(v >min && v < max){
     dom match {
       case DomainSet(values) => dom = DomainSet(values - v); dom.checkEmpty();
       case DomainRange(l,u) => dom = DomainSet((l to u).toSet - v); dom.checkEmpty();
     }
    }
  }
  /*def is01: Boolean = min >= 0 && max <= 1
  def isTrue: Boolean = this.is01 && min == 1
  def isFalse: Boolean = this.is01 && max == 0 */
  override def isBound: Boolean = min == max
  def bind(v: Int) = {geq(v); leq(v);}
  def value:Int = {if(isBound) min else throw new Exception("Asking for the value of an unbound variable")}
  override def toString = {this.id + (if(isBound) "="+value else "");}
}


//TODO: should go to the CP specific part
object VariableHeuristic extends Enumeration {
  val FIRST_FAIL = Value("first_fail")
  val INPUT_ORDER = Value("input_order")
  val ANTI_FIRST_FAIL = Value("anti_first_fail")
  val SMALLEST = Value("smallest")
  val LARGEST = Value("largest")
  val OCCURENCE = Value("occurence")
  val MOST_CONSTRAINED = Value("most_constrained")
  val MAX_REGRET = Value("max_regret") 
}

object ValueHeuristic extends Enumeration {
  val INDOMAIN_MIN = Value("indomain_min")
  val INDOMAIN_MAX = Value("indomain_max")
  val INDOMAIN_MIDDLE = Value("indomain_middle")
  val INDOMAIN_MEDIAN = Value("indomain_median")
  val INDOMAIN = Value("indomain")
  val INDOMAIN_RANDOM = Value("indomain_random")
  val INDOMAIN_SPLIT = Value("indomain_split")
  val INDOMAIN_REVERSE_SPLIT = Value("indomain_reverse_split")
  val INDOMAIN_INTERVAL = Value("indomain_interval")  
}

object Objective extends Enumeration {
  val MINIMIZE = Value("minimize")
  val MAXIMIZE = Value("maximize")
  val SATISFY = Value("satisfy")
}



class Search() {
  //var nSols = 0
  var obj: Objective.Value = Objective.SATISFY
  var variable: Option[IntegerVariable] = None
  //var heuristics: Vector[(Array[Variable],VariableHeuristic.Value,ValueHeuristic.Value)] = Vector.empty 
  private[this] var anns: Iterable[Annotation] = List.empty[Annotation]
  def setHeuristic(anns: Iterable[Annotation]) = { 
    this.anns = anns
    //if(this.anns.size > 0) Console.err.println("% ignoring search annotations")
  }
  def getHeuristic(): Iterable[Annotation] = anns
}
