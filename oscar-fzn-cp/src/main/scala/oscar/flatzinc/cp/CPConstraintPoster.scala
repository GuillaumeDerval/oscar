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

import oscar.flatzinc.model._
import scala.language.implicitConversions 
import oscar.cp._

class CPConstraintPoster(val pstrength: oscar.cp.core.CPPropagStrength){ 
  implicit def c2ca(c: oscar.cp.Constraint): Array[(oscar.cp.Constraint,oscar.cp.core.CPPropagStrength)] = Array[(oscar.cp.Constraint,oscar.cp.core.CPPropagStrength)]((c,pstrength))
  implicit def c2cs(c: oscar.cp.Constraint): (oscar.cp.Constraint,oscar.cp.core.CPPropagStrength)= (c,pstrength)
  implicit def c2ca(c: (oscar.cp.Constraint,oscar.cp.core.CPPropagStrength)): Array[(oscar.cp.Constraint,oscar.cp.core.CPPropagStrength)] = Array[(oscar.cp.Constraint,oscar.cp.core.CPPropagStrength)](c)
  implicit def ca2cs(c: Array[oscar.cp.Constraint]): Array[(oscar.cp.Constraint,oscar.cp.core.CPPropagStrength)] = c.map(c2cs)
  def getConstraint(c: oscar.flatzinc.model.Constraint,getVar: Variable => CPIntVar,getBoolVar: Variable => CPBoolVar): Array[(oscar.cp.Constraint,oscar.cp.core.CPPropagStrength)] = {
    def getVarArray(x:Array[Variable]) = {x.map(getVar);}
    c match{
      //TODO: We create variables in cumulative but we might want to memoize those as well!
      //TODO: Actually to avoid creating new variables, we could rewrite the cumulative in the minizinc definition to create those variables while flattening, and CSE will take care of it.
      case cumulative(s,d,r,capa,_) => oscar.cp.maxCumulativeResource(s.map(getVar), d.map(getVar), s.zip(d).map(vv => getVar(vv._1)+getVar(vv._2)), r.map(getVar), getVar(capa))
      case maximum_int(x,y,_) => 
        c2ca(oscar.cp.maximum(y.map(getVar), getVar(x))) ++ ca2cs(y.map(v => getVar(v) <= getVar(x)))
      case minimum_int(x,y,_) => 
        c2ca(oscar.cp.minimum(y.map(getVar), getVar(x))) ++ ca2cs(y.map(v => getVar(v) >= getVar(x)))
     
      case all_different_int(x,_) => (oscar.cp.allDifferent(x.map(getVar)), Medium)//Weak, Strong, Medium
      case at_least_int(c,x,v,_) => oscar.cp.atLeast(c.value, x.map(getVar), v.value)
      case at_most_int(c,x,v,_) => oscar.cp.atMost(c.value, x.map(getVar), v.value)
      case exactly_int(c,x,v,_) => oscar.cp.countEq(getVar(c), x.map(getVar), v.value)//TODO: use a version with fixed c
      case count_eq(x,v,c,_) => oscar.cp.countEq(getVar(c), x.map(getVar), getVar(v))
      case inverse(xs,ys,ann)                         => oscar.cp.inverse(xs.map(getVar(_)-1),ys.map(getVar(_)-1))

      case bin_packing_load(load,bin,w,ann)           => new oscar.cp.constraints.BinPacking(bin.map(getVar(_)-1),w.map(_.value),load.map(getVar))
      case circuit(xs, ann)                           => new oscar.cp.constraints.Circuit(xs.map(v => getVar(v)-1),false)
      case subcircuit(xs, ann)                        => oscar.cp.constraints.SubCircuit(xs.map(getVar),1)


      //case reif(count_eq(x,v,c,_),b) => getBoolVar(b) ?== oscar.cp.countEq(getVar(c), x.map(getVar), getVar(v))
      
      case array_bool_and(as, r, ann)                 => new oscar.cp.constraints.And(as.map(getBoolVar),getBoolVar(r))
     // case array_bool_element(b, as, r, ann)          => 
      case array_bool_or(as, r, ann)                  => oscar.cp.or(as.map(getBoolVar),getBoolVar(r))
     // case array_bool_xor(as, ann)                    => 
      case array_int_element(b, as, r, ann)           => oscar.cp.element(as.map(_.value), getVar(b)-1, getVar(r))
      case array_var_bool_element(b, as, r, ann)      => oscar.cp.elementVar(as.map(getBoolVar), getVar(b)-1, getBoolVar(r))
      case array_var_int_element(b, as, r, ann)       => oscar.cp.elementVar(as.map(getVar), getVar(b)-1, getVar(r))

      case bool2int(x, y, ann)                        => getBoolVar(x) == getVar(y)
      case bool_and(a, b, r, ann)                     => new oscar.cp.constraints.And(Array(getBoolVar(a),getBoolVar(b)),getBoolVar(r))
      case bool_clause(a, b, ann)                     => oscar.cp.or(a.map(getBoolVar)++b.map(getBoolVar(_).not))
      case reif(bool_clause(a, b, ann),c)             => oscar.cp.or(a.map(getBoolVar)++b.map(getBoolVar(_).not),getBoolVar(c))
      case bool_eq(a, b, ann)                         => getBoolVar(a) == getBoolVar(b)
      case bool_le(a, b, ann)                         => getBoolVar(a) <= getBoolVar(b)
      case bool_lin_eq(params, vars, sum, ann)        => oscar.cp.weightedSum(params.map(_.value), vars.map(getVar), getVar(sum))
      case bool_lin_le(params, vars, sum, ann)        => oscar.cp.weightedSum(params.map(_.value), vars.map(getVar)) <= getVar(sum) //TODO: make it native
      case bool_lt(a, b, ann)                         => getBoolVar(a) < getBoolVar(b)
      case reif(bool_lt(a, b, ann),c)                 => getBoolVar(c) == (getBoolVar(a) ?< getBoolVar(b))//TODO: There might exist a better expression for this.
      case reif(bool_eq(a, b, ann),c)                 => getBoolVar(c) == (getBoolVar(a) ?== getBoolVar(b))//TODO: There might exist a better expression for this.
      case bool_not(a, b, ann)                        => getBoolVar(a) == getBoolVar(b).not
      case bool_or(a, b, r, ann)                      => oscar.cp.or(Array(getBoolVar(a),getBoolVar(b)),getBoolVar(r))
      case bool_xor(a, b, r, ann)                     => getBoolVar(r) == (getVar(a) ?!= getVar(b))

      case int_abs(x, y, ann)                         => new oscar.cp.constraints.Abs(getVar(x), getVar(y))
      //case int_div(x, y, z, ann)                      => oscar.cp.mul(getVar(y), getVar(z)) == getVar(x) // TODO: this does not work as intended
      case int_eq(x, y, ann)                          => getVar(x) == getVar(y)
      case int_le(x, y, ann)                          => getVar(x) <= getVar(y)
      case int_lt(x, y, ann)                          => getVar(x) < getVar(y)
      case reif(int_eq(x,y,ann),b)                    => getBoolVar(b) == (getVar(x) ?== getVar(y))
      case reif(int_le(x,y,ann),b)                    => getBoolVar(b) == (getVar(x) ?<= getVar(y))
      case reif(int_lt(x,y,ann),b)                    => getBoolVar(b) == (getVar(x) ?< getVar(y))
      case reif(int_ne(x,y,ann),b)                    => getBoolVar(b) == (getVar(x) ?!= getVar(y))
      //TODO: Handle binary and ternary cases, as well as all unit weights
      case int_lin_eq(params, vars, sum, ann)         => oscar.cp.weightedSum(params.map(_.value), vars.map(getVar), getVar(sum))
      case int_lin_le(params, vars, sum, ann)         => oscar.cp.weightedSum(params.map(_.value), vars.map(getVar)) <= getVar(sum) //TODO: make it native
      case int_lin_ne(params, vars, sum, ann)         => oscar.cp.weightedSum(params.map(_.value), vars.map(getVar)) != getVar(sum) //TODO: make it native
      case reif(int_lin_eq(params, vars, sum, ann),b) => getBoolVar(b) == (oscar.cp.weightedSum(params.map(_.value), vars.map(getVar)) ?== getVar(sum)) //TODO: make it native
      case reif(int_lin_le(params, vars, sum, ann),b) => getBoolVar(b) == (oscar.cp.weightedSum(params.map(_.value), vars.map(getVar)) ?<= getVar(sum)) //TODO: make it native
      case reif(int_lin_ne(params, vars, sum, ann),b) => getBoolVar(b) == (oscar.cp.weightedSum(params.map(_.value), vars.map(getVar)) ?!= getVar(sum)) //TODO: make it native
      case int_max(x, y, z, ann)                      => oscar.cp.maximum(Array(getVar(x),getVar(y)),getVar(z))
      case int_min(x, y, z, ann)                      => oscar.cp.minimum(Array(getVar(x),getVar(y)),getVar(z))
      case int_mod(x, y, z, ann) if y.isBound         => getVar(x) % y.value == getVar(z)
      case int_ne(x, y, ann)                          => getVar(x) != getVar(y)
      case int_plus(x, y, z, ann)                     => oscar.cp.plus(getVar(x), getVar(y))==getVar(z)
      case int_times(x, y, z, ann)                    => oscar.cp.mul(getVar(x), getVar(y)) ==getVar(z)
      case set_in(x, s, ann)                          => new oscar.cp.constraints.InSet(getVar(x),s.toSortedSet)
      case reif(set_in(x, s, ann),b)                  => new oscar.cp.constraints.InSetReif(getVar(x),s.toSortedSet,getBoolVar(b))
      case table_int(xs,ts,ann)                       => oscar.cp.table(xs.map(getVar(_)), Array.tabulate(ts.size/xs.size)(row => Array.tabulate(xs.size)(i => ts(row*xs.size + i).value)))
      case default => Console.err.println("% No CP constraint found for " + default)
        Array.empty
    } 
  }
  
  
}
  
