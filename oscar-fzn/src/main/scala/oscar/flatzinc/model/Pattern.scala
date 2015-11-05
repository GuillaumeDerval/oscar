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

abstract class Pattern {

  
}
object CstrPattern{
 /* def apply(name: String, args: List[Pattern]): CstrPattern = {
    CstrPattern(name,args)
  }*/
  //THIS is used to match DefCstrPAttern ad a CstrPattern
  def unapply(dcp: Pattern): Option[(String,List[Pattern])] = {
    if(dcp.isInstanceOf[CstrPattern])dcp.asInstanceOf[CstrPattern] match {
      case CstrPattern(name,args) => Some((name,args))
    }else if(dcp.isInstanceOf[DefCstrPattern]){
      dcp.asInstanceOf[DefCstrPattern] match {
        case DefCstrPattern(name,args,defargs) => Some((name,args))
      }
    }else None
  }
  /*
  def unapply(dcp: CstrPattern): Option[(String,List[Pattern])] = {
    Some((dcp.name,dcp.args))
  }*/
}
case class CstrPattern(val name: String, val args: List[Pattern]) extends Pattern
//Any array of identical constraints on arguments in parallel arrays
case class CstrArrayPattern(val name: String, val args: List[Pattern]) extends Pattern

case class DefCstrPattern(val name: String, val args: List[Pattern], val defArg: Pattern) extends Pattern

case class IntVarPattern(val id: String) extends Pattern
case class AnyIntVarPattern(val id: String) extends Pattern

//case class SomeIntValPattern(val id: String) extends Pattern
case class IntValPattern(val v: Int) extends Pattern
case class AnyIntValPattern(val id: String) extends Pattern

case class BoolVarPattern(val id: String) extends Pattern
case class AnyBoolVarPattern(val id: String) extends Pattern

case class BoolValPattern(val v: Boolean) extends Pattern
//case class SomeBoolValPattern(val id: String) extends Pattern
case class AnyBoolValPattern(val id: String) extends Pattern

case class SetValPattern(val v: Domain) extends Pattern
//case class SomeSetValPattern(val id: String) extends Pattern
case class AnySetValPattern(val id: String) extends Pattern


case class ArrayPattern(val vals: List[Pattern]) extends Pattern
//used as argument of nary constraint
case class IntVarArrayPattern(val id: String) extends Pattern
//used as argument of nary constraint
case class BoolVarArrayPattern(val id: String) extends Pattern

case class SomeIntValArrayPattern(val vals: List[Pattern]) extends Pattern
case class SomeBoolValArrayPattern(val vals: List[Pattern]) extends Pattern
case class AnyIntValArrayPattern(val id: String) extends Pattern
case class AnyBoolValArrayPattern(val id: String) extends Pattern

case class GroupPattern(var cstrs: List[Pattern]) extends Pattern