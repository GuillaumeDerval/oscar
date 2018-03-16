/*
 * *****************************************************************************
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
 * ****************************************************************************
 */

package oscar.cbls.lib.invariant.logic

import oscar.cbls.core.computation._

import oscar.cbls.lib.invariant.minmax.{Miax, MiaxArray}

import scala.collection.immutable.SortedSet

/**
  * And(vars)
  *
  * outputs true (0) iff all vars are true (0) otherwise false (sum(vars))
  *
  * Calculates false as the sum of all vars.
  * This could be done differently to get a violation function that behaves differently,
  * we could, for example, compute the squared error, average, or median value.
  * But sum will do for now.
  *
  * @param vars is an iterable of IntVars representing booleans that are true iff 0, otherwise false.
  * @author gustav.bjordal@it.uu.se
  */
case class And(vars: Iterable[IntValue])
  extends IntInvariant(
    vars.foldLeft(0)((a: Int, b: IntValue) => a + b.value),
    0 to vars.foldLeft(0)((acc, intvar) => DomainHelper.safeAddMax(acc, intvar.max)))
    with IntNotificationTarget{

  for (v <- vars) registerStaticAndDynamicDependency(v)
  finishInitialization()

  override def notifyIntChanged(v: ChangingIntValue, id:Int, OldVal: Int, NewVal: Int) {
    this :+= NewVal - OldVal
  }

  override def checkInternals(){
    require(this.value == vars.foldLeft(0)((acc, intvar) => acc + intvar.value),
            Some("output.value == vars.foldLeft(0)((acc,intvar) => acc+intvar.value)"))
  }
}


/**
  * Or(vars)
  *
  * outputs true (0) iff any vars are true (0) otherwise false (min(vars))
  *
  * Calculates false as the minimum value of vars.
  * This could be done differently to get a violation function that behaves differently,
  * we could, for example, return 0 if true and otherwise compute the squared error, average, or median value.
  * But min will do for now.
  *
  * @param vars is an iterable of IntVars representing booleans that are true iff 0, otherwise false.
  * @author gustav.bjordal@it.uu.se
  */
/*
case class Or(vars: Array[IntValue]) extends MiaxArray(vars, null, vars.foldLeft(0)((acc,intvar) => Math.min(acc, intvar.max))) {

  override def Ord(v: IntValue): Int = v.value

  override def ExtremumName: String = "Or"

  //More precise bounds
  override def performBulkComputation(bulkedVar: Array[IntValue]) =
      (bulkedVar.foldLeft(Int.MaxValue)((acc, intvar) => Math.min(intvar.min, acc)),
        bulkedVar.foldLeft(Int.MaxValue)((acc, intvar) =>Math.min(intvar.max, acc)))

  override def checkInternals() {
    for (v <- this.vars) {
      require(this.value <= v.value,
              Some("this.value (" + this.value + ") <= " + v.name + ".value (" + v.value + ")"))
    }
  }
}
*/

case class Or(vars: Array[IntValue])
  extends IntInvariant(
    if(vars.exists(_.value == 0)) { 0 } else { vars.foldLeft(0)((a: Int, b: IntValue) => a + b.value)/vars.length },
    0 to vars.foldLeft(0)((acc, intvar) => DomainHelper.safeAddMax(acc, intvar.max)))
    with IntNotificationTarget{

  for (v <- vars) registerStaticAndDynamicDependency(v)
  finishInitialization()

  override def notifyIntChanged(v: ChangingIntValue, id:Int, OldVal: Int, NewVal: Int) {
    if(vars.exists(_.value == 0)) {
      this := 0
    }else{
      this := vars.foldLeft(0)((a: Int, b: IntValue) => a + b.value) /vars.length
    }
  }

  override def checkInternals(){
    require(this.value == vars.foldLeft(0)((acc, intvar) => acc + intvar.value),
            Some("output.value == vars.foldLeft(0)((acc,intvar) => acc+intvar.value)"))
  }
}


/**
  * bool2int(var)
  *
  * outputs 1 iff var is true (0) otherwise 0
  *
  * Warning: Note that this breaks the representation of booleans, where 0 is true.
  *
  * @param v an IntValue
  * @author gustav.bjordal@it.uu.se
  */
case class Bool2Int(v: IntValue)
  extends IntInvariant(if(v.value == 0) 1 else 0, 0 to 1)
    with IntNotificationTarget{

  registerStaticAndDynamicDependency(v)
  finishInitialization()

  override def notifyIntChanged(v: ChangingIntValue, id:Int, OldVal: Int, NewVal: Int) {
    this := (if(NewVal == 0) 1 else 0)
  }
}


/**
  * boolLE(a,b)
  *
  * outputs true (0) iff a <= b otherwise false (a)
  *
  *
  * @param a IntValue
  * @param b IntValue
  * @author gustav.bjordal@it.uu.se
  */
case class BoolLEInv(a: IntValue, b:IntValue)
  extends IntInvariant(if(a.value > 0 && b.value == 0) a.value else 0, 0 to a.max)
    with IntNotificationTarget{

  registerStaticAndDynamicDependency(a)
  registerStaticAndDynamicDependency(b)
  finishInitialization()

  override def notifyIntChanged(v: ChangingIntValue, id:Int, OldVal: Int, NewVal: Int) {
    this :=
      (if(v == a){
        if(NewVal > 0 && b.value == 0){
          NewVal+1
        }else{
          0
        }
      }else{
        if(NewVal == 0 && a.value >0){
          a.value+1
        }else{
          0
        }
      }) / 2
  }
}

/**
  * boolLT(a,b)
  *
  * outputs true (0) iff a < b otherwise false (a+1)
  *
  *
  * @param a IntValue
  * @param b IntValue
  * @author gustav.bjordal@it.uu.se
  */
case class BoolLTInv(a: IntValue, b:IntValue)
  extends IntInvariant(if(a.value > 0) a.value else if(b.value == 0) 1 else 0, 0 to a.max)
    with IntNotificationTarget{

  registerStaticAndDynamicDependency(a)
  registerStaticAndDynamicDependency(b)
  finishInitialization()

  override def notifyIntChanged(v: ChangingIntValue, id:Int, OldVal: Int, NewVal: Int) {
    this :=
      (if(v == a){
        if(NewVal == 0 && b.value>0)
          0
        else
          (NewVal + b.value + 1)
      }else{
        if(NewVal >0 && a.value == 0)
          0
        else
         (NewVal + a.value + 1)
      }) / 2

  }
}

/**
  * xor(a,b)
  *
  * outputs true (0) iff a != b otherwise false (min(a,b)+1)
  *
  *
  * @param a IntValue
  * @param b IntValue
  * @author gustav.bjordal@it.uu.se
  */
case class XOR(a: IntValue, b:IntValue)
  extends IntInvariant(if((a.value > 0 && b.value > 0) || (a.value == 0 && b.value == 0)) (a.value + b.value + 1) else 0,
                       0 to Math.max(a.max,b.max))
    with IntNotificationTarget{

  registerStaticAndDynamicDependency(a)
  registerStaticAndDynamicDependency(b)
  finishInitialization()

  override def notifyIntChanged(v: ChangingIntValue, id:Int, OldVal: Int, NewVal: Int) {
    if((NewVal > 0 && OldVal == 0) || (NewVal == 0 && OldVal > 0)){
      if(this.value > 0){
        this := 0
      }else {
        if (v == a) {
          this := (NewVal + b.value + 1)/2
        } else {
          this := (a.value + NewVal + 1)/2
        }
      }
    }
  }
}

/**
  * xor(var)
  *
  * outputs true (0) iff a != b otherwise false (min(a,b)+1)
  *
  *
  * @param vars  array of IntValue
  * @author gustav.bjordal@it.uu.se
  */
case class XORArray(vars: Array[IntValue])
  extends IntInvariant((vars.foldLeft(1)((acc,v) => if(v.value > 0) acc+1 else acc)%2)*vars.length,
                       0 to vars.foldLeft(0)((acc,v)=>acc+v.max))
    with IntNotificationTarget{

  registerStaticAndDynamicDependencyArrayIndex(vars)
  finishInitialization()

  override def notifyIntChanged(v: ChangingIntValue, id:Int, OldVal: Int, NewVal: Int) {
    if((NewVal > 0 && OldVal == 0) || (NewVal == 0 && OldVal > 0)){
      if(this.value > 0){
        this := 0
      }else {
        this := vars.foldLeft(Int.MaxValue)((acc,v) => if(v.value>0) v.value + acc else acc)+1
      }
    }
  }
}

object XOR{
  def apply(vars: Array[IntValue]) = new XORArray(vars)
}

object BoolNE{
  def apply(a: IntValue, b:IntValue) = new XOR(a,b)
}

/**
  * not(a)
  *
  * outputs true (0) iff a > 0 otherwise false (1)
  *
  *
  * @param a IntValue
  * @author gustav.bjordal@it.uu.se
  */
case class Not(a: IntValue)
  extends IntInvariant(if(a.value > 0) 0 else 1,
                       0 to 1)
    with IntNotificationTarget{

  registerStaticAndDynamicDependency(a)
  finishInitialization()

  override def notifyIntChanged(v: ChangingIntValue, id:Int, OldVal: Int, NewVal: Int) {
    this := (if(NewVal > 0) 0 else 1)
  }
}