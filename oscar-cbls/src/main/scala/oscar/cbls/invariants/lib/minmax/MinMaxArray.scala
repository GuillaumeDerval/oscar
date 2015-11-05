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
/*******************************************************************************
  * Contributors:
  *     This code has been initially developed by CETIC www.cetic.be
  *         by Renaud De Landtsheer
  *            Yoann Guyot
  ******************************************************************************/

package oscar.cbls.invariants.lib.minmax

import oscar.cbls.invariants.core.algo.heap.{ArrayMap, BinomialHeapWithMoveExtMem}
import oscar.cbls.invariants.core.algo.quick.QList
import oscar.cbls.invariants.core.computation.Invariant._
import oscar.cbls.invariants.core.computation._
import oscar.cbls.invariants.core.propagation.{Checker, KeyForElementRemoval}

import scala.collection.immutable.SortedSet

/**
 * Maintains Max(Var(i) | i in cond)
 * @param varss is an array of IntVar, which can be bulked
 * @param cond is the condition, supposed fully acceptant if not specified (must be specified if varss is bulked)
 * update is O(log(n))
 * @author renaud.delandtsheer@cetic.be
 * */
case class MaxArray(varss: Array[IntValue], cond: SetValue = null, default: Int = Int.MinValue)
  extends MiaxArray(varss, cond, default) {

  override def Ord(v: IntValue): Int = -v.value

  override def ExtremumName: String = "Max"

  //More precise bounds
  override def performBulkComputation(bulkedVar: Array[IntValue]) = {
    (bulkedVar.foldLeft(Int.MinValue)((acc, intvar) => if (intvar.min > acc) intvar.min else acc),
      bulkedVar.foldLeft(Int.MinValue)((acc, intvar) => if (intvar.max > acc) intvar.max else acc))
  }
  
  override def checkInternals(c: Checker) {
    for (v <- this.varss) {
      c.check(this.value >= v.value,
        Some("output.value (" + this.value + ") >= " + v.name + ".value (" + v.value + ")"))
    }
  }
}

/**
 * Maintains Min(Var(i) | i in cond)
 * @param varss is an array of IntVar, which can be bulked
 * @param cond is the condition, supposed fully acceptant if not specified (must be specified if varss is bulked)
 * update is O(log(n))
 * @author renaud.delandtsheer@cetic.be
 * */
case class MinArray(varss: Array[IntValue], cond: SetValue = null, default: Int = Int.MaxValue)
  extends MiaxArray(varss, cond, default) {

  override def Ord(v: IntValue): Int = v.value

  override def ExtremumName: String = "Min"

  //More precise bounds 
  override def performBulkComputation(bulkedVar: Array[IntValue]) = {
    (bulkedVar.foldLeft(Int.MaxValue)((acc, intvar) => if (intvar.min < acc) intvar.min else acc),
      bulkedVar.foldLeft(Int.MaxValue)((acc, intvar) => if (intvar.max < acc) intvar.max else acc))
  }
  
  override def checkInternals(c: Checker) {
    for (v <- this.varss) {
      c.check(this.value <= v.value,
        Some("this.value (" + this.value + ") <= " + v.name + ".value (" + v.value + ")"))
    }
  }
}

/**
 * Maintains Miax(Var(i) | i in cond)
 * Exact ordering is specified by implementing abstract methods of the class.
 * @param vars is an array of IntVar, which can be bulked
 * @param cond is the condition, can be null
 * update is O(log(n))
 * @author renaud.delandtsheer@cetic.be
 * */
abstract class MiaxArray(vars: Array[IntValue], cond: SetValue, default: Int)
  extends IntInvariant with Bulked[IntValue, Domain] with VaryingDependencies {

  var keyForRemoval: Array[KeyForElementRemoval] = new Array(vars.length)
  var h: BinomialHeapWithMoveExtMem[Int] = new BinomialHeapWithMoveExtMem[Int](i => Ord(vars(i)), vars.length, new ArrayMap(vars.length))

  if (cond != null) {
    registerStaticDependency(cond)
    registerDeterminingDependency(cond)
  }

  restrictDomain(bulkRegister(vars).union(default))

  if (cond != null) {
    for (i <- cond.value) {
      h.insert(i)
      keyForRemoval(i) = registerDynamicDependency(vars(i), i)
    }
  } else {
    for (i <- vars.indices) {
      h.insert(i)
      keyForRemoval(i) = registerDynamicDependency(vars(i), i)
    }
  }

  finishInitialization()

  override def performBulkComputation(bulkedVar: Array[IntValue]) =
    InvariantHelper.getMinMaxBounds(bulkedVar)

  def ExtremumName: String
  def Ord(v: IntValue): Int

  if (h.isEmpty) {
    this := default
  } else {
    this := vars(h.getFirst).value
  }

  @inline
  override def notifyIntChanged(v: ChangingIntValue, index: Int, OldVal: Int, NewVal: Int) {
    //mettre a jour le heap
    h.notifyChange(index)
    this := vars(h.getFirst).value
  }

  @inline
  override def notifyInsertOn(v: ChangingSetValue, value: Int) {
    assert(v == cond)
    keyForRemoval(value) = registerDynamicDependency(vars(value), value)

    //mettre a jour le heap
    h.insert(value)
    this := vars(h.getFirst).value
  }

  @inline
  override def notifyDeleteOn(v: ChangingSetValue, value: Int) {
    assert(v == cond)

    keyForRemoval(value).performRemove()
    keyForRemoval(value) = null

    //mettre a jour le heap
    h.delete(value)
    if (h.isEmpty) {
      this := default
    } else {
      this := vars(h.getFirst).value
    }
  }
}


/**
 * Maintains Min(Var(i) | i in cond)
 * @param varss is an array of Int
 * @param ccond is the condition, supposed fully acceptant if not specified (must be specified if varss is bulked)
 * update is O(log(n))
 * @author renaud.delandtsheer@cetic.be
 * */
case class MinConstArray(varss: Array[Int], ccond: SetValue, default: Int = Int.MaxValue)
  extends MiaxConstArray(varss, ccond, default) {

  override def Ord(v: IntValue): Int = v.value

  override def checkInternals(c: Checker) {
    for (v <- this.varss) {
      c.check(this.value <= v,
        Some("this.value (" + this.value + ") <= " + v + ".value (" + v + ")"))
    }
  }
}


/**
 * Maintains Max(Var(i) | i in cond)
 * @param varss is an array of IntVar, which can be bulked
 * @param ccond is the condition, supposed fully acceptant if not specified (must be specified if varss is bulked)
 * update is O(log(n))
 * @author renaud.delandtsheer@cetic.be
 * */
case class MaxConstArray(varss: Array[Int], ccond: SetValue, default: Int = Int.MinValue)
  extends MiaxConstArray(varss, ccond, default) {

  override def Ord(v: IntValue): Int = -v.value

  override def checkInternals(c: Checker) {
    for (v <- this.varss) {
      c.check(this.value >= v,
        Some("output.value (" + this.value + ") >= " + v + ".value (" + v + ")"))
    }
  }
}

/**
 * Maintains Min(Var(i) | i in cond)
 * this is a variant that is lazy, and maintains a TODO-list of postponed updates.
 * postponed updates are ones that do not impact on the outout of the invariant.
 * when there is an update, it is first checked against the TODO-list, for cancellation.
 * if the update does not impact the output, it is postponed
 * if it affects the output, it is performed
 * @param varss is an array of Int
 * @param ccond is the condition, supposed fully acceptant if not specified (must be specified if varss is bulked)
 * @param maxTODOSize is the maximal number of postponed updates (TODOlist is handled as a FIFO)
 * update is O(log(n))
 * @author renaud.delandtsheer@cetic.be
 * */
case class MinConstArrayLazy(varss: Array[Int], ccond: SetValue, default: Int = Int.MaxValue, maxTODOSize:Int = 2)
  extends MiaxConstArrayLazy(varss, ccond, default, maxTODOSize) {

  override def Ord(v: IntValue): Int = v.value

  override def checkInternals(c: Checker) {
    for (v <- this.varss) {
      c.check(this.value <= v,
        Some("this.value (" + this.value + ") <= " + v + ".value (" + v + ")"))
    }
  }
}


/**
 * Maintains Max(Var(i) | i in cond)
 * this is a variant that is lazy, and maintains a TODO-list of postponed updates.
 * postponed updates are ones that do not impact on the outout of the invariant.
 * when there is an update, it is first checked against the TODO-list, for cancellation.
 * if the update does not impact the output, it is postponed
 * if it affects the output, it is performed
 * @param varss is an array of IntVar, which can be bulked
 * @param ccond is the condition, supposed fully acceptant if not specified (must be specified if varss is bulked)
 * @param maxTODOSize is the maximal number of postponed updates (TODOlist is handled as a FIFO)
 * update is O(log(n))
 * @author renaud.delandtsheer@cetic.be
 * */
case class MaxConstArrayLazy(varss: Array[Int], ccond: SetValue, default: Int = Int.MinValue, maxTODOSize:Int = 2)
  extends MiaxConstArrayLazy(varss, ccond, default, maxTODOSize) {

  override def Ord(v: IntValue): Int = -v.value

  override def checkInternals(c: Checker) {
    for (v <- this.varss) {
      c.check(this.value >= v,
        Some("output.value (" + this.value + ") >= " + v + ".value (" + v + ")"))
    }
  }
}

/**
 * Maintains Miax(Var(i) | i in cond)
 * Exact ordering is specified by implementing abstract methods of the class.
 * @param vars is an array of IntVar, which can be bulked
 * @param cond is the condition, cannot be null
 * update is O(log(n))
 * @author renaud.delandtsheer@cetic.be
 * */
abstract class MiaxConstArray(vars: Array[Int], cond: SetValue, default: Int)
  extends IntInvariant{

  var h: BinomialHeapWithMoveExtMem[Int] = new BinomialHeapWithMoveExtMem[Int](i => Ord(vars(i)), vars.length, new ArrayMap(vars.length))

  registerStaticAndDynamicDependency(cond)
  finishInitialization()

  //TODO: restrict domain

  for (i <- cond.value) {
    h.insert(i)
  }

  def Ord(v: IntValue): Int

  if (h.isEmpty) {
    this := default
  } else {
    this := vars(h.getFirst)
  }

  @inline
  override def notifyInsertOn(v: ChangingSetValue, value: Int) {
    assert(v == cond)

    //mettre a jour le heap
    h.insert(value)
    this := vars(h.getFirst)
  }

  @inline
  override def notifyDeleteOn(v: ChangingSetValue, value: Int) {
    assert(v == cond)

    //mettre a jour le heap
    h.delete(value)
    if (h.isEmpty) {
      this := default
    } else {
      this := vars(h.getFirst)
    }
  }
}

/**
 * Maintains Miax(Var(i) | i in cond)
 * Exact ordering is specified by implementing abstract methods of the class.
 * @param vars is an array of IntVar, which can be bulked
 * @param cond is the condition, cannot be null
 * update is O(log(n))
 * @author renaud.delandtsheer@cetic.be
 * */
abstract class MiaxConstArrayLazy(vars: Array[Int], cond: SetValue, default: Int, maxToDoSize:Int)
  extends IntInvariant{

  val n = vars.length
  var h: BinomialHeapWithMoveExtMem[Int] = new BinomialHeapWithMoveExtMem[Int](i => Ord(vars(i)), vars.length, new ArrayMap(vars.length))

  registerStaticAndDynamicDependency(cond)
  finishInitialization()

  //TODO: restrict domain

  for (i <- cond.value) {
    h.insert(i)
  }

  def Ord(v: IntValue): Int

  if (h.isEmpty) {
    this := default
  } else {
    this := vars(h.getFirst)
  }

  val self = this

  //an update is a couple: (value, bolean)
  // where the boolean is true for an insert, false for a delete

  private def isOpposite(a:(Int,Boolean),b:(Int,Boolean)):Boolean =
    a._1 == b._1 && a._2 != b._2

  private def  doIt(a:(Int,Boolean)) {
    if (a._2) {
      h.insert(a._1)
      self := vars(h.getFirst)
    } else {
      h.delete(a._1)
      if (h.isEmpty) {
        self := default
      } else {
        self := vars(h.getFirst)
      }
    }
  }

  private def isMiaxImpacted(u:(Int,Boolean)): Boolean = {
    if(u._2){
      //inseret
      Ord(vars(u._1)) < Ord(self.getValue(true))
    }else{
      //delete
      vars(u._1) == self.getValue(true)
    }
  }

  //they must be done in this order
  var toDo:QList[(Int,Boolean)] = null
  var toDoSize:Int = 0

  @inline
  override def notifyInsertOn(v: ChangingSetValue, value: Int) {
    assert(v == cond)
    processUpdate((value,true))
  }

  @inline
  override def notifyDeleteOn(v: ChangingSetValue, value: Int) {
    assert(v == cond)
    processUpdate((value,false))
  }

  def processUpdate(u:(Int,Boolean)){
    if(isMiaxImpacted(u)){
      //      println("done straight" + u)
      doIt(u)
      while(toDo != null){
        doIt(toDo.head)
        toDo = toDo.tail
      }
      toDoSize  = 0
    }else{
      postponeOrDo(u)
    }
  }

  //TODO this is a queue, but a stack might be faster.
  def postponeOrDo(u:(Int,Boolean)) {
//    println("postponeOrDo:" + u)
    def updateToDo(mToDo: QList[(Int,Boolean)]): QList[(Int,Boolean)] =
      if (mToDo == null) {
        toDoSize += 1;
//        println("postponed");
        QList(u)
      } else {
        if (isOpposite(mToDo.head,u)) {
//          println("anihilation")
          toDoSize -= 1
          mToDo.tail
        } else QList(mToDo.head, updateToDo(mToDo.tail))
      }

    toDo = updateToDo(toDo)
    while(toDoSize > maxToDoSize){
//      println("popping")
      doIt(toDo.head)
      toDo = toDo.tail
      toDoSize -=1
    }
  }
}
