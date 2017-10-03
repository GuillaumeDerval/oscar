package oscar.cp.constraints.tables

import oscar.cp.core.variables.CPIntVar

import scala.collection.mutable.ArrayBuffer

/**
 * Created by helene on 2/10/17.
 */
object TableUtil {

  def toGroundTable(x:Array[CPIntVar],table:Array[Array[Int]],star:Int) = {
    val buff = new ArrayBuffer[Array[Int]]()
    val size = x.length
    def addToBuff(tuple:Array[Int],index:Int) : Unit = {
      if (index < size){
        if (tuple(index) == star){
          for (v <- x(index).iterator){
            val newTuple = tuple.clone()
            newTuple(index) = v
            addToBuff(newTuple, index + 1)
          }
        } else {
          addToBuff(tuple, index+1)
        }
      } else {
        buff += tuple
      }
    }
    var i = table.length
    while (i > 0){
      i -= 1
      addToBuff(table(i),0)
    }
    buff.toArray
  }

  def toGroundTable(x:Array[CPIntVar],table:Array[Array[BasicSmartElement]],star:Int = -1) = {
    val buff = new ArrayBuffer[Array[Int]]()
    val size = x.length
    def addToBuff(tupleBS:Array[BasicSmartElement],tuple:Array[Int],index:Int) : Unit = {
      if (index < size){
        tupleBS(index) match {
          case Equal(vl) =>
            tuple(index) = vl
            addToBuff(tupleBS,tuple,index + 1)
          case Star() =>
            for (v <- x(index).iterator){
              val newTuple = tuple.clone()
              newTuple(index) = v
              addToBuff(tupleBS,newTuple, index + 1)
            }
          case NotEqual(vl) =>
            for (v <- x(index).iterator;if vl != v){
              val newTuple = tuple.clone()
              newTuple(index) = v
              addToBuff(tupleBS,newTuple, index + 1)
            }
          case LessEq(vl) =>
            for (v <- x(index).iterator;if v <= vl){
              val newTuple = tuple.clone()
              newTuple(index) = v
              addToBuff(tupleBS,newTuple, index + 1)
            }
          case GreatEq(vl) =>
            for (v <- x(index).iterator;if v >= vl){
              val newTuple = tuple.clone()
              newTuple(index) = v
              addToBuff(tupleBS,newTuple, index + 1)
            }
          case InSet(vl) =>
            for (v <- x(index).iterator;if vl.contains(v)){
              val newTuple = tuple.clone()
              newTuple(index) = v
              addToBuff(tupleBS,newTuple, index + 1)
            }
          case NotInSet(vl) =>
            for (v <- x(index).iterator;if !vl.contains(v)){
              val newTuple = tuple.clone()
              newTuple(index) = v
              addToBuff(tupleBS,newTuple, index + 1)
            }
        }
      } else {
        buff += tuple
      }
    }
    var i = table.length
    while (i > 0){
      i -= 1
      addToBuff(table(i),Array.fill(size)(0),0)
    }
    buff.toArray
  }

}
