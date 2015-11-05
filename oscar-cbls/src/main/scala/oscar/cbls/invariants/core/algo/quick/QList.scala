package oscar.cbls.invariants.core.algo.quick

import scala.language.implicitConversions

class QList[@specialized T](val head:T, val tail:QList[T] = null){
  def size:Int = {
    var curr = this.tail
    var toReturn = 1
    while(curr != null){
      curr = curr.tail
      toReturn += 1
    }
    toReturn
  }

  def reverse:QList[T] = {
    var toReturn:QList[T] = null
    var current = this
    while(current != null){
      toReturn = QList(current.head,toReturn)
      current = current.tail
    }
    toReturn
  }
  //@deprecated("really, you should not do this","ever")
  def toIterable:Iterable[T] = new IterableQList(this)

  def toIterator:Iterator[T] = new QListIterator[T](this)
}

object QList{
  @inline
  def apply[T](head:T,tail:QList[T] = null):QList[T] = new QList(head,tail)
  //@deprecated("really, you should not do this","ever")
  implicit def toIterable[T](l:QList[T]):Iterable[T] = new IterableQList(l)

  def buildFromIterable[T](l:Iterable[T]):QList[T] = {
    def buildFromIterator[T](l:Iterator[T]):QList[T] = {
      if(l.hasNext) QList(l.next(),buildFromIterator(l))
      else null
    }
    buildFromIterator(l.toIterator)
  }
}

class IterableQList[@specialized T](l:QList[T]) extends Iterable[T]{

  override def foreach[U](f: (T) => U): Unit = {
    var currentpos = l
    while(currentpos != null){
      f(currentpos.head)
      currentpos = currentpos.tail
    }
  }

  override def iterator: Iterator[T] = new QListIterator(l)
}

class QListIterator[@specialized T](var currentPos:QList[T]) extends Iterator[T]{
  override def hasNext: Boolean = currentPos != null

  override def next(): T = {
    val toReturn = currentPos.head
    currentPos = currentPos.tail
    toReturn
  }
}
