package oscar.cbls.algo.quick
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

import scala.language.implicitConversions

class QList[@specialized T](val head:T, val tail:QList[T] = null){
  def size:Long = {
    var curr = this.tail
    var toReturn = 1L
    while(curr != null){
      curr = curr.tail
      toReturn += 1L
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

  def qMap[X](fun:T => X):QList[X] = new QList(fun(head),if(tail == null) null else tail.qMap(fun))
}

object QList{

  def append[T](l:Iterable[T],q:QList[T]):QList[T] = {
    val it = l.toIterator

    var toReturn = q
    while(it.hasNext){
      toReturn = QList(it.next,toReturn)
    }
    toReturn
  }

  def nonReversedAppend[T](l:Iterable[T],q:QList[T]):QList[T] = {

    def prependValue(it: Iterator[T], q:QList[T]): QList[T] ={
      val next = it.next()
      QList(next,
        if(it.hasNext) {
          prependValue(it,q)
        } else {
          q
        })
    }
    if(l.nonEmpty)
      prependValue(l.toIterator,q)
    else q
  }


  def apply[T](head:T,tail:QList[T] = null):QList[T] = new QList(head,tail)

  implicit def toIterable[T](l:QList[T]):Iterable[T] = new IterableQList(l)

  def buildFromIterable[T](l:Iterable[T]):QList[T] = {
    var acc:QList[T] = null
    val it = l.toIterator
    while(it.hasNext){
      acc = QList(it.next(),acc)
    }
    acc
  }

  def nonReversedBuildFromIterable[T](l:Iterable[T]):QList[T] ={
    val reversed = buildFromIterable(l)
    if(reversed == null) null
    else reversed.reverse
  }

  def qMap[T,X](q:QList[T],fun:T => X):QList[X] = {
    if(q == null) null
    else q.qMap(fun)
  }

  def qFold[T,@specialized(Long)X](q:QList[T],accF:(X,T) => X,initX:X):X = {
    var l = q
    var acc = initX
    while(l != null){
      acc = accF(acc,l.head)
      l = l.tail
    }
    acc
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
