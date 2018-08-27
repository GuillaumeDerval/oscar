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

  def qMap[X](fun:T => X):QList[X] = new QList(fun(head),if(tail == null) null else tail.qMap(fun))

  def append(tail:QList[T]):QList[T] = {
    if(this.tail == null) QList(head,tail)
    else QList(head, this.tail.append(tail))
  }
}

object QList{

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

  def qMap[T,X](q:QList[T],fun:T => X):QList[X] = {
    if(q == null) null
    else q.qMap(fun)
  }

  def append[T](a:QList[T],b:QList[T]):QList[T] = {
    if(a == null) b
    else a append b
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




object QListBuilder{
  def base[T](l:QList[T]):QListBuilder[T] = new QListBuilderLeaf[T](l)
  def acc[T](l:QList[QListBuilder[T]]):QListBuilder[T] = new QListBuilderNode[T](l)

  def apply[T](builders:QListBuilder[T]*) = {
    var filtered:QList[QListBuilder[T]] = null
    for(b <- builders){
      if(b != null && !b.isEmpty)
        filtered = new QList(b,filtered)
    }
    new QListBuilderNode(filtered)
  }


  def empty[T]():QListBuilder[T] = base[T](null)
}

abstract class QListBuilder[T]{
  def flatten:QList[T] = toListAcc(null)
  def toListAcc(tail:QList[T]):QList[T]
  def isEmpty:Boolean
}

class QListBuilderLeaf[T](content:QList[T]) extends QListBuilder[T]{
  override def toListAcc(tail: QList[T]): QList[T] = {
    QList.append(content,tail)
  }

  override def isEmpty: Boolean = content != null
}

class QListBuilderNode[T](content:QList[QListBuilder[T]]) extends QListBuilder[T]{

  def isEmpty: Boolean = {
    var toCheck = content
    while(toCheck != null){
      if(!toCheck.head.isEmpty) return false
      toCheck = toCheck.tail
    }
    true
  }

  override def toListAcc(tail: QList[T]): QList[T] = {
    def myToListAcc(myList:QList[QListBuilder[T]]):QList[T] = {
      if(myList == null) tail
      else myList.head.toListAcc(myToListAcc(myList.tail))
    }
    myToListAcc(content)
  }
}
