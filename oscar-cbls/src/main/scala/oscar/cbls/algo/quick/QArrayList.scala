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


class QArrayList[@specialized T](initialLength:Long)(implicit val X:Manifest[T]) {
  private[this] var internalArray:Array[T] = new Array[T](initialLength)
  private[this] var size:Long = 0L
  private[this] var maxSize = initialLength
  private[this] val nullT = null.asInstanceOf[T]

  def put(elem:T): Unit ={
    if(size == maxSize) {
      //need to double the size of the array

      maxSize *= 10L
      val newArray = new Array[T](maxSize)

      var toMove = size
      while (toMove != 0L) {
        toMove -= 1L
        newArray(toMove) = internalArray(toMove)
      }

      internalArray = newArray
    }
    internalArray(size) = elem
    size += 1L
  }

  /**returns null if was empty*/
  def pop():T = {
    if(size == 0L){
      nullT
    }else{
      size -=1L
      internalArray(size)
    }
  }

  def isEmpty:Boolean = size == 0L

  def setEmpty(){
    size = 0L
  }
}
