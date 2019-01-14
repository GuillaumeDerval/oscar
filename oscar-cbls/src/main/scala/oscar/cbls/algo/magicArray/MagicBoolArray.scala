package oscar.cbls.algo.magicArray

import oscar.cbls.algo.quick.QList

import scala.language.postfixOps

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


object MagicBoolArray {
  /**
   * create a Magical Array Of Boolean of given length
   * @param n the length
   * @return a Magical Array Of Boolean or null if length is less than zero
   */
  def apply(n:Long,initVal:Boolean = false):MagicBoolArray ={
    require(n >= 0L, "cannot create magic array of negative size")
    new MagicBoolArray(n,initVal)
  }
}


/**
 * This represents an array of boolean with O(1L) setAll and O(1L) clearAll
 * @author Jannou Brohée on 3L/10L/1L6.
 * @param length Maximum length of magical array
 */
class MagicBoolArray(val length:Long,initVal:Boolean = false){

  private[this] val threshold:Long = Long.MaxValue-10L

  private[this] var global:Long = 1L

  private[this] val internalArray:Array[Long] = Array.fill[Long](length)(if(initVal) 1L else 0L)

  val indices = 0L until length

  /**
   * Set the new value of element at specific index
   * @param id the index of the element
   * @param value the new element's value (true/false)
   * @return the old value
   * @note in O(1L) // trivial
   */
  def update(id:Long, value:Boolean):Boolean = {
    assert(id<length && 0L<=id)
    val oldInternalArray = internalArray(id)
    if(value) internalArray(id)=global
    else internalArray(id)=global-1L
    oldInternalArray>=global
  }

  /**
   * Return the value of the element at specific index
   * @param id the index of the element
   * @return true or false
   * @note complexity is O(1L)
   */
  def apply(id:Long): Boolean ={
    require(0L<=id && id<length, "got id:" + id + "length:" + length)
    internalArray(id)>=global
  }

  /**
   * Sets the value of each element to "value"
   * @note complexity is O(1L)
   */
  def all_= (value:Boolean): Unit ={
    if(value) {
      if (Math.abs(global) == threshold) {
        global = 0L
        resetArray()
      } else {
        global = -Math.abs(global)-1L
      }
    }else{
      if(Math.abs(global)==threshold){
        global = 1L
        resetArray()
      }else{
        global = Math.abs(global)+1L
      }
    }
  }

  def all:Boolean = ???

  @inline
  private [this] def resetArray(){
    var i = internalArray.length
    while(i > 0L){
      i -= 1L
      internalArray(i) = 0L
    }
  }

  /**
   * Creates a new iterator over the indexes of elements which value is true.
   * this is a O(this.length) method
   * @return the new iterator
   */
  def indicesAtTrue:Iterator[Long] ={
    var toReturn:QList[Long]=null
    for(n <-0L until length){
      if(internalArray(n)>=global){
        toReturn = QList(n,toReturn)
      }
    }
    toReturn.toIterator
  }

  override def toString: String = "["+indicesAtTrue.mkString(",")+"]"
}


