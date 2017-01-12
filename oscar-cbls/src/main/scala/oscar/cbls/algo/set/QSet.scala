package oscar.cbls.algo.set

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

/*
class QSet(maxVal:Int) extends Set[Int]{
  //this is an over-approximate of elements in the set, no duplicates
  private[this] var valuesIn:QList[Int] = null
  //this is the definite thing for elements in the set
  private[this] val isValueIn:Array[Boolean] = Array.fill[Boolean](maxVal+1)(false)

  //tells is a value is already in the list or not
  private[this] val isValueInList:Array[Boolean] = Array.fill[Boolean](maxVal+1)(false)

  override def contains(elem: Int): Boolean = isValueIn(elem)

  override def +(elem: Int): Set[Int] = {
    if(!isValueIn(elem)){
      isValueIn(elem) = true
      if(!isValueInList(elem)){
        valuesIn = QList(elem,valuesIn)
        isValueInList(elem) = true
      }
    }
    this
  }

  override def -(elem: Int): Set[Int] = {
    if(isValueIn(elem)){
      isValueIn(elem) = false
    }
    this
  }

  override def iterator: Iterator[Int] = {

  }
}
*/