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

package oscar.cbls.core.draft.computation

import scala.language.implicitConversions
import scala.util.Random


object Domain{

  implicit def rangeToDomain(r:Range):Domain = {
    Domain(r.head,r.last)
  }

  implicit def coupleToDomain(i:(Int,Int)):Domain = {
    Domain(i._1,i._2)
  }

  implicit def apply(v:Iterable[Int]):Domain = v.min to v.max

  val positiveOrNull:Domain= Domain(0, Int.MaxValue)

  def singleton(v:Int):Domain = Domain(v,v)

  val fullRange:Domain = Domain(Int.MinValue, Int.MaxValue)
}

//this represents a domain; basically a min-max bound.
//there is no size because max-min can overflow.
//you can use the sizeOverflows method to know about that
case class Domain(min:Int,max:Int){
  require(max >= min, "domain is empty")

  def contains(v:Int): Boolean = min <= v && max >= v

  val sizeOVerflows = min.toLong + Int.MaxValue > max.toLong

  def sizeOpt:Option[Int]= if (sizeOVerflows) None else Some(max - min + 1)

  def size:Int = if (sizeOVerflows) throw new Error("size is too large to fit in Int") else max - min + 1

  def randomValue(): Int = (min to max)(Random.nextInt(max-min+1))

  def intersect(d: Domain): Domain = {
    math.max(d.min,min) to math.min(d.max,max)
  }

  def union(d: Domain): Domain = {
    math.min(d.min,min) to math.max(d.max,max)
  }

  def toRange:Range = min to max

  def values:Iterable[Int] = min to max

  def iterator: Iterator[Int] = values.iterator

  override def toString(): String = "Domain(min:" + min + ", max:" +  max + (if (sizeOVerflows) " overflow on size" else "") +")"
}


object DomainHelper{

  def safeAddMax(a:Int,b:Int):Int = {
    val tmp = a.toLong + b.toLong
    if(tmp > Int.MaxValue) Int.MaxValue
    else tmp.toInt
  }

  def safeAddMin(a:Int,b:Int):Int = {
    val tmp = a.toLong + b.toLong
    if(tmp < Int.MinValue) Int.MinValue
    else tmp.toInt
  }

  def safeMulMax(a:Int,b:Int):Int = {
    val tmp = a.toLong * b.toLong
    if(tmp > Int.MaxValue) Int.MaxValue
    else tmp.toInt
  }

  def safeMulMin(a:Int,b:Int):Int = {
    val tmp = a.toLong * b.toLong
    if(tmp < Int.MinValue) Int.MaxValue
    else tmp.toInt
  }
}