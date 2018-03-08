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
  *     This code has been initially developed by
  *         by Jean Noël Monette, Gustav Björdal
  ******************************************************************************/
package oscar.cbls.core.computation

import scala.collection.immutable.SortedSet
import scala.language.implicitConversions
import scala.util.Random

//TODO: remplacer çà par Option(Int,Int)

object Domain{

  def empty:Domain = Domain(0 to 0)  //TODO: improve!

  implicit def rangeToDomain(r:Range):Domain = {
    DomainRange(r.head,r.last)
  }

  implicit def setToDomain(s:Set[Int]):Domain = {
    DomainSet(s)
  }

  implicit def coupleToDomain(i:(Int,Int)):Domain = {
    if(i._1 == i._2) SingleValueDomain(i._1)
    else DomainRange(i._1,i._2)
  }

  implicit def intToDomain(i:Int) = SingleValueDomain(i)

  def apply(v:Iterable[Int]):Domain =
    v match{
      case r:Range => r
      case s:SortedSet[Int] => s.firstKey to s.lastKey
    }
}

sealed abstract class Domain extends Iterable[Int]{
  def min: Int
  def max: Int
  def size: Int
  def contains(v:Int): Boolean
  //  def intersect(d:Domain):Domain
  def values:Iterable[Int]
  def randomValue():Int
  def restrict(d:Domain):Domain = intersect(d)
  def intersect(d:Domain):Domain

  def union(d:Domain):Domain

  override def iterator: Iterator[Int] = values.iterator
}


/**this is an inclusive domain*/
case class DomainRange(override val min: Int, override val max: Int) extends Domain {
  if (min > max) throw new EmptyDomainException
  def contains(v:Int): Boolean = min <= v && max >= v
  override def size = if(max==Int.MaxValue && min==Int.MinValue) Int.MaxValue else math.max(max-min+1,0)
  override def values: Iterable[Int] = min to max
  override def randomValue(): Int = (min to max)(Random.nextInt(max-min+1))
  override def intersect(d: Domain): Domain = {
    val newDomain:Domain = d match{
      case r:DomainRange => math.max(r.min,min) to math.min(r.max,max)
      case FullRange => this
      case d:SingleValueDomain => d.intersect(this)
      case s:DomainSet =>  s.intersect(this)
    }
    if (newDomain.isEmpty) throw new EmptyDomainException
    newDomain
  }

  override def union(d: Domain): Domain = {
    val newDomain:Domain = d match{
      case r:DomainRange => if((r.max > max && r.min > max) || (max > r.max && min > r.max)){
        (r.min to r.max toSet) union (min to max toSet)
      }else{
        math.min(r.min,min) to math.max(r.max,max)
      }
      case FullRange => FullRange
      case SingleValueDomain(v) => if(contains(v)){
        this
      }else{
        if(v+1 == min){
          v to max
        }else if(v-1 == max){
          min to v
        }else{
          (min to max toSet) union Set(v)
        }
      }
      case d:DomainSet =>  d.union(this)
    }
    if (newDomain.isEmpty) throw new EmptyDomainException
    newDomain
  }

  def toRange:Range = min to max

  override def toString(): String = "DomainRange(min:" + min + ", max:" +  max + ")"
}

case class DomainSet(val s:Set[Int]) extends Domain {
  override def min: Int = s.min
  override def max: Int = s.max
  override def size: Int = s.size
  if (min > max) throw new EmptyDomainException

  def contains(v:Int): Boolean = s.contains(v)
  override def values: Iterable[Int] = s
  override def randomValue(): Int = s.toList.apply(Random.nextInt(size))
  override def intersect(d: Domain): Domain = {
    val newDomain:Domain = d match{
      case r:DomainRange =>
        (r.toRange.toSet) intersect s
      case FullRange => this
      case d:SingleValueDomain => d.intersect(this)
      case ds:DomainSet =>  ds.s intersect s
    }
    if (newDomain.isEmpty) throw new EmptyDomainException
    newDomain
  }

  override def union(d: Domain): Domain = {
    val newDomain:Domain = d match{
      case r:DomainRange => r.toRange.toSet union s
      case FullRange => FullRange
      case SingleValueDomain(v) => if(contains(v)){
        this
      }else{
        Set(v) union s
      }
      case d:DomainSet =>  d.s union s
    }
    if (newDomain.isEmpty) throw new EmptyDomainException
    newDomain
  }

  override def toString(): String = "DomainSet(" + s.mkString(", ") + ")"
}

case object FullRange extends Domain{
  override def min: Int = Int.MinValue
  override def max: Int = Int.MaxValue
  override def size: Int = Int.MaxValue
  override def randomValue(): Int = Random.nextInt()
  override def contains(v: Int): Boolean = true
  override def values: Iterable[Int] =  min to max
  override def intersect(d: Domain): Domain = d
  override def union(d: Domain): Domain = this
  override def toString(): String = "FullRange"
}

object PositiveOrNullRange extends DomainRange(0, Int.MaxValue)


case class SingleValueDomain(value:Int) extends Domain{
  override def min: Int = value
  override def max: Int = value
  override def size: Int = 1
  override def contains(v: Int): Boolean = v == value

  override def randomValue(): Int = value

  override def intersect(d: Domain): Domain =
    if (d.contains(value)) this else throw new EmptyDomainException

  override def union(d: Domain): Domain = {
    d match {
      case SingleValueDomain(v) =>
        if(v == value) this
        else if(math.abs(v-value) == 1) math.min(v,value) to math.max(v,value)
        else Set(v) union Set(value)
      case _ => d.union(this)
    }
  }
  override def values: Iterable[Int] = List(value)

  override def toString(): String = "SingleValueDomain(" + value + ")"
}

class EmptyDomainException extends Exception("domain is empty")

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