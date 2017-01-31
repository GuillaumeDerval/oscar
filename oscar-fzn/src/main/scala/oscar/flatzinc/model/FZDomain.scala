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
/**
  * @author Leonard Debroux
  * @author Gustav Björdal
  * @author Jean-Noël Monette
  */

package oscar.flatzinc.model

import oscar.flatzinc.UnsatException

import scala.collection.immutable.SortedSet


sealed abstract class Domain {
  def min: Int
  def max: Int
  def contains(v:Int): Boolean
  def size: Int
  def boundTo(v: Int) = min == v && max == v
  def geq(v:Int);
  def leq(v:Int);
  def inter(d:Domain):Unit = {
    if(d.isInstanceOf[DomainRange])inter(d.asInstanceOf[DomainRange])
    else inter(d.asInstanceOf[DomainSet])
  }
  def inter(d:DomainRange):Unit = {
    geq(d.min);
    leq(d.max);
  }
  def inter(d:DomainSet):Unit = {
    throw new UnsupportedOperationException("Inter of a Set")
  }
  def checkEmpty() = {
    if (min > max) throw new UnsatException("Empty Domain");
  }
  def toSortedSet: SortedSet[Int]
}

case class DomainRange(var mi: Int, var ma: Int) extends Domain {
  //
  def min = mi
  def max = ma
  def contains(v:Int): Boolean = mi <= v && ma >= v
  def size = if(ma==Helper.FznMaxInt && mi==Helper.FznMinInt) Helper.FznMaxInt else ma-mi+1
  def geq(v:Int) = { mi = math.max(v,mi); checkEmpty() }
  def leq(v:Int) = { ma = math.min(v,ma); checkEmpty() }
  def toRange = mi to ma
  def toSortedSet: SortedSet[Int] = SortedSet[Int]() ++ (mi to ma)
}

case class DomainSet(var values: Set[Int]) extends Domain {
  override def checkEmpty() = {
    if (values.isEmpty) throw new UnsatException("Empty Domain");
  }
  def min = values.min
  def max = values.max
  def size = values.size
  def contains(v:Int): Boolean = values.contains(v)
  def geq(v:Int) = {values = values.filter(x => x>=v); checkEmpty() }
  def leq(v:Int) = {values = values.filter(x => x<=v); checkEmpty() }
  override def inter(d:DomainSet) = {values = values.intersect(d.values); checkEmpty() }
  def toSortedSet: SortedSet[Int] = SortedSet[Int]() ++ values
}