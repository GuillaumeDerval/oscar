/** *****************************************************************************
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
  * *****************************************************************************/
package oscar.algebra

trait Indexable1[T]{
  def range: Indices[T]
  def name: String
  override def toString = name
}

/** Abstract class for variables */
abstract class Var extends Term[Linear] {
 require(name.nonEmpty)
  def name: String

  def eval(env: Var => Double) = env(this)

  def toExpression = new Expression(Stream(new Prod(Const(1.0), Seq(this))))

  override def toString = name

  //  override def derive(v: Var): Expression = {
  //    if (v equals this) One
  //    else Zero
  //  }

  override def equals(that: Any) = {
    that match {
      case other: Var =>
        other.name equals this.name
      case _ => false
    }
  }

  override def hashCode: Int = name.hashCode
}

trait Indexed{
  def id: Int
}

class Var1Ref[T](val v: Var1[T])(val index: T) extends oscar.algebra.Var with Indexed {
  def name = s"${v.name}[$index]"
  def id = v.id + v.range.indexOf(index)
  def value = None
}

object SymbolicVar1 {
  def apply[T](v: Var1[T])(index: String) = new SymbolicVar1[T](v)(index)
}

class SymbolicVar1[T] private(v: Var1[T])(index: String) extends oscar.algebra.Var {
  def name = s"${v.name}[$index]"

  def value = None
}

class Var2Ref[A,B](val v: Var2[A,B])(val indexA: A, val indexB: B) extends oscar.algebra.Var with Indexed {
  def name = s"${v.name}[$indexA,$indexB]"
  def id = v.id + v.rangeA.indexOf(indexA)*v.rangeB.values.size + v.rangeB.indexOf(indexB)
  def value = None
}

object SymbolicVar2 {
  def apply[A,B](v: Var2[A,B])(indexA: String, indexB: String) = new SymbolicVar2[A,B](v)(indexA,indexB)
}

class SymbolicVar2[A,B] private(v: Var2[A,B])(indexA: String, indexB: String) extends oscar.algebra.Var {
  def name = s"${v.name}[$indexA,$indexB]"

  def value = None
}

trait VarDescription {
  def name: String

  def lowerBound: Option[Double]

  def upperBound: Option[Double]
}

case class Var0(val name: String, val lowerBound: Option[Double] = None, val upperBound: Option[Double] = None)(implicit model: Model[_, _]) extends Var with VarDescription with Indexed {
  val id = model.maxIndex
  model.maxIndex += 1
  model.variables += this -> id
  def value = None
}

case class Var1[T](range: Indices[T], name: String, lowerBound: Option[Double] = None, upperBound: Option[Double] = None)(implicit model: Model[_, _]) extends VarDescription with Indexable1[T]{

  val id = model.maxIndex
  model.maxIndex += range.values.size

  model.variables += this -> id

  //def apply(i: Symbolic) = SymbolicVar(this)(i.name)
  def get(i: T) = new Var1Ref(this)(i)

  def apply(i: Index[T]): Var = {
    i match {
      case SymbolicIndex(name) => SymbolicVar1(this)(name)
      case ConcreteIndex(index) =>
        require(range.values.contains(index))
        // TODO improve this... indexOf is surely not the best...
        new Var1Ref[T](this)(index)
      //Var(id + range.values.indexOf(index),lowerBound,upperBound)
    }
  }
}

case class Var2[A,B](rangeA: Indices[A], rangeB: Indices[B], name: String, lowerBound: Option[Double] = None, upperBound: Option[Double] = None)(implicit model: Model[_, _]) extends VarDescription {


  val id = model.maxIndex

  model.variables += this -> id
  model.maxIndex += rangeA.values.size * rangeB.values.size

  def get(i: A)(j: B) = new Var2Ref(this)(i,j)

  def apply(i: Index[A])(j: Index[B]) = {
//    require(rangeA.values.contains(i._1))
//    require(rangeB.values.contains(j))
    (i,j) match {
      case (ConcreteIndex(indexA),ConcreteIndex(indexB)) => new Var2Ref(this)(indexA,indexB)
      case _ => SymbolicVar2(this)(i.index, j.index)

    }
  }
}



class Param1Ref[T](val param: Param1[T])(val index: T) extends Term[Constant] {
  override def toString = name
  def name = s"${param.name}($index)"
  def value = None
  def eval(env: Var => Double) = param.values(index)

  def toExpression = new Expression(Stream(new Prod(Const(1.0), Seq(this))))
}


object SymbolicParam1 {
  def apply[T](v: Param1[T])(index: String) = new SymbolicParam1[T](v)(index)
}

class SymbolicParam1[T] private(v: Param1[T])(index: String) extends Term[Constant] {
  def name = s"${v.name}[$index]"
  override def toString = name
  def value = None
  //def eval(env: Var => Double) = v.values(index)

  def toExpression = new Expression(Stream(new Prod(Const(1.0), Seq(this))))
}

case class Param1[T](val name: String, val range: Indices[T], val values: Map[T,Double], default: Option[Double] = None)(implicit model: Model[_,_]) extends Indexable1[T]{

  model.params += this

  def apply(i: Index[T]) = {
    i match {
      case SymbolicIndex(name) => SymbolicParam1(this)(name)
      case ConcreteIndex(index) =>
        require(range.values.contains(index))
        // TODO improve this... indexOf is surely not the best...
        new Param1Ref[T](this)(index)
      //Var(id + range.values.indexOf(index),lowerBound,upperBound)
    }
  }

}