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
package oscar


package object algebra {

  type LinearExpression = NormalizedExpression[Linear,Double]
  type LinearConstraintExpression = Equation[Linear,Double]

//  implicit def int2Integral(i: Int) = new Integral(i)
//  implicit def double2Continuous(d: Double) = new Continuous(d)
//
//  implicit object IntegralIsNumeric extends Numeric[Integral]{
//    override def plus(x: Integral, y: Integral): Integral = x.intValue+y.intValue
//
//    override def minus(x: Integral, y: Integral): Integral = x.intValue-y.intValue
//
//    override def times(x: Integral, y: Integral): Integral = x.intValue*y.intValue
//
//    override def negate(x: Integral): Integral = new Integral(-x.intValue)
//
//    override def fromInt(x: Int): Integral = new Integral(x)
//
//    override def toInt(x: Integral): Int = x.intValue
//
//    override def toLong(x: Integral): Long = x.intValue
//
//    override def toFloat(x: Integral): Float =   x.intValue
//
//    override def toDouble(x: Integral): Double = x.intValue.toDouble
//
//    override def compare(x: Integral, y: Integral): Int = implicitly[Numeric[Int]].compare(x.intValue,y.intValue)
//  }
//
//  implicit object ContinuousIsIsNumeric extends Numeric[Continuous]{
//    override def plus(x: Continuous, y: Continuous): Continuous = x.doubleValue+y.doubleValue
//
//    override def minus(x: Continuous, y: Continuous): Continuous = x.doubleValue-y.doubleValue
//
//    override def times(x: Continuous, y: Continuous): Continuous = x.doubleValue*y.doubleValue
//
//    override def negate(x: Continuous): Continuous = new Continuous(-x.doubleValue)
//
//    override def fromInt(x: Int): Continuous = new Continuous(x)
//
//    override def toInt(x: Continuous): Int = x.doubleValue.toInt
//
//    override def toLong(x: Continuous): Long = x.doubleValue.toLong
//
//    override def toFloat(x: Continuous): Float =   x.doubleValue.toFloat
//
//    override def toDouble(x: Continuous): Double = x.doubleValue
//
//    override def compare(x: Continuous, y: Continuous): Int = implicitly[Numeric[Double]].compare(x.doubleValue,y.doubleValue)
//  }
  
  implicit def term2Expression[T <: ExpressionDegree,V:Numeric](term: Term[T,V]) = term.normalized
  //implicit def double2Expression[T <: AnyType](d: Double) = Const(d).toExpression

  //implicit def value2ConcreteIndex[T](value: T) = ConcreteIndex(value)


  // some useful linear algebra functions
  
  def min(a:Double,b:Double): Double = {a.min(b)}
  def max(a:Double,b:Double): Double = {a.max(b)}
  
  
  def sumNum[A](indexes1:Iterable[A])(f: A => Double) : Double = {
    (for(i<-indexes1) yield f(i)).sum
  }
  
  def sumNum[A,B](indexes1 : Iterable[A],indexes2 : Iterable[B])(f: (A,B) => Double) : Double = {
    (for(i<-indexes1;j<-indexes2) yield f(i,j)).sum
  }
  
  def sumNum[A,B,C](indexes1 : Iterable[A],indexes2 : Iterable[B], indexes3: Iterable[C])(f: (A,B,C) => Double) : Double = {
    (for(i<-indexes1;j<-indexes2;k<-indexes3) yield f(i,j,k)).sum
  }
  
  def sumNum[A,B,C,D](indexes1 : Iterable[A],indexes2 : Iterable[B], indexes3 : Iterable[C], indexes4 : Iterable[D])(f : (A,B,C,D) => Double) : Double = {
    (for(i <- indexes1;j <- indexes2; k<- indexes3; l <- indexes4) yield f(i,j,k,l)).sum
  } 
  
  def createVarMap[T,V](ts:Seq[T])(varConstr:T=>V): Map[T,V] = { (for (t<-ts) yield t-> varConstr(t)).toMap }  
  
  
  // -------------------------  linear expressions & constraints -------------------

  def sumOf[T <: ExpressionDegree,V:Numeric](exprs : Iterable[NormalizedExpression[T,V]]) : NormalizedExpression[T,V] = {
    new NormalizedExpression(exprs.toStream.map(_.terms).flatten)
  }

  /**
   * sum[a <- A] f(a)
   */
  def sum[A,T <: ExpressionDegree,V:Numeric](indexes : Iterable[A])(f : A => NormalizedExpression[T,V]) : NormalizedExpression[T,V] = sumOf(indexes map f)

  /**
   * sum[a <- A, b <- B] f(a,b)
   */
  def sum[A,B,T <: ExpressionDegree,V:Numeric](indexes1 : Iterable[A], indexes2 : Iterable[B])(f : (A,B) => NormalizedExpression[T,V]) : NormalizedExpression[T,V] = {
         sumOf(for(i <- indexes1; j <- indexes2) yield f(i,j))
  }

  /**
   * sum[a <- A, b <- B, c <- C] f(a,b,c)
   */
  def sum[A,B,C,T <: ExpressionDegree,V:Numeric](indexes1 : Iterable[A], indexes2 : Iterable[B], indexes3 : Iterable[C])(f : (A,B,C) => NormalizedExpression[T,V]) : NormalizedExpression[T,V] = {
    	sumOf(for(i <- indexes1; j <- indexes2; k <- indexes3) yield f(i,j,k))
  }

  /**
   * sum[a <- A, b <- B, c <- C, d <- D] f(a,b,c,d)
   */
  def sum[A,B,C,D,T <: ExpressionDegree,V:Numeric](indexes1 : Iterable[A], indexes2 : Iterable[B], indexes3 : Iterable[C], indexes4 : Iterable[D])(f : (A,B,C,D) => NormalizedExpression[T,V]) : NormalizedExpression[T,V] = {
    	sumOf(for(i <- indexes1; j <- indexes2; k<- indexes3; l <- indexes4) yield f(i,j,k,l))
  }
//
//  /**
//   * sum[a <- A such that filter(a) == true] f(a)
//   */
//  def sum[A](S1:Iterable[A], filter: A => Boolean, f:(A) => LinearExpression): LinearExpression = {
//       sum(for (a <- S1; if(filter(a)))  yield f(a))
//  }
//
//  /**
//   * sum[a <- A, b <- B such that filter(a,b) == true] f(a,b)
//   */
//  def sum[A,B](S1:Iterable[A],S2:Iterable[B], filter: (A,B) => Boolean, f:(A,B) => LinearExpression): LinearExpression = {
//       sum(for (a <- S1; b <- S2; if(filter(a,b)))  yield f(a,b))
//  }
//
//  /**
//   * sum[a <- A, b <- B, c <- C such that filter(a,b,c) == true] f(a,b,c)
//   */
//  def sum[A,B,C](S1:Iterable[A],S2:Iterable[B],S3:Iterable[C], filter: (A,B,C) => Boolean, f:(A,B,C) => LinearExpression): LinearExpression = {
//       sum(for (a <- S1; b <- S2; c <- S3; if(filter(a,b,c)))  yield f(a,b,c))
//  }
//
//  /**
//   * sum[a <- A, b <- B, c <- C, d <- D such that filter(a,b,c,d) == true] f(a,b,c,d)
//   */
//  def sum[A,B,C,D](S1:Iterable[A],S2:Iterable[B],S3:Iterable[C],S4:Iterable[D], filter: (A,B,C,D) => Boolean, f:(A,B,C,D) => LinearExpression): LinearExpression = {
//       sum(for (a <- S1; b <- S2; c <- S3; d <- S4; if(filter(a,b,c,d)))  yield f(a,b,c,d))
//  }
  
  // ------------   Implicits -----------------------
  
  implicit def double2const[V:Numeric](d : V) : NormalizedExpression[Constant,V] = Const(d)

  // ------------------------- general mathematical expressions -------------------

//
//  //Non linear functions
//  def log(expr : Expression) = new Log(expr)
//  def sq(expr:Expression) = expr*expr
//  //Trigo
//  def cos(expr: Expression) = new Cos(expr)
//  def sin(expr: Expression) = new Sin(expr)
//  def tan(expr: Expression) = new Tan(expr)
  
  //def sum[T >: Constant <: AnyType](exprs : Iterable[Expression[T]]) : Expression[T] = exprs.foldLeft(Const(0.0) : Expression[T])(_ + _)

  //def sum[A, T <: AnyType](indexes : Iterable[A])(f : A => Expression[T]) : Expression[T] = sum[T](indexes map f)

  
  // -------------------- constraints --------------------
  


}
