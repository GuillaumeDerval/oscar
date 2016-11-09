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

  /* ------------   Implicits ----------------------- */

  implicit def double2const(d : Double) : NormalizedExpression[Constant,Double] = Const(d).normalized
  implicit def int2const(d : Int) : NormalizedExpression[Constant,Double] = Const(d.toDouble).normalized

  class ModelDecorator[O  >: Constant <: ExpressionDegree, C <: ExpressionDegree, V: Numeric](val model: Model[O,C,V]){
    def solve(implicit solver: SolverInterface[O,C,V]): ModelStatus[O, C, V] = {
      val run = solver.run(model)
      val status = run.solve
      run.release()
      status
    }
  }

  implicit def model2decorator[O  >: Constant <: ExpressionDegree, C <: ExpressionDegree, V: Numeric](model: Model[O,C,V]): ModelDecorator[O, C, V] = new ModelDecorator(model)

  def maximize[O  >: Constant <: ExpressionDegree, V: Numeric](obj: NormalizedExpression[O,V])(implicit model: Model[O,_,V]): Unit = model.withObjective(new Maximize[O,V](obj))
  def minimize[O  >: Constant <: ExpressionDegree, V: Numeric](obj: NormalizedExpression[O,V])(implicit model: Model[O,_,V]): Unit = model.withObjective(new Minimize[O,V](obj))

  def subjectTo[C <: ExpressionDegree, V: Numeric](constraints: Equation[C, V]*)(implicit model: Model[_,C,V]): Unit = {
    for (c <- constraints) model.subjectTo(c)
  }

  type LinearExpression = NormalizedExpression[Linear,Double]
  type LinearConstraintExpression = Equation[Linear,Double]

  implicit def term2Expression[T <: ExpressionDegree,V:Numeric](term: Term[T,V]): NormalizedExpression[T,V] = term.normalized

  /* ------------   Linear algebra ----------------------- */
  
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
    (for (i <- indexes1; j <- indexes2; k <- indexes3; l <- indexes4) yield f(i, j, k, l)).sum
  }

  def createVarMap[T,V](ts:Seq[T])(varConstr:T => V): Map[T,V] = { (for (t<-ts) yield t-> varConstr(t)).toMap }


  /* ------------------------- Linear Expressions & Constraints ------------------- */

  def sumOf[T <: ExpressionDegree,V:Numeric](exprs : Iterable[NormalizedExpression[T,V]]) : NormalizedExpression[T,V] = {
    new NormalizedExpression(exprs.toStream.flatMap(_.terms))
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

  // TODO implement these functions
  def abs(expr: NormalizedExpression[_,Double])(implicit model: Model[Linear,Linear,Double]): NormalizedExpression[Linear,Double] = ???

  def sign(expr: NormalizedExpression[_,Double])(implicit model: Model[Linear,Linear,Double]): NormalizedExpression[Linear,Double] = ???

}
