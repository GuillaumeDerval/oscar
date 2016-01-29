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

import oscar.algebra.linear.{Zero, Var, Const, LinearExpression}


package object algebra {
  
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

  def sum(exprs : Iterable[LinearExpression]) : LinearExpression = {
    import scala.collection.mutable.Map
    val mymap = Map[Var,Double]()
    var mycte = 0.0
    for (expr <- exprs) {
      mycte += expr.cte
      for((k,v) <- expr.coef) {
        mymap.get(k) match {
           case Some(c) => mymap(k) = c+v
           case None => mymap += (k -> v)
        }
      }
    }
    import scala.collection.immutable.Map
    mymap.filterNot(_._2 == 0)
    new LinearExpression() {
      val cte = mycte
      val coef = mymap.toMap
    }
    //exprs.foldLeft(Zero : LinearExpression)(_ + _)
  }
  
  /**
   * sum[a <- A] f(a)
   */ 
  def sum[A](indexes : Iterable[A])(f : A => LinearExpression) : LinearExpression = sum(indexes map f)
  
  /**
   * sum[a <- A, b <- B] f(a,b)
   */ 
  def sum[A,B](indexes1 : Iterable[A],indexes2 : Iterable[B])(f : (A,B) => LinearExpression) : LinearExpression = {
         sum(for(i <- indexes1;j <- indexes2) yield f(i,j))
  }
  
  /**
   * sum[a <- A, b <- B, c <- C] f(a,b,c)
   */ 
  def sum[A,B,C](indexes1 : Iterable[A],indexes2 : Iterable[B], indexes3 : Iterable[C])(f : (A,B,C) => LinearExpression) : LinearExpression = {
    	sum(for(i <- indexes1;j <- indexes2; k <- indexes3) yield f(i,j,k))
  }

  /**
   * sum[a <- A, b <- B, c <- C, d <- D] f(a,b,c,d)
   */ 
  def sum[A,B,C,D](indexes1 : Iterable[A],indexes2 : Iterable[B], indexes3 : Iterable[C], indexes4 : Iterable[D])(f : (A,B,C,D) => LinearExpression) : LinearExpression = {
    	sum(for(i <- indexes1;j <- indexes2; k<- indexes3; l <- indexes4) yield f(i,j,k,l))
  }
  
  /**
   * sum[a <- A such that filter(a) == true] f(a)
   */
  def sum[A](S1:Iterable[A], filter: A => Boolean, f:(A) => LinearExpression): LinearExpression = {
       sum(for (a <- S1; if(filter(a)))  yield f(a))  
  }
  
  /**
   * sum[a <- A, b <- B such that filter(a,b) == true] f(a,b)
   */
  def sum[A,B](S1:Iterable[A],S2:Iterable[B], filter: (A,B) => Boolean, f:(A,B) => LinearExpression): LinearExpression = {
       sum(for (a <- S1; b <- S2; if(filter(a,b)))  yield f(a,b))  
  }

  /**
   * sum[a <- A, b <- B, c <- C such that filter(a,b,c) == true] f(a,b,c)
   */  
  def sum[A,B,C](S1:Iterable[A],S2:Iterable[B],S3:Iterable[C], filter: (A,B,C) => Boolean, f:(A,B,C) => LinearExpression): LinearExpression = {
       sum(for (a <- S1; b <- S2; c <- S3; if(filter(a,b,c)))  yield f(a,b,c))  
  }
  
  /**
   * sum[a <- A, b <- B, c <- C, d <- D such that filter(a,b,c,d) == true] f(a,b,c,d)
   */    
  def sum[A,B,C,D](S1:Iterable[A],S2:Iterable[B],S3:Iterable[C],S4:Iterable[D], filter: (A,B,C,D) => Boolean, f:(A,B,C,D) => LinearExpression): LinearExpression = {
       sum(for (a <- S1; b <- S2; c <- S3; d <- S4; if(filter(a,b,c,d)))  yield f(a,b,c,d))  
  }  
  
  // ------------   Implicits -----------------------
  
  implicit def double2const(d : Double) : Const = if (d == 0.0) Zero else Const(d)
  implicit def int2const(i : Int) : Const = double2const(i)
  
  // ------------------------- general mathematical expressions -------------------

  
  //Non linear functions
  def log(expr : Expression) = new Log(expr)
  def sq(expr:Expression) = expr*expr
  //Trigo
  def cos(expr: Expression) = new Cos(expr)
  def sin(expr: Expression) = new Sin(expr)
  def tan(expr: Expression) = new Tan(expr) 
  
//  def sum(exprs : Iterable[Expression]) : Expression = exprs.foldLeft(Zero : Expression)(_ + _)
//  
//  def sum[A](indexes : Iterable[A])(f : A => Expression) : Expression = sum(indexes map f)

  
  // -------------------- constraints --------------------
  


}
