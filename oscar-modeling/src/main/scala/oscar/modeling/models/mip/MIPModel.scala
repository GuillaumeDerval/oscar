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

package oscar.modeling.models.mip

import de.xypron.linopt.Problem
import oscar.modeling.algebra
import oscar.modeling.algebra.bool.{BoolExpression, _}
import oscar.modeling.algebra.floating.{FloatExpression, IntExpressionToFloat}
import oscar.modeling.algebra.integer.IntExpression
import oscar.modeling.constraints.{AllDifferent, Constraint, ExpressionConstraint}
import oscar.modeling.models._
import oscar.modeling.vars.{FloatVar, IntVar}
import oscar.modeling.vars.mip.{MIPFloatVar, MIPIntVar, MIPVar}

import scala.collection.mutable

object MIPModel {
  def apply(implicit modelDeclaration: ModelDeclaration): MIPModel = {
    new MIPModel(modelDeclaration.getCurrentModel.asInstanceOf[UninstantiatedModel])
  }
}

/**
  * Model for Linear Programming
 *
  * @param p: the model from which to inherit
 */
class MIPModel(p: UninstantiatedModel) extends InstantiatedModel(p) {
  implicit lazy val linProblem = new Problem()
  private var currentEqId = 0
  private var currentVarId = 0
  private lazy val MAX_VALUE =  10000.0
  private lazy val MIN_VALUE = -10000.0

  override type IntVarImplementation = MIPIntVar
  override type FloatVarImplementation = MIPFloatVar
  var hasSolution = false //to be defined by the MIP Solver when there is a solution

  private def generateVarNameIfNeeded(name: String) = {
    if(name == null || name.isEmpty) {
      currentVarId += 1
      "var"+currentVarId.toString
    }
    else
      name
  }

  private def getNewRow(): Problem#Row = {
    currentEqId += 1
    linProblem.row("cst"+currentEqId.toString)
  }

  override protected def instantiateIntVar(content: Iterable[Int], name: String): MIPIntVar = {
    val max = content.max
    val min = content.min
    if(max - min + 1 != content.size)
      throw new RuntimeException("MIP Integer variable requires range domains")
    MIPIntVar(min, max, generateVarNameIfNeeded(name))(this)
  }

  override protected def instantiateFloatVar(min: Double, max: Double, name: String): MIPFloatVar = MIPFloatVar(min, max, generateVarNameIfNeeded(name))(this)

  override def post(constraint: Constraint): Unit = constraint match {
    case ExpressionConstraint(expr) => postBooleanExpr(expr)
    case AllDifferent(expressions) => {
      val allValues = expressions.flatMap(_.values().map(_.toDouble)).distinct
      var nB = 0
      val matrix = Array.tabulate(expressions.length, allValues.length)((x, y) => {
        nB += 1
        linProblem.column(currentVarId.toString + "ad("+x+","+y+")").`type`(Problem.ColumnType.INTEGER).bounds(0.0, 1.0)
      })
      currentVarId += 1

      for(i <- expressions.indices) {
        val row = getNewRow().bounds(1.0, 1.0)
        for(j <- matrix(i).indices)
          row.add(1.0, matrix(i)(j))
      }

      for(i <- allValues.indices) {
        val row = getNewRow().bounds(0.0, 1.0)
        for(j <- expressions.indices)
          row.add(1.0, matrix(j)(i))
      }

      for(i <- expressions.indices) {
        val row = getNewRow().bounds(0.0, 0.0)
        val left = allValues.zip(matrix(i)).toList
        val right = getLinearExpression(expressions(i))
        pushForRow(row, left ++ negate(right))
      }
    }
    case _ => throw new RuntimeException("No support for constraint "+constraint.getClass.getName+" in linear programming")
  }

  private def postBooleanExpr(expr: BoolExpression): Unit = {
    expr match {
      case Eq(array) =>
        val expressions = array.map(getLinearExpression)
        for(i <- expressions.indices)
          for(j <- i+1 until expressions.length) {
            pushForRow(getNewRow().bounds(0.0, 0.0), expressions(i) ++ negate(expressions(j)))
          }
      case GrEq(a,b) =>
        val exprA = getLinearExpression(a)
        val exprB = getLinearExpression(b)
        pushForRow(getNewRow().bounds(0.0, MAX_VALUE), exprA ++ negate(exprB))
      case LrEq(a,b) =>
        val exprA = getLinearExpression(a)
        val exprB = getLinearExpression(b)
        pushForRow(getNewRow().bounds(MIN_VALUE, 0.0), exprA ++ negate(exprB))
      case Gr(a,b) =>
        val exprA = getLinearExpression(a)
        val exprB = getLinearExpression(IntExpressionToFloat(b)+0.5)
        pushForRow(getNewRow().bounds(0.0, MAX_VALUE), exprA ++ negate(exprB))
      case Lr(a,b) =>
        val exprA = getLinearExpression(a)
        val exprB = getLinearExpression(IntExpressionToFloat(b)-0.5)
        pushForRow(getNewRow().bounds(MIN_VALUE, 0.0), exprA ++ negate(exprB))
      case EqFloat(array) =>
        val expressions = array.map(getLinearExpression)
        for(i <- expressions.indices)
          for(j <- i+1 until expressions.length) {
            pushForRow(getNewRow().bounds(0.0, 0.0), expressions(i) ++ negate(expressions(j)))
          }
      case GrEqFloat(a,b) =>
        val exprA = getLinearExpression(a)
        val exprB = getLinearExpression(b)
        pushForRow(getNewRow().bounds(0.0, MAX_VALUE), exprA ++ negate(exprB))
      case LrEqFloat(a,b) =>
        val exprA = getLinearExpression(a)
        val exprB = getLinearExpression(b)
        pushForRow(getNewRow().bounds(MIN_VALUE, 0.0), exprA ++ negate(exprB))
      case _ => throw new RuntimeException("No support for expression "+expr.getClass.getName+" in linear programming")
    }
  }

  private def negate(exprs: List[(Double, Problem#Column)]) = exprs.map(x => (-x._1, x._2))

  private def getLinearExpression(expr: IntExpression): List[(Double, Problem#Column)] = expr match {
    case algebra.integer.Constant(c) => List((c, null))
    case algebra.integer.Minus(a, b) => getLinearExpression(a) ++ negate(getLinearExpression(b))
    case algebra.integer.Sum(array) => array.flatMap(getLinearExpression).toList
    case algebra.integer.UnaryMinus(a) => negate(getLinearExpression(a))
    case algebra.integer.Prod(array) =>
      var mainExpr: IntExpression = null
      var multiplier = 1.0
      for(expr2 <- array) expr2 match {
        case algebra.integer.Constant(b) => multiplier *= b
        case bound if bound.isBound => multiplier *= bound.evaluate()
        case other =>
          if(mainExpr == null)
            mainExpr = other
          else
            throw new RuntimeException("Only product with constants are allowed")
      }
      if(mainExpr == null)
        List((multiplier, null))
      else
        getLinearExpression(mainExpr).map(x => (x._1*multiplier, x._2))
    case v: IntVar => List((1, intRepresentatives.get(v).realMipVar))
    case _ => throw new RuntimeException("No support for expression "+expr.getClass.getName+" in MIP")
  }

  private def getLinearExpression(expr: FloatExpression): List[(Double, Problem#Column)] = expr match {
    case algebra.floating.Constant(c) => List((c, null))
    case algebra.floating.Minus(a, b) => getLinearExpression(a) ++ negate(getLinearExpression(b))
    case algebra.floating.Sum(array) => array.flatMap(getLinearExpression).toList
    case algebra.floating.UnaryMinus(a) => negate(getLinearExpression(a))
    case algebra.floating.IntExpressionToFloat(a) => getLinearExpression(a)
    case algebra.floating.Prod(array) =>
      var mainExpr: FloatExpression = null
      var multiplier = 1.0
      for(expr2 <- array) expr2 match {
        case algebra.floating.Constant(b) => multiplier *= b
        case bound if bound.isBound => multiplier *= bound.evaluate()
        case other =>
          if(mainExpr == null)
            mainExpr = other
          else
            throw new RuntimeException("Only product with constants are allowed")
      }
      if(mainExpr == null)
        List((multiplier, null))
      else
        getLinearExpression(mainExpr).map(x => (x._1*multiplier, x._2))
    case v: FloatVar => List((1, floatRepresentatives.get(v).realMipVar))
    case _ => throw new RuntimeException("No support for expression "+expr.getClass.getName+" in MIP")
  }

  private def pushForRow(row: Problem#Row, exprs: List[(Double, Problem#Column)]): Unit = {
    //First, let's extract the constant
    var constant: Double = exprs.filter(_._2 == null).map(_._1).sum
    //Correct the bound
    if(row.getLowerBound != null && row.getLowerBound != MIN_VALUE)
      row.setLowerBound(row.getLowerBound-constant)
    if(row.getUpperBound != null && row.getUpperBound != MAX_VALUE)
      row.setUpperBound(row.getUpperBound-constant)

    //The GLPK lib imposes that we cannot add the same variable twice.
    //Push the remaining variables
    val hashMap = mutable.HashMap[Problem#Column, Double]()

    for((mult, vari) <- exprs; if vari != null)
      hashMap.put(vari, hashMap.getOrElse(vari, 0.0)+mult)

    for((vari, mult) <- hashMap)
      row.add(mult, vari)
  }

  override protected def postObjective(optimisationMethod: OptimisationMethod): Unit = optimisationMethod match {
    case MinimisationFloat(o) => {
      val obj = linProblem.objective("__obj__", Problem.Direction.MINIMIZE)
      //obj.bounds(o.min, o.max)
      pushForRow(obj, getLinearExpression(o))
    }
    case MaximisationFloat(o) => {
      val obj = linProblem.objective("__obj__", Problem.Direction.MAXIMIZE)
      //obj.bounds(o.min, o.max)
      pushForRow(obj, getLinearExpression(o))
    }
    case Minimisation(o) => {
      val obj = linProblem.objective("__obj__", Problem.Direction.MINIMIZE)
      //obj.bounds(o.min.toDouble, o.max.toDouble)
      pushForRow(obj, getLinearExpression(o))
    }
    case Maximisation(o) => {
      val obj = linProblem.objective("__obj__", Problem.Direction.MAXIMIZE)
      //obj.bounds(o.min.toDouble, o.max.toDouble)
      pushForRow(obj, getLinearExpression(o))
    }
    case NoOptimisation() => {
      //there is a small bug in linopt that makes GLPK crash if there is not optimization method
      val obj = linProblem.objective("__noopti__", Problem.Direction.MAXIMIZE)
    }
    case _ => throw new RuntimeException("You can only minimize/maximize float expression in linear programming")
  }
}