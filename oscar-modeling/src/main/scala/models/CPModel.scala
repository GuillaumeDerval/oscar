package models

import algebra._
import constraints._
import oscar.algo.reversible.ReversibleInt
import oscar.cp
import cp.constraints.tables.TableAlgo
import cp.constraints.{CPObjective, CPObjectiveUnit, CPObjectiveUnitMaximize, CPObjectiveUnitMinimize}
import cp.core.{CPOutcome, CPPropagStrength}
import cp.{CPBoolVarOps, CPIntVarOps}
import vars.cp.int.{CPBoolVar => ModelCPBoolVar, CPIntVar => ModelCPIntVar}
import vars.domainstorage.int.{AdaptableIntDomainStorage, IntervalDomainStorage, SetDomainStorage, SingletonDomainStorage}
import vars.{BoolVar, IntVar}

import scala.collection.mutable

/**
  * Model associated with a CP Solver
  * @param p
  */
class CPModel(p: UninstantiatedModel) extends InstantiatedModel(p){
  implicit lazy val cpSolver = new cp.CPSolver()
  override type IntVarImplementation = ModelCPIntVar

  val cpObjective: CPObjectiveUnit= this.optimisationMethod match {
    case m: Minimisation =>
      new CPObjectiveUnitMinimize(this.getRepresentative(m.objective).realCPVar)
    case m: Maximisation =>
      new CPObjectiveUnitMaximize(this.getRepresentative(m.objective).realCPVar)
    case _ => null
  }

  if(cpObjective != null)
    cpSolver.optimize(new CPObjective(cpSolver, cpObjective))

  def getReversibleInt(init: Int) = new ReversibleInt(cpSolver, init)

  override protected def instantiateAdaptableIntDomainStorage(adaptable: AdaptableIntDomainStorage): ModelCPIntVar = {
    if(adaptable.min >= 0 && adaptable.max <= 1)
      ModelCPBoolVar(adaptable, cpSolver)
    else
      ModelCPIntVar(adaptable.content, cpSolver)
  }

  override protected def instantiateSetDomainStorage(set: SetDomainStorage): ModelCPIntVar = {
    if(set.min >= 0 && set.max <= 1)
      ModelCPBoolVar(set, cpSolver)
    else
      ModelCPIntVar(set, cpSolver)
  }

  override protected def instantiateSingletonDomainStorage(singleton: SingletonDomainStorage): ModelCPIntVar = {
    if(singleton.min >= 0 && singleton.max <= 1)
      ModelCPBoolVar(singleton, cpSolver)
    else
      ModelCPIntVar(singleton, cpSolver)
  }

  override protected def instantiateIntervalDomainStorage(interval: IntervalDomainStorage): ModelCPIntVar = {
    if(interval.min >= 0 && interval.max <= 1)
      ModelCPBoolVar(interval, cpSolver)
    else
      ModelCPIntVar(interval, cpSolver)
  }

  override def post(constraint: Constraint): Boolean = {
    constraint match {
      case ExpressionConstraint(expr: BoolExpression) => postBooleanExpression(expr)
      case Among(n, x, s) => cpSolver.post(cp.modeling.constraint.among(postIntExpressionAndGetVar(n), x.map(postIntExpressionAndGetVar), s)) != CPOutcome.Failure
      case MinCumulativeResource(starts, durations, ends, demands, resources, capacity, id) =>
        val cpStart = starts map postIntExpressionAndGetVar
        val cpDuration = durations map postIntExpressionAndGetVar
        val cpEnds = ends map postIntExpressionAndGetVar
        val cpDemands = demands map postIntExpressionAndGetVar
        val cpResources = resources map postIntExpressionAndGetVar
        val cpCapacity = postIntExpressionAndGetVar(capacity)
        cpSolver.post(cp.modeling.constraint.minCumulativeResource(cpStart, cpDuration, cpEnds, cpDemands, cpResources, cpCapacity, id)) != CPOutcome.Failure
      case MaxCumulativeResource(starts, durations, ends, demands, resources, capacity, id) =>
        val cpStart = starts map postIntExpressionAndGetVar
        val cpDuration = durations map postIntExpressionAndGetVar
        val cpEnds = ends map postIntExpressionAndGetVar
        val cpDemands = demands map postIntExpressionAndGetVar
        val cpResources = resources map postIntExpressionAndGetVar
        val cpCapacity = postIntExpressionAndGetVar(capacity)
        cpSolver.post(cp.modeling.constraint.maxCumulativeResource(cpStart, cpDuration, cpEnds, cpDemands, cpResources, cpCapacity, id)) != CPOutcome.Failure
      case AllDifferent(array) => cpSolver.post(cp.modeling.constraint.allDifferent(array.map(postIntExpressionAndGetVar)), CPPropagStrength.Weak) != CPOutcome.Failure
      case LexLeq(a, b) => cpSolver.post(cp.modeling.constraint.lexLeq(a.map(postIntExpressionAndGetVar), b.map(postIntExpressionAndGetVar))) != CPOutcome.Failure
      case Table(array, values) => cpSolver.post(cp.modeling.constraint.table(array.map(postIntExpressionAndGetVar), values, TableAlgo.MDD4R)) != CPOutcome.Failure
      case NegativeTable(array, values) => cpSolver.post(cp.modeling.constraint.negativeTable(array.map(postIntExpressionAndGetVar), values)) != CPOutcome.Failure
      case MinCircuit(succ, distMatrixSucc, cost) => cpSolver.post(cp.modeling.constraint.minCircuit(succ.map(postIntExpressionAndGetVar), distMatrixSucc, postIntExpressionAndGetVar(cost)), CPPropagStrength.Strong) != CPOutcome.Failure
      case MinCircuitWeak(succ, distMatrixSucc, cost) => cpSolver.post(cp.modeling.constraint.minCircuit(succ.map(postIntExpressionAndGetVar), distMatrixSucc, postIntExpressionAndGetVar(cost)), CPPropagStrength.Weak) != CPOutcome.Failure
      case GCC(x, minVal, low, up) => cpSolver.post(new cp.constraints.GCC(x.map(postIntExpressionAndGetVar), minVal, low, up)) != CPOutcome.Failure
      case BinPacking(x, w, l) => cpSolver.post(new cp.constraints.BinPacking(x.map(postIntExpressionAndGetVar), w, l.map(postIntExpressionAndGetVar))) != CPOutcome.Failure
      case Circuit(succ, symmetric) => cpSolver.post(new cp.constraints.Circuit(succ.map(postIntExpressionAndGetVar), symmetric), CPPropagStrength.Strong) != CPOutcome.Failure
      case SubCircuit(succ, offset) => cpSolver.post(cp.constraints.SubCircuit(succ.map(postIntExpressionAndGetVar), offset)) != CPOutcome.Failure
      case Inverse(a, b) => cpSolver.post(new cp.constraints.Inverse(a.map(postIntExpressionAndGetVar), b.map(postIntExpressionAndGetVar))) != CPOutcome.Failure
      case MinAssignment(xarg, weightsarg, cost) => cpSolver.post(new cp.constraints.MinAssignment(xarg.map(postIntExpressionAndGetVar), weightsarg, postIntExpressionAndGetVar(cost))) != CPOutcome.Failure
      case StrongEq(a, b) => cpSolver.post(postIntExpressionAndGetVar(a) === postIntExpressionAndGetVar(b), CPPropagStrength.Strong) != CPOutcome.Failure
      case Spread(a, s1, s2) => cpSolver.post(new cp.constraints.Spread(a.toArray.map(postIntExpressionAndGetVar), s1, postIntExpressionAndGetVar(s2), true))  != CPOutcome.Failure
      case default => throw new Exception() //TODO: put a real exception here
    }
  }

  def postEquality(left: IntExpression, right: IntExpression, second: Boolean = false): Boolean = (left, right) match {
    case (Minus(a, b), v: IntExpression) =>
      cpSolver.add(new cp.constraints.BinarySum(postIntExpressionAndGetVar(v),postIntExpressionAndGetVar(b),postIntExpressionAndGetVar(a))) != CPOutcome.Failure
    case (Sum(Array(a, b)), v: IntExpression) =>
      cpSolver.add(new cp.constraints.BinarySum(postIntExpressionAndGetVar(a),postIntExpressionAndGetVar(b),postIntExpressionAndGetVar(v))) != CPOutcome.Failure
    case (Prod(Array(a, b)), v: IntExpression) =>
      cpSolver.add(new cp.constraints.MulVar(postIntExpressionAndGetVar(a),postIntExpressionAndGetVar(b),postIntExpressionAndGetVar(v))) != CPOutcome.Failure
    case _ =>
      if(!second) //retry with the reversed order
        postEquality(right, left, second = true)
      else
        postConstraintForPossibleConstant(left, right,
          (x,y)=>(y === x),
          (x,y)=>(x === y),
          (x,y)=>(x === y)
        )
  }

  def postBooleanExpression(expr: BoolExpression): Boolean = {
    expr match {
      case And(array) =>
        array.forall(i => postBooleanExpression(i))
      case Eq(a, b) =>
        postEquality(a, b)
      case Gr(a, b) =>
        cpSolver.add(new cp.constraints.Gr(postIntExpressionAndGetVar(a),postIntExpressionAndGetVar(b))) != CPOutcome.Failure
      case GrEq(a, b) =>
        cpSolver.add(new cp.constraints.GrEq(postIntExpressionAndGetVar(a),postIntExpressionAndGetVar(b))) != CPOutcome.Failure
      case Lr(a, b) =>
        cpSolver.add(new cp.constraints.Le(postIntExpressionAndGetVar(a),postIntExpressionAndGetVar(b))) != CPOutcome.Failure
      case LrEq(a, b) =>
        cpSolver.add(new cp.constraints.LeEq(postIntExpressionAndGetVar(a),postIntExpressionAndGetVar(b))) != CPOutcome.Failure
      case Or(Array(a,b)) => //binary Or
        cpSolver.add(cp.or(Array(postBoolExpressionAndGetVar(a), postBoolExpressionAndGetVar(b)))) != CPOutcome.Failure
      case Or(a) => //n-ary Or
        cpSolver.add(cp.or(a.map(postBoolExpressionAndGetVar))) != CPOutcome.Failure
      case Not(a) =>
        cpSolver.add(postBoolExpressionAndGetVar(a).not) != CPOutcome.Failure
      case NotEq(a, b) =>
        postConstraintForPossibleConstant(a, b,
          (x,y)=>(y !== x),
          (x,y)=>(x !== y),
          (x,y)=>(x !== y)
        )
      case InSet(a, b) =>
        cpSolver.add(new cp.constraints.InSet(postIntExpressionAndGetVar(a), b)) != CPOutcome.Failure
      case Implication(a, b) =>
        val v = cp.CPBoolVar()
        cpSolver.add(new cp.constraints.Implication(v, postBoolExpressionAndGetVar(a), postBoolExpressionAndGetVar(b))) != CPOutcome.Failure &&
        cpSolver.add(v) != CPOutcome.Failure
      case Xor(a, b) => throw new Exception() //TODO: throw valid exception
      case v: BoolVar =>
        cpSolver.add(getRepresentative(v).realCPVar.asInstanceOf[cp.CPBoolVar])  != CPOutcome.Failure
    }
  }

  def postBoolExpressionAndGetVar(expr: BoolExpression): cp.CPBoolVar = {
    expr match {
      case And(Array(a,b)) => //binary And
        val b = cp.modeling.constraint.plus(postBoolExpressionAndGetVar(a),
          postBoolExpressionAndGetVar(a))
        b ?=== 2
      case And(array) => //n-ary And
        val v: Array[cp.CPIntVar] = array.map(x => postBoolExpressionAndGetVar(x).asInstanceOf[cp.CPIntVar])
        val b = cp.CPIntVar(0, array.length)
        cpSolver.post(cp.modeling.constraint.sum(v, b))
        b ?=== array.length
      case Eq(a, b) =>
        CPIntVarOps(postIntExpressionAndGetVar(a)) ?=== postIntExpressionAndGetVar(b)
      case Gr(a, b) =>
        CPIntVarOps(postIntExpressionAndGetVar(a)) ?> postIntExpressionAndGetVar(b)
      case GrEq(a, b) =>
        CPIntVarOps(postIntExpressionAndGetVar(a)) ?>= postIntExpressionAndGetVar(b)
      case Lr(a, b) =>
        CPIntVarOps(postIntExpressionAndGetVar(a)) ?< postIntExpressionAndGetVar(b)
      case LrEq(a, b) =>
        CPIntVarOps(postIntExpressionAndGetVar(a)) ?<= postIntExpressionAndGetVar(b)
      case Or(array) => //n-ary Or
        val b = cp.CPBoolVar()
        cpSolver.post(new cp.constraints.OrReif2(array.map(postBoolExpressionAndGetVar), b))
        b
      case Not(a) =>
        postBoolExpressionAndGetVar(a).not
      case NotEq(a, b) =>
        CPIntVarOps(postIntExpressionAndGetVar(a)) ?!== postIntExpressionAndGetVar(b)
      case Implication(a, b) =>
        CPBoolVarOps(postBoolExpressionAndGetVar(a)) ==> postBoolExpressionAndGetVar(b)
      case Xor(a, b) => throw new Exception() //TODO: throw valid exception
      case InSet(a, b) => throw new Exception() //TODO: throw valid exception
      case v: BoolVar => getRepresentative(v).realCPVar.asInstanceOf[cp.CPBoolVar]
    }
  }

  def postBoolExpressionWithVar(expr: BoolExpression, result: cp.CPBoolVar): Unit = {
    expr match {
      case And(Array(a,b)) => //binary And
        val b = cp.modeling.constraint.plus(postBoolExpressionAndGetVar(a),
          postBoolExpressionAndGetVar(a))
        cpSolver.post(new cp.constraints.EqReif(b, 2, result))
      case And(array) => //n-ary And
        val v: Array[cp.CPIntVar] = array.map(x => postBoolExpressionAndGetVar(x).asInstanceOf[cp.CPIntVar])
        val b = cp.CPIntVar(0, array.length)
        cpSolver.post(cp.modeling.constraint.sum(v, b))
        cpSolver.post(new cp.constraints.EqReif(b, array.length, result))
      case Eq(a, b) =>
        // TODO what if a/b is a constant? get rid of ReifVar?
        cpSolver.post(new cp.constraints.EqReifVar(postIntExpressionAndGetVar(a), postIntExpressionAndGetVar(b), result))
      case Gr(a, b) =>
        // TODO what if a/b is a constant? get rid of ReifVar?
        cpSolver.post(new cp.constraints.GrEqVarReif(postIntExpressionAndGetVar(a), postIntExpressionAndGetVar(b)+1, result))
      case GrEq(a, b) =>
        // TODO what if a/b is a constant? get rid of ReifVar?
        cpSolver.post(new cp.constraints.GrEqVarReif(postIntExpressionAndGetVar(a), postIntExpressionAndGetVar(b), result))
      case Lr(a, b) =>
        // TODO what if a/b is a constant? get rid of ReifVar?
        cpSolver.post(new cp.constraints.GrEqVarReif(postIntExpressionAndGetVar(b), postIntExpressionAndGetVar(a)+1, result))
      case LrEq(a, b) =>
        // TODO what if a/b is a constant? get rid of ReifVar?
        cpSolver.post(new cp.constraints.GrEqVarReif(postIntExpressionAndGetVar(b), postIntExpressionAndGetVar(a), result))
      case Or(array) =>
        cpSolver.add(new cp.constraints.OrReif2(array.map(postBoolExpressionAndGetVar), result))
      case Not(a) =>
        cpSolver.post(new cp.constraints.Eq(postBoolExpressionAndGetVar(a).not, result))
      case NotEq(a, b) =>
        // TODO what if a/b is a constant? get rid of ReifVar?
        cpSolver.post(new cp.constraints.DiffReifVar(postIntExpressionAndGetVar(a), postIntExpressionAndGetVar(b), result))
      case Implication(a, b) =>
        cpSolver.post(new cp.constraints.Implication(postBoolExpressionAndGetVar(a), postBoolExpressionAndGetVar(b), result))
      case Xor(a, b) => throw new Exception() //TODO: throw valid exception
      case InSet(a, b) => throw new Exception() //TODO: throw valid exception
      case v: BoolVar => throw new Exception() //TODO: throw valid exception
    }
  }

  lazy val expr_cache: mutable.HashMap[IntExpression, cp.CPIntVar] = mutable.HashMap[IntExpression, cp.CPIntVar]()
  def postIntExpressionAndGetVar(expr: IntExpression): cp.CPIntVar =
    expr_cache.getOrElseUpdate(expr,{
      expr match {
        case expr: BoolExpression => postBoolExpressionAndGetVar(expr)
        case Abs(a) => postIntExpressionAndGetVar(a).abs
        case Constant(a) => cp.CPIntVar(a)
        case Count(x, y) =>
          val v = cp.CPIntVar(0, x.length)
          cpSolver.add(new cp.constraints.Count(v, x.map(postIntExpressionAndGetVar), postIntExpressionAndGetVar(y)))
          v
        case Element(x, y) =>
          val vx: Array[cp.CPIntVar] = x.map(postIntExpressionAndGetVar)
          val vy: cp.CPIntVar = postIntExpressionAndGetVar(y)
          vx(vy)
        case ElementCst(x, y) =>
          val vy: cp.CPIntVar = postIntExpressionAndGetVar(y)
          x(vy)
        case ElementCst2D(x, y, z) =>
          val vy: cp.CPIntVar = postIntExpressionAndGetVar(y)
          val vz: cp.CPIntVar = postIntExpressionAndGetVar(z)
          x(vy)(vz)
        case Max(x) =>
          val vx = x.map(postIntExpressionAndGetVar)
          val m = cp.CPIntVar(vx.map(_.min).max, vx.map(_.max).max)
          cpSolver.add(cp.maximum(vx, m))
          m
        case Min(x) =>
          val vx = x.map(postIntExpressionAndGetVar)
          val m = cp.CPIntVar(vx.map(_.min).min, vx.map(_.max).min)
          cpSolver.add(cp.maximum(vx, m))
          m
        case Minus(x, y) =>
          getCPIntVarForPossibleConstant(x, y,
            (a,b) => -b + a,
            (a,b) => a - b,
            (a,b) => a - b)
        case Modulo(x, y) =>
          val v = cp.CPIntVar(expr.min, expr.max)
          cpSolver.add(new CPIntVarOps(postIntExpressionAndGetVar(x)) % y == v)
          v
        case Prod(Array(x, y)) => //binary prod
          getCPIntVarForPossibleConstant(x, y,
            (a,b) => b * a,
            (a,b) => a * b,
            (a,b) => a * b)
        case Prod(x) => //n-ary prod
          //OscaR only has binary product; transform into a balanced binary tree to minimise number of constraints
          def recurmul(exprs: Array[cp.CPIntVar]): Array[cp.CPIntVar] = {
            if(exprs.length == 1)
              exprs
            else
              recurmul(exprs.grouped(2).map({
                case Array(a, b) => CPIntVarOps(a) * b
                case Array(a) => a
              }).toArray)
          }
          recurmul(x.map(postIntExpressionAndGetVar))(0)
        case Sum(Array(x, y)) => //binary sum
          getCPIntVarForPossibleConstant(x, y,
            (a,b) => b + a,
            (a,b) => a + b,
            (a,b) => a + b)
        case Sum(x) => //n-ary sum
          cp.sum(x.map(postIntExpressionAndGetVar))
        case UnaryMinus(a) =>
          -postIntExpressionAndGetVar(a)
        case WeightedSum(x, y) =>
          cp.weightedSum(y, x.map(postIntExpressionAndGetVar))
        case Div(x, y) => throw new Exception() //TODO: real exception
        case Exponent(x, y) => throw new Exception() //TODO: real exception
        case v: IntVar =>
          getRepresentative(v).realCPVar
      }
    })

  /**
    * Post the right constraint depending on the type of a and b.
    *
    * @param a
    * @param b
    * @param leftCst this function will be called if a is constant
    * @param rightCst this function will be called if b is constant
    * @param allVar this function will be called if a and b are not constant
    */
  def postConstraintForPossibleConstant(a: IntExpression, b: IntExpression,
                                        leftCst: (Int, cp.CPIntVar) => cp.Constraint,
                                        rightCst: (cp.CPIntVar, Int) => cp.Constraint,
                                        allVar: (cp.CPIntVar, cp.CPIntVar) => cp.Constraint): Boolean = {
    (a,b) match {
      case (Constant(value), variable:IntExpression) => cpSolver.add(leftCst(value, postIntExpressionAndGetVar(variable))) != CPOutcome.Failure
      case (variable: IntExpression, Constant(value)) => cpSolver.add(rightCst(postIntExpressionAndGetVar(variable), value)) != CPOutcome.Failure
      case (v1: IntExpression, v2: IntExpression) => cpSolver.add(allVar(postIntExpressionAndGetVar(v1), postIntExpressionAndGetVar(v2))) != CPOutcome.Failure
    }
  }

  /**
    * Post the right constraint depending on the type of a and b.
    *
    * @param a
    * @param b
    * @param leftCst this function will be called if a is constant
    * @param rightCst this function will be called if b is constant
    * @param allVar this function will be called if a and b are not constant
    */
  def getCPIntVarForPossibleConstant(a: IntExpression, b: IntExpression,
                                     leftCst: (Int, cp.CPIntVar) => cp.CPIntVar,
                                     rightCst: (cp.CPIntVar, Int) => cp.CPIntVar,
                                     allVar: (cp.CPIntVar, cp.CPIntVar) => cp.CPIntVar): cp.CPIntVar = {
    (a,b) match {
      case (Constant(value), variable:IntExpression) => leftCst(value, postIntExpressionAndGetVar(variable))
      case (variable: IntExpression, Constant(value)) => rightCst(postIntExpressionAndGetVar(variable), value)
      case (v1: IntExpression, v2: IntExpression) => allVar(postIntExpressionAndGetVar(v1), postIntExpressionAndGetVar(v2))
    }
  }
}
