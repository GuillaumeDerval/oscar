package oscar.modeling.models

import oscar.modeling.algebra._
import oscar.modeling.constraints._
import oscar.algo.reversible.ReversibleInt
import oscar.cp
import cp.constraints.{CPObjective, CPObjectiveUnit, CPObjectiveUnitMaximize, CPObjectiveUnitMinimize}
import cp.core.{CPPropagStrength, NoSolutionException}
import cp.{CPBoolVarOps, CPIntVarOps}
import oscar.algo.DisjointSets
import oscar.algo.search.Outcome
import oscar.modeling.models.CPModel.{InstantiateAndReuse, InstantiateAndStoreInCache}
import oscar.modeling.vars.cp.CPIntVar
import oscar.modeling.vars.cp.{CPBoolVar => ModelCPBoolVar, CPIntVar => ModelCPIntVar}
import oscar.modeling.vars.domainstorage.IntDomainStorage
import oscar.modeling.vars.{BoolVar, IntVar}

import scala.collection.immutable
import scala.collection.mutable

private case class CPCstEq(expr: IntExpression, cst: Int) extends Constraint

object CPModel {
  private case class InstantiateAndStoreInCache(expr: IntExpression) extends Constraint
  private case class InstantiateAndReuse(reuseFrom: IntExpression, toInstantiate: IntExpression) extends Constraint

  /**
    * Preprocess some things in order to improve performance of the solver
    * Currently preprocessed:
    * - Eq constraints
    *
    * TODO merge IntVars together
    */
  private def preprocessCP(p: UninstantiatedModel): UninstantiatedModel = {
    var representatives = p.intRepresentatives

    // Find all the Eq
    val eqs = p.constraints.filter{case ExpressionConstraint(eq: Eq) => true; case _ => false}.map{case ExpressionConstraint(eq: Eq) => eq}.toArray
    if(eqs.isEmpty) //nothing to do here
      return p

    val otherConstraints = p.constraints.filter{case ExpressionConstraint(eq: Eq) => false; case _ => true}

    // Find, for each expression, in which eq they are. The goal is to merge eq as much as possible
    val exprToSet = mutable.HashMap[IntExpression, mutable.Set[Int]]()
    for((eq, idx) <- eqs.zipWithIndex)
      for(expr <- eq.v)
        exprToSet.getOrElseUpdate(expr, mutable.Set()).add(idx)

    // Merge all the Eq containing same expressions
    val unionFind = new DisjointSets[mutable.Set[IntExpression]](0, eqs.length-1)
    for((expr, set) <- exprToSet) {
      set.sliding(2).map(_.toArray).foreach{
        case Array(a,b) => unionFind.union(a,b)
        case Array(a) => //ignore
      }
    }

    // Merge all the Eq containing "same" IntVars (pointing to the same effective values)
    val intvarsToSet = exprToSet.filter(_._1.isInstanceOf[IntVar]).toArray.asInstanceOf[Array[(IntVar, mutable.Set[Int])]]
    for(i <- 0 until intvarsToSet.size) {
      val repr1 = p.getRepresentative(intvarsToSet(i)._1)
      for(j <- i+1 until intvarsToSet.size) {
        val repr2 = p.getRepresentative(intvarsToSet(j)._1)
        if(repr1 == repr2)
          unionFind.union(intvarsToSet(i)._2.head, intvarsToSet(i)._1.head)
      }
    }

    // Each set in allSets is a new Eq
    val allSets = eqs.indices.map(unionFind.find).toSet
    allSets.foreach(_.data = Some(mutable.Set()))
    for((expr, set) <- exprToSet) {
      set.foreach(unionFind.find(_).data.get.add(expr))
    }

    // Merge IntVars in the same Eq
    for(setObj <- allSets) {
      val set = setObj.data.get
      val intvars = set.filter(_.isInstanceOf[IntVar]).map(_.asInstanceOf[IntVar]).toArray
      val notIntvars = set.filterNot(_.isInstanceOf[IntVar])
      if(intvars.length > 1) {
        // Compute new domain
        val newDomain = immutable.SortedSet[Int]() ++ intvars.map(p.getRepresentative(_).toVector.toSet).reduceLeft((a,b)=> b.intersect(a))
        val newDomainStorage = new IntDomainStorage(newDomain, intvars.head.name)
        representatives = (1 until intvars.length).foldLeft(p.intRepresentatives){case (iR, idx) => iR.union(intvars(0), intvars(idx), newDomainStorage)}
      }
    }

    // Map each expression to a constant representing the equality which it is in
    val exprToEq : Map[IntExpression, Int] = allSets.zipWithIndex.flatMap{case (eq, idx) => eq.data.get.map(_ -> idx)}.toMap

    // Generate instantiation order (constants first, then toposort of the expressions)
    val topoSortOrder = expressionTopoSort(allSets.flatMap(_.data.get))

    // Select the variable to instantiate for each Eq (the first appearing in the toposort)
    // Create InstantiateAndStoreInCache for each of these
    val newEqConstraints = mutable.ArrayBuffer[Constraint]()
    val eqFirstExpr = Array.fill[IntExpression](allSets.size)(null)
    val exprUsed = Array.fill(exprToEq.size)(false)
    for((expr,idx) <- topoSortOrder.zipWithIndex) {
      val eq = exprToEq(expr)
      if(null == eqFirstExpr(eq)) {
        eqFirstExpr(eq) = expr
        exprUsed(idx) = true
        newEqConstraints += InstantiateAndStoreInCache(expr)
      }
    }

    // Create InstantiateAndReuse for all the other values (linked to the Eq, in the order of the toposort)
    for((expr,idx) <- topoSortOrder.zipWithIndex; if !exprUsed(idx)) {
      newEqConstraints += InstantiateAndReuse(eqFirstExpr(exprToEq(expr)), expr)
    }

    val newConstraints: List[Constraint]= newEqConstraints.toList ++ otherConstraints
    UninstantiatedModel(p.declaration, newConstraints, representatives, p.optimisationMethod)
  }

  private def expressionTopoSort(expressions: Set[IntExpression]): Array[IntExpression] = {
    /**
      * Fix some problems with on-the-fly-generated subexprs.
      * For example, as Div generates a Minus(y-Modulo(x,y)), we should ensures we checks its dependencies correctly
      */
    def customSubExpr(expr: IntExpression): Iterable[IntExpression] = expr match {
      case Div(x, y) => Array(x - Modulo(x, y))
      case Xor(a,b) => Array(And(Or(a,b), Not(And(a,b))))
      case default => default.subexpressions()
    }

    // Create the dependency graph
    val links = expressions.map(_ -> mutable.Set[IntExpression]()).toMap

    for(expr <- expressions) {
      def recurFind(subExprs: Iterable[IntExpression]): Unit = {
        for(subExpr <- subExprs) {
          if(expressions.contains(subExpr))
            links(subExpr).add(expr) //subExpr must be instantiated before expr
          else
            recurFind(customSubExpr(subExpr))
        }
      }
      recurFind(customSubExpr(expr))
    }

    // We now have the graph, let's toposort it
    val visited = mutable.HashMap[IntExpression, Boolean]()
    visited ++= expressions.map(_ -> false)

    val topo = mutable.Stack[IntExpression]()

    def DFSVisit(expr: IntExpression): Unit = {
      visited.put(expr, true)
      links(expr).foreach(node => if(!visited(node)) DFSVisit(node))
      topo.push(expr)
    }
    for(expr <- expressions; if !visited(expr)) DFSVisit(expr)

    // Get the effective order.
    topo.toArray.sortBy{ //sortBy is stable
      case _: Constant => 0 //Put constants before everything else, as by def they do not have subexprs
      case _: BoolVar => 1 //Then put BoolVars as they should not have a big domain
      case _: IntVar => 2 //Then IntVar, as they are already instantiated
      case default => 3 //Then evrything that is not one of the above
    }
  }
}

/**
  * Model associated with a CP Solver
  * @param p
  */
class CPModel(p: UninstantiatedModel) extends InstantiatedModel(CPModel.preprocessCP(p)){
  implicit lazy val cpSolver = new cp.CPSolver()
  override type IntVarImplementation = CPIntVar

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

  def instantiateIntVar(content: Iterable[Int], name: String): CPIntVar = {
    if(content.min >= 0 && content.max <= 1)
      ModelCPBoolVar(content, name, cpSolver)
    else
      ModelCPIntVar(content, name, cpSolver)
  }

  override def post(constraint: Constraint): Outcome = {
    try {
      constraint match {
        case instantiable: CPInstantiableConstraint => instantiable.cpPost(cpSolver)
        case InstantiateAndStoreInCache(expr) =>
          postIntExpressionAndGetVar(expr)
          Outcome.Suspend
        case InstantiateAndReuse(reuse, expr) =>
          postIntExpressionWithVar(expr, postIntExpressionAndGetVar(reuse))
          Outcome.Suspend
        case ExpressionConstraint(expr: BoolExpression) => postBooleanExpression(expr)
        case Among(n, x, s) => cpSolver.add(cp.modeling.constraint.among(postIntExpressionAndGetVar(n), x.map(postIntExpressionAndGetVar), s))
        case MinCumulativeResource(starts, durations, ends, demands, resources, capacity, id) =>
          val cpStart = starts map postIntExpressionAndGetVar
          val cpDuration = durations map postIntExpressionAndGetVar
          val cpEnds = ends map postIntExpressionAndGetVar
          val cpDemands = demands map postIntExpressionAndGetVar
          val cpResources = resources map postIntExpressionAndGetVar
          val cpCapacity = postIntExpressionAndGetVar(capacity)
          cpSolver.add(cp.modeling.constraint.minCumulativeResource(cpStart, cpDuration, cpEnds, cpDemands, cpResources, cpCapacity, id))
        case MaxCumulativeResource(starts, durations, ends, demands, resources, capacity, id) =>
          val cpStart = starts map postIntExpressionAndGetVar
          val cpDuration = durations map postIntExpressionAndGetVar
          val cpEnds = ends map postIntExpressionAndGetVar
          val cpDemands = demands map postIntExpressionAndGetVar
          val cpResources = resources map postIntExpressionAndGetVar
          val cpCapacity = postIntExpressionAndGetVar(capacity)
          cpSolver.add(cp.modeling.constraint.maxCumulativeResource(cpStart, cpDuration, cpEnds, cpDemands, cpResources, cpCapacity, id))
        case AllDifferent(array) => cpSolver.add(cp.modeling.constraint.allDifferent(array.map(postIntExpressionAndGetVar)), CPPropagStrength.Weak)
        case LexLeq(a, b) => cpSolver.add(cp.modeling.constraint.lexLeq(a.map(postIntExpressionAndGetVar), b.map(postIntExpressionAndGetVar)))
        case Table(array, values, None) =>
          cpSolver.add(cp.modeling.constraint.table(array.map(postIntExpressionAndGetVar), values))
        case NegativeTable(array, values, None) => cpSolver.add(cp.modeling.constraint.negativeTable(array.map(postIntExpressionAndGetVar), values))
        case Table(array, values, Some(starred)) => cpSolver.add(new oscar.cp.constraints.tables.TableCTStar(array.map(postIntExpressionAndGetVar), values, starred))
        case NegativeTable(array, values, Some(starred)) => cpSolver.add(new oscar.cp.constraints.tables.TableCTNegStar(array.map(postIntExpressionAndGetVar), values, starred))
        case MinCircuit(succ, distMatrixSucc, cost) => cpSolver.add(cp.modeling.constraint.minCircuit(succ.map(postIntExpressionAndGetVar), distMatrixSucc, postIntExpressionAndGetVar(cost)), CPPropagStrength.Strong)
        case MinCircuitWeak(succ, distMatrixSucc, cost) => cpSolver.add(cp.modeling.constraint.minCircuit(succ.map(postIntExpressionAndGetVar), distMatrixSucc, postIntExpressionAndGetVar(cost)), CPPropagStrength.Weak)
        case GCC(x, minVal, low, up) => cpSolver.add(new cp.constraints.GCC(x.map(postIntExpressionAndGetVar), minVal, low, up))
        case GCCVar(x, y) => cpSolver.add(cp.modeling.constraint.gcc(x.map(postIntExpressionAndGetVar), y.map(a => (a._1, postIntExpressionAndGetVar(a._2)))))
        case BinPacking(x, w, l) => cpSolver.add(new cp.constraints.BinPacking(x.map(postIntExpressionAndGetVar), w, l.map(postIntExpressionAndGetVar)))
        case Circuit(succ, symmetric) => cpSolver.add(new cp.constraints.Circuit(succ.map(postIntExpressionAndGetVar), symmetric), CPPropagStrength.Strong)
        case SubCircuit(succ, offset) => cpSolver.add(cp.constraints.SubCircuit(succ.map(postIntExpressionAndGetVar), offset))
        case Inverse(a, b) => cpSolver.add(new cp.constraints.Inverse(a.map(postIntExpressionAndGetVar), b.map(postIntExpressionAndGetVar)))
        case MinAssignment(xarg, weightsarg, cost) => cpSolver.add(new cp.constraints.MinAssignment(xarg.map(postIntExpressionAndGetVar), weightsarg, postIntExpressionAndGetVar(cost)))
        case StrongEq(a, b) => cpSolver.add(postIntExpressionAndGetVar(a) === postIntExpressionAndGetVar(b), CPPropagStrength.Strong)
        case Spread(a, s1, s2) => cpSolver.add(new cp.constraints.Spread(a.toArray.map(postIntExpressionAndGetVar), s1, postIntExpressionAndGetVar(s2), true))
        case UnaryResourceSimple(starts, durations, ends, resources, id) =>
          val cpStarts = starts.map(postIntExpressionAndGetVar)
          val cpDurations = durations.map(postIntExpressionAndGetVar)
          val cpEnds = ends.map(postIntExpressionAndGetVar)
          val cpResources = resources.map(postIntExpressionAndGetVar)
          cpSolver.add(cp.modeling.constraint.unaryResource(cpStarts, cpDurations, cpEnds, cpResources, id))
        case UnaryResourceTransitionType(starts, durations, ends, types, transitionTimes) =>
          val cpStarts = starts.map(postIntExpressionAndGetVar)
          val cpDurations = durations.map(postIntExpressionAndGetVar)
          val cpEnds = ends.map(postIntExpressionAndGetVar)
          cpSolver.add(cp.modeling.constraint.unaryResource(cpStarts, cpDurations, cpEnds, types, transitionTimes))
        case UnaryResourceTransition(starts, durations, ends, transitionTimes) =>
          val cpStarts = starts.map(postIntExpressionAndGetVar)
          val cpDurations = durations.map(postIntExpressionAndGetVar)
          val cpEnds = ends.map(postIntExpressionAndGetVar)
          cpSolver.add(cp.modeling.constraint.unaryResource(cpStarts, cpDurations, cpEnds, transitionTimes))
        case UnaryResourceTransitionFamilies(starts, durations, ends, familyMatrix, families) =>
          val cpStarts = starts.map(postIntExpressionAndGetVar)
          val cpDurations = durations.map(postIntExpressionAndGetVar)
          val cpEnds = ends.map(postIntExpressionAndGetVar)
          cpSolver.add(cp.modeling.constraint.unaryResource(cpStarts, cpDurations, cpEnds, familyMatrix, families))
        case DiffN(x, dx, y, dy) =>
          cpSolver.add(cp.modeling.constraint.diffn(x.map(postIntExpressionAndGetVar), dx.map(postIntExpressionAndGetVar), y.map(postIntExpressionAndGetVar), dy.map(postIntExpressionAndGetVar)).toArray)
        case Regular(on, automaton) =>
          cpSolver.add(cp.modeling.constraint.regular(on.map(postIntExpressionAndGetVar), automaton))
        case NotAllEqual(x) =>
          cpSolver.add(cp.modeling.constraint.notAllEqual(x.map(postIntExpressionAndGetVar)))
        case default => throw new Exception("Unknown constraint " + constraint.getClass.toString) //TODO: put a real exception here
      }
    }
    catch {
      case c: NoSolutionException => Outcome.Failure
    }
  }

  def postEquality(left: IntExpression, right: IntExpression, second: Boolean = false): Outcome = (left, right) match {
    //TODO replace partially with preprocessing
    case (Minus(a, b), v: IntExpression) =>
      cpSolver.add(new cp.constraints.BinarySum(postIntExpressionAndGetVar(v),postIntExpressionAndGetVar(b),postIntExpressionAndGetVar(a)))
    case (Sum(Array(a, b)), v: IntExpression) =>
      cpSolver.add(new cp.constraints.BinarySum(postIntExpressionAndGetVar(a),postIntExpressionAndGetVar(b),postIntExpressionAndGetVar(v)))
    case (Prod(Array(a, b)), v: IntExpression) =>
      cpSolver.add(new cp.constraints.MulVar(postIntExpressionAndGetVar(a),postIntExpressionAndGetVar(b),postIntExpressionAndGetVar(v)))
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

  def postBooleanExpression(expr: BoolExpression): Outcome = {
    expr match {
      case instantiable: CPInstantiableBoolExpression => instantiable.cpPostAsConstraint(cpSolver)
      case And(array) =>
        val r = array.map(i => postBooleanExpression(i))
        if(r.contains(Outcome.Failure))
          Outcome.Failure
        else if(r.contains(Outcome.Suspend))
          Outcome.Suspend
        else
          Outcome.Success
      case Eq(Array(a, b)) => //binary Eq
        postEquality(a, b)
      case Eq(x) => //n-ary Eq
        //TODO lots of ways to improve, must preprocess
        val r = x.sliding(2).map(a => postEquality(a(0), a(1)))
        if(r.contains(Outcome.Failure))
          Outcome.Failure
        else if(r.contains(Outcome.Suspend))
          Outcome.Suspend
        else
          Outcome.Success
      case Gr(a, b) =>
        cpSolver.add(new cp.constraints.Gr(postIntExpressionAndGetVar(a),postIntExpressionAndGetVar(b)))
      case GrEq(a, b) =>
        cpSolver.add(new cp.constraints.GrEq(postIntExpressionAndGetVar(a),postIntExpressionAndGetVar(b)))
      case Lr(a, b) =>
        cpSolver.add(new cp.constraints.Le(postIntExpressionAndGetVar(a),postIntExpressionAndGetVar(b)))
      case LrEq(a, b) =>
        cpSolver.add(new cp.constraints.LeEq(postIntExpressionAndGetVar(a),postIntExpressionAndGetVar(b)))
      case Or(Array(a,b)) => //binary Or
        cpSolver.add(cp.or(Array(postBoolExpressionAndGetVar(a), postBoolExpressionAndGetVar(b))))
      case Or(a) => //n-ary Or
        cpSolver.add(cp.or(a.map(postBoolExpressionAndGetVar)))
      case Not(a) =>
        cpSolver.add(postBoolExpressionAndGetVar(a).not)
      case NotEq(a, b) =>
        postConstraintForPossibleConstant(a, b,
          (x,y)=>(y !== x),
          (x,y)=>(x !== y),
          (x,y)=>(x !== y)
        )
      case InSet(a, b) =>
        cpSolver.add(new cp.constraints.InSet(postIntExpressionAndGetVar(a), b))
      case Implication(a, b) =>
        val v = cp.CPBoolVar(b = true)
        cpSolver.add(new cp.constraints.Implication(postBoolExpressionAndGetVar(a), postBoolExpressionAndGetVar(b), v))
      case Xor(a, b) => postBooleanExpression(And(Or(a,b), Not(And(a,b))))
      case v: BoolVar =>
        cpSolver.add(getRepresentative(v).asInstanceOf[cp.CPBoolVar])
      case default => throw new Exception("Unknown BoolExpression "+default.getClass.toString) //TODO: put a real exception here
    }
  }

  // Cache that stores equivalent cp.CPIntVar for each IntExpression, in order to avoid duplicates
  protected lazy val expr_cache: mutable.HashMap[IntExpression, cp.CPIntVar] = mutable.HashMap[IntExpression, cp.CPIntVar]()

  def postBoolExpressionAndGetVar(expr: BoolExpression): cp.CPBoolVar = {
    expr_cache.getOrElseUpdate(expr, expr match {
      case instantiable: CPInstantiableBoolExpression => instantiable.cpPostAndGetVar(cpSolver)
      case And(x) => //binary And
        val b = cp.CPBoolVar()
        cpSolver.add(new oscar.cp.constraints.And(x.map(postBoolExpressionAndGetVar), b))
        b
      case Eq(Array(a, b)) => //binary eq reif
        CPIntVarOps(postIntExpressionAndGetVar(a)) ?=== postIntExpressionAndGetVar(b)
      case Eq(x) => //n-ary eq reif
        // TODO Is it better to post n*(n-1)/2 reif constraints or only n-1?
        // map to binary Eq
        postBoolExpressionAndGetVar(And(for(a <- x; b <- x; if a != b) yield Eq(a,b).asInstanceOf[BoolExpression]))
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
        cpSolver.add(cp.modeling.constraint.or(array.map(postBoolExpressionAndGetVar), b))
        b
      case Not(a) =>
        postBoolExpressionAndGetVar(a).not
      case NotEq(a, b) =>
        CPIntVarOps(postIntExpressionAndGetVar(a)) ?!== postIntExpressionAndGetVar(b)
      case Implication(a, b) =>
        CPBoolVarOps(postBoolExpressionAndGetVar(a)) ==> postBoolExpressionAndGetVar(b)
      case Xor(a, b) => postBoolExpressionAndGetVar(And(Or(a,b), Not(And(a,b))))
      case InSet(a, b) =>
        val c = cp.CPBoolVar()
        cpSolver.add(new cp.constraints.InSetReif(postIntExpressionAndGetVar(a), b, c))
        c
      case v: BoolVar => getRepresentative(v).asInstanceOf[cp.CPBoolVar]
      case default => throw new Exception("Unknown BoolExpression "+default.getClass.toString) //TODO: put a real exception here
    }).asInstanceOf[cp.CPBoolVar]
  }

  def postBoolExpressionWithVar(expr: BoolExpression, result: cp.CPBoolVar): Outcome = {
    if(expr_cache.contains(expr)) {
      // This should not happen too often, but is still feasible as we create new expr on the fly
      println("An expression given to postBoolExpressionWithVar has already been instantiated!")
      cpSolver.add(expr_cache(expr) === result)
    }
    val ret: Outcome = expr match {
      case instantiable: CPInstantiableBoolExpression => instantiable.cpPostWithVar(cpSolver, result)
      case And(x) =>
        cpSolver.add(new oscar.cp.constraints.And(x.map(postBoolExpressionAndGetVar), result))
      case Eq(Array(a, b)) => //binary Eq
        cpSolver.add(new cp.constraints.EqReifVar(postIntExpressionAndGetVar(a), postIntExpressionAndGetVar(b), result))
      case Eq(x) => //n-ary Eq
        // TODO Is it better to post n*(n-1)/2 reif constraints or only n-1?
        // map to binary Eq
        postBoolExpressionWithVar(And(for(a <- x; b <- x; if a != b) yield Eq(a,b).asInstanceOf[BoolExpression]), result)
      case Gr(a, b) =>
        cpSolver.add(new cp.constraints.GrEqVarReif(postIntExpressionAndGetVar(a), postIntExpressionAndGetVar(b)+1, result))
      case GrEq(a, b) =>
        cpSolver.add(new cp.constraints.GrEqVarReif(postIntExpressionAndGetVar(a), postIntExpressionAndGetVar(b), result))
      case Lr(a, b) =>
        cpSolver.add(new cp.constraints.GrEqVarReif(postIntExpressionAndGetVar(b), postIntExpressionAndGetVar(a)+1, result))
      case LrEq(a, b) =>
        cpSolver.add(new cp.constraints.GrEqVarReif(postIntExpressionAndGetVar(b), postIntExpressionAndGetVar(a), result))
      case Or(array) =>
        cpSolver.add(new cp.constraints.OrReif2(array.map(postBoolExpressionAndGetVar), result))
      case Not(a) =>
        cpSolver.add(new cp.constraints.Eq(postBoolExpressionAndGetVar(a).not, result))
      case NotEq(a, b) =>
        cpSolver.add(new cp.constraints.DiffReifVar(postIntExpressionAndGetVar(a), postIntExpressionAndGetVar(b), result))
      case Implication(a, b) =>
        cpSolver.add(new cp.constraints.Implication(postBoolExpressionAndGetVar(a), postBoolExpressionAndGetVar(b), result))
      case Xor(a, b) => postBoolExpressionWithVar(And(Or(a,b), Not(And(a,b))), result)
      case InSet(a, b) => cpSolver.add(new cp.constraints.InSetReif(postIntExpressionAndGetVar(a), b, result))
      case v: BoolVar => cpSolver.add(postBoolExpressionAndGetVar(v) === result)
      case default => throw new Exception("Unknown BoolExpression "+default.getClass.toString) //TODO: put a real exception here
    }
    // Must ABSOLUTELY be put AFTER all the calls to postIntExpressionAndGetVar
    expr_cache.put(expr, result)
    ret
  }

  def postIntExpressionAndGetVar(expr: IntExpression): cp.CPIntVar = {
    expr match {
      case boolexpr: BoolExpression => postBoolExpressionAndGetVar(boolexpr) //should be done outside expr_cache
                                                                             //as it is updated by postBoolExpressionAndGetVar
      case default => expr_cache.getOrElseUpdate(expr, expr match {
          case instantiable: CPInstantiableIntExpression => instantiable.cpPostAndGetVar(cpSolver)
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
            cpSolver.add(cp.minimum(vx, m))
            m
          case Minus(x, y) =>
            getCPIntVarForPossibleConstant(x, y,
              (a, b) => -b + a,
              (a, b) => a - b,
              (a, b) => a - b)
          case Modulo(x, y) =>
            val v = cp.CPIntVar(expr.min, expr.max)
            cpSolver.add(new CPIntVarOps(postIntExpressionAndGetVar(x)) % y == v)
            v
          case Prod(Array(x, y)) => //binary prod
            getCPIntVarForPossibleConstant(x, y,
              (a, b) => b * a,
              (a, b) => a * b,
              (a, b) => a * b)
          case Prod(x) => //n-ary prod
            //OscaR only has binary product; transform into a balanced binary tree to minimise number of constraints
            def recurmul(exprs: Array[cp.CPIntVar]): Array[cp.CPIntVar] = {
              if (exprs.length == 1)
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
              (a, b) => b + a,
              (a, b) => a + b,
              (a, b) => a + b)
          case Sum(x) => //n-ary sum
            cp.sum(x.map(postIntExpressionAndGetVar))
          case UnaryMinus(a) =>
            -postIntExpressionAndGetVar(a)
          case WeightedSum(x, y) =>
            cp.weightedSum(y, x.map(postIntExpressionAndGetVar))
          case Div(x, y) =>
            val v = cp.CPIntVar(expr.min, expr.max)
            cpSolver.add(new oscar.cp.constraints.MulCte(v, y, postIntExpressionAndGetVar(x - Modulo(x, y))))
            v
          case NValues(x) =>
            val v = cp.CPIntVar(expr.min, expr.max)
            cpSolver.add(new oscar.cp.constraints.AtLeastNValue(x.map(postIntExpressionAndGetVar), v))
            v
          case Exponent(x, y) => throw new Exception() //TODO: real exception
          case v: IntVar =>
            getRepresentative(v).realCPVar
          case default => throw new Exception("Unknown IntExpression "+default.getClass.toString) //TODO: put a real exception here
        }
      )
    }
  }

  def postIntExpressionWithVar(expr: IntExpression, result: cp.CPIntVar): Outcome = {
    if(expr_cache.contains(expr)) {
      // This should not happen too often, but is still feasible as we create new expr on the fly
      println("An expression given to postBoolExpressionWithVar has already been instantiated!")
      cpSolver.add(expr_cache(expr) === result)
    }
    val ret: Outcome = expr match {
      case boolexpr: BoolExpression =>
        postBoolExpressionWithVar(boolexpr, result.asInstanceOf[cp.CPBoolVar])
      case instantiable: CPInstantiableIntExpression => instantiable.cpPostWithVar(cpSolver, result)
      case Abs(a) =>
        cpSolver.add(new cp.constraints.Abs(postIntExpressionAndGetVar(expr), result))
      case Constant(a) =>
        cpSolver.add(new cp.constraints.EqCons(result, a))
      case Count(x, y) =>
        cpSolver.add(new cp.constraints.Count(result, x.map(postIntExpressionAndGetVar), postIntExpressionAndGetVar(y)))
      case Element(x, y) =>
        val vx: Array[cp.CPIntVar] = x.map(postIntExpressionAndGetVar)
        val vy: cp.CPIntVar = postIntExpressionAndGetVar(y)
        cpSolver.add(new cp.constraints.ElementVar(vx, vy, result))
      case ElementCst(x, y) =>
        val vy: cp.CPIntVar = postIntExpressionAndGetVar(y)
        cpSolver.add(new cp.constraints.ElementCst(x, vy, result))
      case ElementCst2D(x, y, z) =>
        val vy: cp.CPIntVar = postIntExpressionAndGetVar(y)
        val vz: cp.CPIntVar = postIntExpressionAndGetVar(z)
        cpSolver.add(new cp.constraints.ElementCst2D(x, vy, vz, result))
      case Max(x) =>
        val vx = x.map(postIntExpressionAndGetVar)
        cpSolver.add(cp.maximum(vx, result))
      case Min(x) =>
        val vx = x.map(postIntExpressionAndGetVar)
        cpSolver.add(cp.minimum(vx, result))
      case Minus(x, y) =>
        cpSolver.add(new oscar.cp.constraints.BinarySum(result,postIntExpressionAndGetVar(y),postIntExpressionAndGetVar(x)))
      case Modulo(x, y) =>
        cpSolver.add(new CPIntVarOps(postIntExpressionAndGetVar(x)) % y == result)
      case Prod(Array(x, y)) => //binary prod
        cpSolver.add(new oscar.cp.constraints.MulVar(postIntExpressionAndGetVar(x), postIntExpressionAndGetVar(y), result))
      case Prod(x) => //n-ary prod
        //OscaR only has binary product; transform into a balanced binary tree to minimise number of constraints
        def recurmul(exprs: Array[cp.CPIntVar]): Array[cp.CPIntVar] = {
          if (exprs.length == 2)
            exprs
          else
            recurmul(exprs.grouped(2).map({
              case Array(a, b) => CPIntVarOps(a) * b
              case Array(a) => a
            }).toArray)
        }
        val bprod = recurmul(x.map(postIntExpressionAndGetVar))
        cpSolver.add(new oscar.cp.constraints.MulVar(bprod(0), bprod(1), result))
      case Sum(Array(x, y)) => //binary sum
        cpSolver.add(new oscar.cp.constraints.BinarySum(postIntExpressionAndGetVar(x), postIntExpressionAndGetVar(y), result))
      case Sum(x) => //n-ary sum
        cpSolver.add(cp.modeling.constraint.sum(x.map(postIntExpressionAndGetVar), result))
      case UnaryMinus(a) =>
        cpSolver.add(result === -postIntExpressionAndGetVar(a))
      case WeightedSum(x, y) =>
        cpSolver.add(cp.modeling.constraint.weightedSum(y, x.map(postIntExpressionAndGetVar), result))
      case Div(x, y) =>
        cpSolver.add(new oscar.cp.constraints.MulCte(result, y, postIntExpressionAndGetVar(x - Modulo(x, y))))
      case NValues(x) =>
        cpSolver.add(new oscar.cp.constraints.AtLeastNValue(x.map(postIntExpressionAndGetVar), result))
      case Exponent(x, y) => throw new Exception() //TODO: real exception
      case v: IntVar =>
        cpSolver.add(postIntExpressionAndGetVar(v) === result)
      case default => throw new Exception("Unknown IntExpression "+default.getClass.toString) //TODO: put a real exception here
    }
    // Must ABSOLUTELY be put AFTER all the calls to postIntExpressionAndGetVar
    expr_cache.put(expr, result)
    ret
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
  def postConstraintForPossibleConstant(a: IntExpression, b: IntExpression,
                                        leftCst: (Int, cp.CPIntVar) => cp.Constraint,
                                        rightCst: (cp.CPIntVar, Int) => cp.Constraint,
                                        allVar: (cp.CPIntVar, cp.CPIntVar) => cp.Constraint): Outcome = {
    (a,b) match {
      case (Constant(value), variable:IntExpression) => cpSolver.add(leftCst(value, postIntExpressionAndGetVar(variable)))
      case (variable: IntExpression, Constant(value)) => cpSolver.add(rightCst(postIntExpressionAndGetVar(variable), value))
      case (v1: IntExpression, v2: IntExpression) => cpSolver.add(allVar(postIntExpressionAndGetVar(v1), postIntExpressionAndGetVar(v2)))
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
