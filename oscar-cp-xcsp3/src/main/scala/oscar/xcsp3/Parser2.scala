package oscar.xcsp3

import java.io.File
import java.util

import org.xcsp.common.XEnums._
import org.xcsp.common.predicates.{XNodeExpr, XNodeLeaf, XNodeParent}
import org.xcsp.parser.XCallbacks
import org.xcsp.parser.XCallbacks.XCallbacksParameters
import org.xcsp.parser.XParser.{Condition, ConditionIntvl, ConditionVal, ConditionVar}
import org.xcsp.parser.XVariables._
import oscar.modeling.algebra._
import oscar.modeling.constraints._
import oscar.modeling.models.ModelDeclaration
import oscar.modeling.solvers.cp.branchings.Branching
import oscar.modeling.solvers.cp.decompositions.CartProdRefinement
import oscar.modeling.solvers.cp.{DistributedCPApp, DistributedCPAppConfig}
import oscar.modeling.vars.IntVar

import scala.collection.mutable

/**
  * An XCSP3 parser that converts an XCSP3 instance to an OscaR-Modeling model
  * @param modelDeclaration OscaR-Modeling base model declaration
  * @param filename path to the instance
  */
private class XCSP3Parser2(modelDeclaration: ModelDeclaration, filename: String) extends XCallbacksDecomp {
  implicit val implModelDeclaration = modelDeclaration
  val varHashMap = collection.mutable.LinkedHashMap[String, IntExpression]() //automagically maintains the order of insertion

  // Force parameter update
  XCallbacks.callbacksParameters.clear()
  XCallbacks.callbacksParameters.put(XCallbacksParameters.RECOGNIZE_SPECIAL_UNARY_INTENSION_CASES, new Object)
  XCallbacks.callbacksParameters.put(XCallbacksParameters.RECOGNIZE_SPECIAL_BINARY_INTENSION_CASES,  new Object)
  XCallbacks.callbacksParameters.put(XCallbacksParameters.RECOGNIZE_SPECIAL_TERNARY_INTENSION_CASES,  new Object)
  //XCallbacks.callbacksParameters.put(XCallbacksParameters.RECOGNIZE_SPECIAL_COUNT_CASES,  new Object)
  XCallbacks.callbacksParameters.put(XCallbacksParameters.RECOGNIZE_SPECIAL_NVALUES_CASES,  new Object)
  XCallbacks.callbacksParameters.put(XCallbacksParameters.INTENSION_TO_EXTENSION_ARITY_LIMIT, 0:java.lang.Integer) // included
  XCallbacks.callbacksParameters.put(XCallbacksParameters.INTENSION_TO_EXTENSION_SPACE_LIMIT, 1000000:java.lang.Integer)
  XCallbacks.callbacksParameters.put(XCallbacksParameters.INTENSION_TO_EXTENSION_PRIORITY, false:java.lang.Boolean)


  loadInstance(filename)

  // Variables
  override def buildVarInteger(x: XVarInteger, minValue: Int, maxValue: Int): Unit = {
    //println("Adding var "+x.id)
    varHashMap += ((x.id, IntVar(minValue, maxValue, Some(x.id))))
  }

  override def buildVarInteger(x: XVarInteger, values: Array[Int]): Unit = {
    varHashMap += ((x.id, IntVar(values.toSet, Some(x.id))))
  }

  // Constraints
  override def buildCtrAllDifferent(id: String, list: Array[XVarInteger]): Unit = {
    //println("Adding AllDifferent "+id)
    val cst = AllDifferent(list.map(elem => varHashMap(elem.id)))
    modelDeclaration.add(cst)
  }

  override def buildCtrPrimitive(id: String, xvi: XVarInteger, opa: TypeArithmeticOperator, yvi: XVarInteger, op: TypeConditionOperatorRel, k: Int): Unit = {
    //println(id)
    val x = varHashMap(xvi.id)
    val y = varHashMap(yvi.id)
    val r: IntExpression = opa match {
      case TypeArithmeticOperator.ADD => x + y
      case TypeArithmeticOperator.DIST => Dist(x, y)
      case TypeArithmeticOperator.DIV => x / y.evaluate()
      case TypeArithmeticOperator.MUL => x * y
      case TypeArithmeticOperator.SUB => x - y
      case TypeArithmeticOperator.MOD => throw new Exception("Modulo between vars is not implemented")
    }
    val r2 = (op match {
      case TypeConditionOperatorRel.EQ => r === k
      case TypeConditionOperatorRel.GE => r >= k
      case TypeConditionOperatorRel.GT => r > k
      case TypeConditionOperatorRel.LE => r <= k
      case TypeConditionOperatorRel.LT => r < k
      case TypeConditionOperatorRel.NE => r !== k
    }).toConstraint
    modelDeclaration.add(r2)
  }

  override def buildCtrPrimitive(id: String, xvi: XVarInteger, op: TypeConditionOperatorRel, k: Int): Unit = {
    val r = varHashMap(xvi.id)
    val r2 = (op match {
      case TypeConditionOperatorRel.EQ => r === k
      case TypeConditionOperatorRel.GE => r >= k
      case TypeConditionOperatorRel.GT => r > k
      case TypeConditionOperatorRel.LE => r <= k
      case TypeConditionOperatorRel.LT => r < k
      case TypeConditionOperatorRel.NE => r !== k
    }).toConstraint
    modelDeclaration.add(r2)
  }

  override def buildCtrPrimitive(id: String, xvi: XVarInteger, opa: TypeArithmeticOperator, yvi: XVarInteger, op: TypeConditionOperatorRel, zvi: XVarInteger): Unit = {
    val x = varHashMap(xvi.id)
    val y = varHashMap(yvi.id)
    val z = varHashMap(zvi.id)
    val r: IntExpression = opa match {
      case TypeArithmeticOperator.ADD => x + y
      case TypeArithmeticOperator.DIST => Dist(x, y)
      case TypeArithmeticOperator.DIV => x / y.evaluate()
      case TypeArithmeticOperator.MUL => x * y
      case TypeArithmeticOperator.SUB => x - y
      case TypeArithmeticOperator.MOD => throw new Exception("Modulo between vars is not implemented")
    }
    val r2 = (op match {
      case TypeConditionOperatorRel.EQ => r === z
      case TypeConditionOperatorRel.GE => r >= z
      case TypeConditionOperatorRel.GT => r > z
      case TypeConditionOperatorRel.LE => r <= z
      case TypeConditionOperatorRel.LT => r < z
      case TypeConditionOperatorRel.NE => r !== z
    }).toConstraint
    modelDeclaration.add(r2)
  }

  def _buildCrtWithCondition(id: String, expr: IntExpression, operator: Condition): Unit = {
    val expr2: Array[BoolExpression] = operator match {
      case c: ConditionVal =>
        c.operator match {
          case TypeConditionOperator.EQ => Array(expr === c.k)
          case TypeConditionOperator.GE => Array(expr >= c.k)
          case TypeConditionOperator.GT => Array(expr > c.k)
          case TypeConditionOperator.LE => Array(expr <= c.k)
          case TypeConditionOperator.LT => Array(expr < c.k)
          case TypeConditionOperator.NE => Array(expr !== c.k)
          case TypeConditionOperator.IN => throw new XCSP3ParseException("Invalid operator IN on a ConditionVal")
          case TypeConditionOperator.NOTIN => throw new XCSP3ParseException("Invalid operator NOTIN on a ConditionVal")
        }
      case c: ConditionVar =>
        c.operator match {
          case TypeConditionOperator.EQ => Array(expr === varHashMap(c.x.id))
          case TypeConditionOperator.GE => Array(expr >= varHashMap(c.x.id))
          case TypeConditionOperator.GT => Array(expr > varHashMap(c.x.id))
          case TypeConditionOperator.LE => Array(expr <= varHashMap(c.x.id))
          case TypeConditionOperator.LT => Array(expr < varHashMap(c.x.id))
          case TypeConditionOperator.NE => Array(expr !== varHashMap(c.x.id))
          case TypeConditionOperator.IN => throw new XCSP3ParseException("Invalid operator IN on a ConditionVar")
          case TypeConditionOperator.NOTIN => throw new XCSP3ParseException("Invalid operator NOTIN on a ConditionVar")
        }
      case c: ConditionIntvl =>
        c.operator match {
          case TypeConditionOperator.EQ => throw new XCSP3ParseException("Invalid operator EQ on a ConditionIntvl")
          case TypeConditionOperator.GE => throw new XCSP3ParseException("Invalid operator GE on a ConditionIntvl")
          case TypeConditionOperator.GT => throw new XCSP3ParseException("Invalid operator GT on a ConditionIntvl")
          case TypeConditionOperator.LE => throw new XCSP3ParseException("Invalid operator LE on a ConditionIntvl")
          case TypeConditionOperator.LT => throw new XCSP3ParseException("Invalid operator LT on a ConditionIntvl")
          case TypeConditionOperator.NE => throw new XCSP3ParseException("Invalid operator NE on a ConditionIntvl")
          case TypeConditionOperator.IN => Array(expr <= c.max, expr >= c.min)
          case TypeConditionOperator.NOTIN => Array(Or(Array(expr > c.max, expr < c.min)))
        }
    }
    expr2.map(_.toConstraint).foreach(modelDeclaration.add)
  }

  override def buildCtrSum(id: String, list: Array[XVarInteger], condition: Condition): Unit = {
    _buildCrtWithCondition(id, Sum(list.map(i => varHashMap(i.id))), condition)
  }

  override def buildCtrSum(id: String, list: Array[XVarInteger], coeffs: Array[Int], condition: Condition): Unit = {
    _buildCrtWithCondition(id, WeightedSum(list.map(i => varHashMap(i.id)), coeffs), condition)
  }

  override def buildCtrSum(id: String, list: Array[XVarInteger], coeffs: Array[XVarInteger], condition: Condition): Unit = {
    _buildCrtWithCondition(id, Sum(list.zip(coeffs).map(i => varHashMap(i._1.id)*varHashMap(i._2.id))), condition)
  }

  protected lazy val expr_cache: mutable.HashMap[String, IntExpression] = mutable.HashMap()

  def _recursiveIntentionBuilder[V](node: XNodeExpr[V]): IntExpression = {
    expr_cache.getOrElseUpdate(node.toString(Array(), false), {
      node match {
        case x: XNodeLeaf[V] => _recursiveIntentionBuilderLeafNode(x)
        case x: XNodeParent[V] => _recursiveIntentionBuilderParentNode(x)
      }
    })
  }

  def _recursiveIntentionBuilderLeafNode[V](node: XNodeLeaf[V]): IntExpression = {
    node.getType match {
      case TypeExpr.VAR => varHashMap(node.value.toString)
      case TypeExpr.LONG => node.value.asInstanceOf[Long].toInt
    }
  }

  def _recursiveIntentionBuilderParentNode[V](tree: XNodeParent[V]): IntExpression = {
    tree.getType match {
      case TypeExpr.IN =>
        assert(tree.sons(1).getType == TypeExpr.SET)
        try{
          val set = tree.sons(1).asInstanceOf[XNodeParent[V]].sons.map(i => i.asInstanceOf[XNodeLeaf[V]].value.asInstanceOf[Long].toInt)
          InSet(_recursiveIntentionBuilder(tree.sons(0)), set.toSet)
        }
        catch {
          case a: ClassCastException =>
            //Cannot cast to XNodeLeaf => not only integers
            val main = _recursiveIntentionBuilder(tree.sons(0))
            Or(tree.sons(1).asInstanceOf[XNodeParent[V]].sons.map(x => _recursiveIntentionBuilder(x) === main))
        }
      case TypeExpr.OR => Or(tree.sons.map(_recursiveIntentionBuilder(_).asInstanceOf[BoolExpression]))
      case TypeExpr.AND => And(tree.sons.map(_recursiveIntentionBuilder(_).asInstanceOf[BoolExpression]))
      case TypeExpr.EQ => Eq(tree.sons.map(_recursiveIntentionBuilder))
      case TypeExpr.LT => _recursiveIntentionBuilder(tree.sons(0)) < _recursiveIntentionBuilder(tree.sons(1))
      case TypeExpr.LE => _recursiveIntentionBuilder(tree.sons(0)) <= _recursiveIntentionBuilder(tree.sons(1))
      case TypeExpr.GE => _recursiveIntentionBuilder(tree.sons(0)) >= _recursiveIntentionBuilder(tree.sons(1))
      case TypeExpr.GT => _recursiveIntentionBuilder(tree.sons(0)) > _recursiveIntentionBuilder(tree.sons(1))
      case TypeExpr.NE => _recursiveIntentionBuilder(tree.sons(0)) !== _recursiveIntentionBuilder(tree.sons(1))
      case TypeExpr.ADD => Sum(tree.sons.map(_recursiveIntentionBuilder))
      case TypeExpr.SUB => _recursiveIntentionBuilder(tree.sons(0)) - _recursiveIntentionBuilder(tree.sons(1))
      case TypeExpr.DIV =>
        assert(tree.sons(1).getType == TypeExpr.LONG)
        _recursiveIntentionBuilder(tree.sons(0)) / tree.sons(1).asInstanceOf[XNodeLeaf[V]].value.asInstanceOf[Long].toInt
      case TypeExpr.ABS => Abs(_recursiveIntentionBuilder(tree.sons(0)))
      case TypeExpr.NEG => -_recursiveIntentionBuilder(tree.sons(0))
      case TypeExpr.MIN => Min(tree.sons.map(_recursiveIntentionBuilder))
      case TypeExpr.MAX => Max(tree.sons.map(_recursiveIntentionBuilder))
      case TypeExpr.DIST => Dist(_recursiveIntentionBuilder(tree.sons(0)), _recursiveIntentionBuilder(tree.sons(1)))
      case TypeExpr.NOT => !_recursiveIntentionBuilder(tree.sons(0))
      case TypeExpr.MOD =>
        assert(tree.sons(1).getType == TypeExpr.LONG)
        _recursiveIntentionBuilder(tree.sons(0)) % tree.sons(1).asInstanceOf[XNodeLeaf[V]].value.asInstanceOf[Long].toInt
      case TypeExpr.MUL =>
        Prod(tree.sons.map(_recursiveIntentionBuilder))
      case TypeExpr.IFF =>
        val csts = tree.sons.map(_recursiveIntentionBuilder).foldLeft((List[BoolExpression](), null: IntExpression))((cur, next) => {
          if(cur._2 == null) (List[BoolExpression](), next)
          else ((cur._2 === next) :: cur._1, next)
        })._1
        if(csts.length == 1)
          csts.last
        else
          And(csts.toArray) //TODO improve this, need for an n-ary eq
      case TypeExpr.IMP =>
        _recursiveIntentionBuilder(tree.sons(0)).asInstanceOf[BoolExpression] ==> _recursiveIntentionBuilder(tree.sons(1)).asInstanceOf[BoolExpression]
      case TypeExpr.XOR =>
        _recursiveIntentionBuilder(tree.sons(0)).asInstanceOf[BoolExpression] ^ _recursiveIntentionBuilder(tree.sons(1)).asInstanceOf[BoolExpression]
      case TypeExpr.SQR => ??? //1
      case TypeExpr.POW => ??? //2
      case TypeExpr.SET => ??? //0, Integer.MAX_VALUE
      case TypeExpr.IF => ??? //3
      case TypeExpr.CARD => ??? //1
      case TypeExpr.UNION => ??? //2, Integer.MAX_VALUE
      case TypeExpr.INTER => ??? //2, Integer.MAX_VALUE
      case TypeExpr.DIFF => ??? //2
      case TypeExpr.SDIFF => ??? //2, Integer.MAX_VALUE
      case TypeExpr.HULL => ??? //1
      case TypeExpr.DJOINT => ??? //2
      case TypeExpr.SUBSET => ??? //2
      case TypeExpr.SUBSEQ => ??? //2
      case TypeExpr.SUPSET => ??? //2
      case TypeExpr.SUPSEQ => ??? //2
      case TypeExpr.CONVEX => ??? //1
      case TypeExpr.FDIV => ??? //2
      case TypeExpr.FMOD => ??? //2
      case TypeExpr.SQRT => ??? //1
      case TypeExpr.NROOT => ??? //2
      case TypeExpr.EXP => ??? //1
      case TypeExpr.LN => ??? //1
      case TypeExpr.LOG => ??? //2
      case TypeExpr.SIN => ??? //1
      case TypeExpr.COS => ??? //1
      case TypeExpr.TAN => ??? //1
      case TypeExpr.ASIN => ??? //1
      case TypeExpr.ACOS => ??? //1
      case TypeExpr.ATAN => ??? //1
      case TypeExpr.SINH => ??? //1
      case TypeExpr.COSH => ??? //1
      case TypeExpr.TANH => ??? //1
    }
  }

  override def buildCtrIntension(id: String, scope: Array[XVarInteger], syntaxTreeRoot: XNodeParent[XVar]): Unit = {
    val cst = _recursiveIntentionBuilder(syntaxTreeRoot).asInstanceOf[BoolExpression].toConstraint
    modelDeclaration.add(cst)
  }

  override def buildCtrCardinality(id: String, list: Array[XVarInteger], closed: Boolean, values: Array[Int], occurs: Array[Int]): Unit = {
    val cst = GCC(list.map(i => varHashMap(i.id)), values, occurs, occurs)
    modelDeclaration.add(cst)
  }

  override def buildCtrCardinality(id: String, list: Array[XVarInteger], closed: Boolean, values: Array[Int], occursMin: Array[Int], occursMax: Array[Int]): Unit = {
    val cst = GCC(list.map(i => varHashMap(i.id)), values, occursMin, occursMax)
    modelDeclaration.add(cst)
  }

  override def buildCtrCardinality(id: String, list: Array[XVarInteger], closed: Boolean, values: Array[Int], occurs: Array[XVarInteger]): Unit = {
    val cst = GCC(list.map(i => varHashMap(i.id)), values, occurs.map(i => varHashMap(i.id)))
    modelDeclaration.add(cst)
  }

  override def buildCtrOrdered(id: String, list: Array[XVarInteger], operator: TypeOperator): Unit = {
    val rel: (IntExpression, IntExpression) => Constraint = operator match {
      case TypeOperator.GE => (a, b) => a >= b
      case TypeOperator.GT => (a, b) => a > b
      case TypeOperator.LE => (a, b) => a <= b
      case TypeOperator.LT => (a, b) => a < b
      case TypeOperator.SUBSEQ => ???
      case TypeOperator.SUBSET => ???
      case TypeOperator.SUPSEQ => ???
      case TypeOperator.SUPSET => ???
    }
    list.map(i => varHashMap(i.id)).sliding(2).map{x => rel(x(0), x(1))}.foreach(modelDeclaration.post)
  }


  override def buildCtrExtension(id: String, x: XVarInteger, values: Array[Int], positive: Boolean, flags: util.Set[TypeFlag]): Unit = {
    assert(!flags.contains(TypeFlag.STARRED_TUPLES)) // no sense!
    if(positive) {
      //InSet constraint
      modelDeclaration.add(InSet(varHashMap(x.id), values.toSet))
    }
    else {
      val v = varHashMap(x.id)
      values.foreach(y => modelDeclaration.add(v !== y))
    }
  }

  override def buildCtrExtension(id: String, list: Array[XVarInteger], tuples: Array[Array[Int]], positive: Boolean, flags: util.Set[TypeFlag]): Unit = {
    //println(list.map(x => x.id).mkString(" "))
    val cst: Constraint = if(positive) {
      Table(list.map(x => varHashMap(x.id)), tuples, if(flags.contains(TypeFlag.STARRED_TUPLES)) Some(Integer.MAX_VALUE-1) else None)
    }
    else {
      NegativeTable(list.map(x => varHashMap(x.id)), tuples, if(flags.contains(TypeFlag.STARRED_TUPLES)) Some(Integer.MAX_VALUE-1) else None)
    }
    modelDeclaration.add(cst)
  }

  override def buildCtrInstantiation(id: String, list: Array[XVarInteger], values: Array[Int]): Unit = {
    val csts = list.zip(values).map(x => (varHashMap(x._1.id) === x._2).toConstraint)
    csts.foreach(modelDeclaration.add)
  }

  override def buildCtrElement(id: String, list: Array[XVarInteger], startIndex: Int, index: XVarInteger, rank: TypeRank, value: XVarInteger): Unit = {
    if(rank != TypeRank.ANY)
      throw new Exception("Element constraint only supports ANY as position for the index")
    val array = list.map(x => varHashMap(x.id))
    val indexExpr = if(startIndex == 0) varHashMap(index.id) else varHashMap(index.id) - startIndex
    val valueExpr = varHashMap(value.id)
    modelDeclaration.add(array(indexExpr) === valueExpr)
  }

  override def buildCtrElement(id: String, list: Array[XVarInteger], startIndex: Int, index: XVarInteger, rank: TypeRank, value: Int): Unit = {
    if(rank != TypeRank.ANY)
      throw new Exception("Element constraint only supports ANY as position for the index")
    val array = list.map(x => varHashMap(x.id))
    val indexExpr = if(startIndex == 0) varHashMap(index.id) else varHashMap(index.id) - startIndex
    modelDeclaration.add(array(indexExpr) === value)
  }

  override def buildCtrAmong(id: String, list: Array[XVarInteger], values: Array[Int], k: Int): Unit = {
    modelDeclaration.add(Among(k, list.map(x => varHashMap(x.id)), values.toSet))
  }

  override def buildCtrAmong(id: String, list: Array[XVarInteger], values: Array[Int], k: XVarInteger): Unit = {
    modelDeclaration.add(Among(varHashMap(k.id), list.map(x => varHashMap(x.id)), values.toSet))
  }

  override def buildCtrExactly(id: String, list: Array[XVarInteger], value: Int, k: Int): Unit = {
    modelDeclaration.add(Among(k, list.map(x => varHashMap(x.id)), value))
  }

  override def buildCtrExactly(id: String, list: Array[XVarInteger], value: Int, k: XVarInteger): Unit = {
    modelDeclaration.add(Among(varHashMap(k.id), list.map(x => varHashMap(x.id)), value))
  }

  override def buildCtrAtLeast(id: String, list: Array[XVarInteger], value: Int, k: Int): Unit = {
    modelDeclaration.add(AtLeast(k, list.map(x => varHashMap(x.id)), value))
  }

  override def buildCtrAtMost(id: String, list: Array[XVarInteger], value: Int, k: Int): Unit = {
    modelDeclaration.add(AtMost(k, list.map(x => varHashMap(x.id)), value))
  }

  override def buildCtrMinimum(id: String, list: Array[XVarInteger], condition: Condition): Unit = {
    _buildCrtWithCondition(id, Min(list.map(x => varHashMap(x.id))), condition)
  }

  override def buildCtrMaximum(id: String, list: Array[XVarInteger], condition: Condition): Unit = {
    _buildCrtWithCondition(id, Max(list.map(x => varHashMap(x.id))), condition)
  }

  override def buildCtrLex(id: String, lists: Array[Array[XVarInteger]], operator: TypeOperator): Unit = {
    val constraintType: (Array[IntExpression], Array[IntExpression]) => Constraint = operator match {
      case TypeOperator.GE => LexGeq.apply
      case TypeOperator.GT => LexGr.apply
      case TypeOperator.LE => LexLeq
      case TypeOperator.LT => LexLr.apply
    }
    lists.map(tuple => tuple.map(x => varHashMap(x.id))).sliding(2).foreach(tuples => modelDeclaration.add(constraintType(tuples(0), tuples(1))))
  }

  def _getConditionVar(condition: Condition): IntExpression = condition match {
    case c: ConditionVal => c.k
    case c: ConditionVar => varHashMap(c.x.id)
  }

  def buildCtrCumulative(id: String, starts: Array[IntExpression], durations: Array[IntExpression], ends: Array[IntExpression], demands: Array[IntExpression], minCum: Boolean, limit: IntExpression): Unit = {
    if(minCum)
      modelDeclaration.add(MinCumulativeResource(starts, durations, ends, demands, limit))
    else
      modelDeclaration.add(MaxCumulativeResource(starts, durations, ends, demands, limit))
  }

  def buildCtrCumulative(id: String, starts: Array[IntExpression], durations: Array[IntExpression], ends: Array[IntExpression], demands: Array[IntExpression], condition: Condition): Unit = {
    val constraintParameters = condition.operator match {
      case TypeConditionOperator.EQ =>
        buildCtrCumulative(id, starts, durations, ends, demands, true, _getConditionVar(condition))
        buildCtrCumulative(id, starts, durations, ends, demands, false, _getConditionVar(condition))
      case TypeConditionOperator.GE => buildCtrCumulative(id, starts, durations, ends, demands, false, _getConditionVar(condition))
      case TypeConditionOperator.GT => buildCtrCumulative(id, starts, durations, ends, demands, false, _getConditionVar(condition)+1)
      case TypeConditionOperator.LE => buildCtrCumulative(id, starts, durations, ends, demands, true, _getConditionVar(condition))
      case TypeConditionOperator.LT => buildCtrCumulative(id, starts, durations, ends, demands, true, _getConditionVar(condition)-1)
      case TypeConditionOperator.NE =>
        val tVar = IntVar(0, demands.map(_.max).sum)
        modelDeclaration.add(tVar !== _getConditionVar(condition))
        buildCtrCumulative(id, starts, durations, ends, demands, true, tVar)
        buildCtrCumulative(id, starts, durations, ends, demands, false, tVar)
      case TypeConditionOperator.IN =>
        val tVar = IntVar(condition.asInstanceOf[ConditionIntvl].min, condition.asInstanceOf[ConditionIntvl].max)
        buildCtrCumulative(id, starts, durations, ends, demands, true, tVar)
        buildCtrCumulative(id, starts, durations, ends, demands, false, tVar)
      case TypeConditionOperator.NOTIN =>
        val tVar = IntVar(0, demands.map(_.max).sum)
        modelDeclaration.add(Or(Array(tVar < condition.asInstanceOf[ConditionIntvl].min, tVar > condition.asInstanceOf[ConditionIntvl].max)))
        buildCtrCumulative(id, starts, durations, ends, demands, true, tVar)
        buildCtrCumulative(id, starts, durations, ends, demands, false, tVar)
    }
  }

  override def buildCtrCumulative(id: String, origins: Array[XVarInteger], lengths: Array[XVarInteger], ends: Array[XVarInteger], heights: Array[XVarInteger], condition: Condition): Unit = {
    val mOrigins = origins map(x => varHashMap(x.id))
    val mLengths = lengths map(x => varHashMap(x.id))
    val mEnds = ends map(x => varHashMap(x.id))
    val mHeights = heights map(x => varHashMap(x.id))
    buildCtrCumulative(id, mOrigins, mLengths, mEnds, mHeights, condition)
  }

  override def buildCtrCumulative(id: String, origins: Array[XVarInteger], lengths: Array[Int], ends: Array[XVarInteger], heights: Array[Int], condition: Condition): Unit = {
    val mOrigins = origins map(x => varHashMap(x.id))
    val mLengths = lengths map(x => IntVar(x))
    val mEnds = ends map(x => varHashMap(x.id))
    val mHeights = heights map(x => IntVar(x))
    buildCtrCumulative(id, mOrigins, mLengths, mEnds, mHeights, condition)
  }

  override def buildCtrCumulative(id: String, origins: Array[XVarInteger], lengths: Array[Int], ends: Array[XVarInteger], heights: Array[XVarInteger], condition: Condition): Unit = {
    val mOrigins = origins map(x => varHashMap(x.id))
    val mLengths = lengths map(x => IntVar(x))
    val mEnds = ends map(x => varHashMap(x.id))
    val mHeights = heights map(x => varHashMap(x.id))
    buildCtrCumulative(id, mOrigins, mLengths, mEnds, mHeights, condition)
  }

  override def buildCtrCumulative(id: String, origins: Array[XVarInteger], lengths: Array[XVarInteger], ends: Array[XVarInteger], heights: Array[Int], condition: Condition): Unit = {
    val mOrigins = origins map(x => varHashMap(x.id))
    val mLengths = lengths map(x => varHashMap(x.id))
    val mEnds = ends map(x => varHashMap(x.id))
    val mHeights = heights map(x => IntVar(x))
    buildCtrCumulative(id, mOrigins, mLengths, mEnds, mHeights, condition)
  }

  override def buildCtrCircuit(id: String, list: Array[XVarInteger], startIndex: Int): Unit = {
    modelDeclaration.add(SubCircuit(list.map(x => varHashMap(x.id)), startIndex))
  }

  override def buildCtrCircuit(id: String, list: Array[XVarInteger], startIndex: Int, size: Int): Unit = {
    if(size == list.length)
      modelDeclaration.add(Circuit(list.map(x => varHashMap(x.id))))
    else {
      val vars = list.map(x => varHashMap(x.id))
      // count non-self-looping variables
      modelDeclaration.add(Sum(vars.zipWithIndex.map({case (v, idx) => v !== (idx+startIndex)})) === size)
      modelDeclaration.add(SubCircuit(list.map(x => varHashMap(x.id)), startIndex))
    }
  }

  override def buildCtrCircuit(id: String, list: Array[XVarInteger], startIndex: Int, size: XVarInteger): Unit = {
    val vars = list.map(x => varHashMap(x.id))
    // count non-self-looping variables
    modelDeclaration.add(Sum(vars.zipWithIndex.map({case (v, idx) => v !== (idx+startIndex)})) === varHashMap(size.id))
    modelDeclaration.add(SubCircuit(list.map(x => varHashMap(x.id)), startIndex))
  }

  //unaryResource(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar])
  override def buildCtrNoOverlap(id: String, origins: Array[XVarInteger], lengths: Array[Int], zeroIgnored: Boolean): Unit = {
    val (starts, durations) = origins.zip(lengths).filter({case (v, l) => l != 0 || !zeroIgnored}).unzip
    val ends = buildEndsFromStartAndLength(starts, durations)
    val mStarts = starts.map(x => varHashMap(x.id))
    val mDurations = durations.map(x => Constant(x):IntExpression)
    val mEnds = ends.map(x => varHashMap(x.id))
    modelDeclaration.post(UnaryResource(mStarts, mDurations, mEnds))
    // Bind start, duration and ends
    (mStarts, mDurations, mEnds).zipped.foreach({case (s, d, e) => modelDeclaration.post((s+d)===e)})
  }

  override def buildCtrNoOverlap(id: String, origins: Array[XVarInteger], lengths: Array[XVarInteger], zeroIgnored: Boolean): Unit = {
    // TODO we ignore the value of zeroIgnored for now
    val ends = buildEndsFromStartAndLength(origins, lengths)
    val mStarts = origins.map(x => varHashMap(x.id))
    val mDurations = lengths.map(x => varHashMap(x.id))
    val mEnds = ends.map(x => varHashMap(x.id))
    if(zeroIgnored) {
      val required: Array[BoolExpression] =  mDurations.map(_ !== 0)
      modelDeclaration.post(UnaryResource(mStarts, mDurations, mEnds, required))
    }
    else {
      modelDeclaration.post(UnaryResource(mStarts, mDurations, mEnds))
    }
    // Bind start, duration and ends
    (mStarts, mDurations, mEnds).zipped.foreach({case (s, d, e) => modelDeclaration.post((s+d)===e)})
  }

  override def buildCtrNoOverlap2D(id: String, x: Array[XVarInteger], dx: Array[Int], y: Array[XVarInteger], dy: Array[Int]): Unit = {
    modelDeclaration.post(DiffN(x.map(e => varHashMap(e.id)), dx.map(Constant), y.map(e => varHashMap(e.id)), dy.map(Constant)))
  }

  override def buildCtrNoOverlap2D(id: String, x: Array[XVarInteger], dx: Array[XVarInteger], y: Array[XVarInteger], dy: Array[XVarInteger]): Unit = {
    modelDeclaration.post(DiffN(x.map(e => varHashMap(e.id)), dx.map(e => varHashMap(e.id)), y.map(e => varHashMap(e.id)), dy.map(e => varHashMap(e.id))))
  }

  override def buildCtrAllEqual(id: String, list: Array[XVarInteger]): Unit = {
    modelDeclaration.post(Eq(list.map(e => varHashMap(e.id))))
  }

  override def buildCtrChannel(id: String, list1: Array[XVarInteger], startIndex1: Int, list2: Array[XVarInteger], startIndex2: Int): Unit = {
    modelDeclaration.post(Inverse(list1.map(e => varHashMap(e.id)).map(e => if(startIndex1 == 0) e else e-startIndex1),
      list2.map(e => varHashMap(e.id)).map(e => if(startIndex2 == 0) e else e-startIndex2)))
  }

  override def buildCtrChannel(id: String, list: Array[XVarInteger], startIndex: Int): Unit = {
    val corrected = list.map(e => varHashMap(e.id)).map(e => if(startIndex == 0) e else e-startIndex)
    modelDeclaration.post(Inverse(corrected, corrected))
  }

  override def buildCtrChannel(id: String, list: Array[XVarInteger], startIndex: Int, pos: XVarInteger): Unit = {
    // In this form, each variable has only domain 0/1. 'pos' should point to the index of the first variable having 1 as value
    val vars = list.map(e => varHashMap(e.id))
    val posVar = varHashMap(pos.id)
    val newVars = list.indices.map(idx => IntVar(Set(idx, list.length))).toArray
    modelDeclaration.post(Inverse(newVars :+ posVar, newVars :+ posVar))
    for(i <- vars.indices)
      modelDeclaration.post(vars(i) === (newVars(i) === i))
  }

  override def buildCtrMDD(id: String, list: Array[XVarInteger], transitions: Array[Array[AnyRef]]): Unit = {
    // TODO move to XCallbacksDecomp
    case class Transition(orig: String, value: Int, dest: String) {
      override def toString = s"($orig,$value,$dest)"
    }
    val allTransitions = transitions.map(t => Transition(t(0).toString, t(1).toString.toInt, t(2).toString))

    def outTransitions(nodeId: String): Array[Transition] = allTransitions.filter(t => t.orig.equals(nodeId))
    def inTransitions(nodeId: String): Array[Transition] = allTransitions.filter(t => t.dest.equals(nodeId))

    val nodes: Set[String] = allTransitions.map(_.orig).toSet union allTransitions.map(_.dest).toSet

    val root = nodes.filter(n => inTransitions(n).size == 0).head
    // val sink = nodes.filter(n => outTransitions(n).size == 0).head

    var tuples: Array[Array[Transition]] = outTransitions(root).map(Array(_))

    for (i <- 0 until list.size - 1) {
      tuples =
        for (t: Array[Transition] <- tuples;
             last = t(t.size - 1).dest;
             j <- outTransitions(last)) yield {
          t :+ j
        }
    }
    val tableTuples = for (t <- tuples) yield t.map(_.value)
    buildCtrExtension(id, list, tableTuples, true, new util.HashSet[TypeFlag]())
  }

  override def buildCtrCount(id: String, list: Array[XVarInteger], values: Array[Int], condition: Condition): Unit = {
    val setOfVal = values.toSet
    val counterVar: IntExpression = Sum(list.map(x => varHashMap(x.id)).map( x => {
      if(setOfVal.size != 1) InSet(x, setOfVal) else x === setOfVal.head
    }))

    condition match {
      case condition if (condition.isInstanceOf[ConditionIntvl]) => {
        val min = condition.asInstanceOf[ConditionIntvl].min
        val max = condition.asInstanceOf[ConditionIntvl].max
        condition.operator match {
          case TypeConditionOperator.IN => {
            modelDeclaration.add(counterVar <= max)
            modelDeclaration.add(counterVar >= min)
          }
          case TypeConditionOperator.NOTIN => {
            for (v <- min to max) {
              modelDeclaration.add(counterVar !== v)
            }
          }
        }
      }
      case condition if (condition.isInstanceOf[ConditionVal]) => {
        val c = condition.asInstanceOf[ConditionVal].k
        condition.operator match {
          case TypeConditionOperator.LE => modelDeclaration.add(counterVar <= c)
          case TypeConditionOperator.GE => modelDeclaration.add(counterVar >= c)
          case TypeConditionOperator.GT => modelDeclaration.add(counterVar > c)
          case TypeConditionOperator.LT => modelDeclaration.add(counterVar < c)
          case TypeConditionOperator.EQ => modelDeclaration.add(counterVar === c)
          case _ => throw new RuntimeException("not supported operator")
        }
      }
      case condition if (condition.isInstanceOf[ConditionVar]) => {
        val c = varHashMap(condition.asInstanceOf[ConditionVar].x.id)
        condition.operator match {
          case TypeConditionOperator.LE => modelDeclaration.add(counterVar <= c)
          case TypeConditionOperator.GE => modelDeclaration.add(counterVar >= c)
          case TypeConditionOperator.GT => modelDeclaration.add(counterVar > c)
          case TypeConditionOperator.LT => modelDeclaration.add(counterVar < c)
          case TypeConditionOperator.EQ => modelDeclaration.add(counterVar === c)
          case _ => throw new RuntimeException("not supported operator")
        }
      }
    }
  }

  override def buildCtrCount(id: String, list: Array[XVarInteger], values: Array[XVarInteger], condition: Condition): Unit = ???

  override def buildCtrAllDifferentExcept(id: String, list: Array[XVarInteger], except: Array[Int]): Unit = throw new Exception("AllDifferentExcept is not implemented")
  override def buildCtrMinimum(id: String, list: Array[XVarInteger], startIndex: Int, index: XVarInteger, rank: TypeRank, condition: Condition): Unit = throw new Exception("Minimum/MinArg is not implemented")
  override def buildCtrMaximum(id: String, list: Array[XVarInteger], startIndex: Int, index: XVarInteger, rank: TypeRank, condition: Condition): Unit = throw new Exception("Maximum/MaxArg is not implemented")
  override def buildCtrCardinality(id: String, list: Array[XVarInteger], closed: Boolean, values: Array[XVarInteger], occurs: Array[XVarInteger]): Unit = throw new Exception("GCC with var cardinalities is not implemented")
  override def buildCtrCardinality(id: String, list: Array[XVarInteger], closed: Boolean, values: Array[XVarInteger], occurs: Array[Int]): Unit = throw new Exception("GCC with var cardinalities is not implemented")
  override def buildCtrCardinality(id: String, list: Array[XVarInteger], closed: Boolean, values: Array[XVarInteger], occursMin: Array[Int], occursMax: Array[Int]): Unit = throw new Exception("GCC with var cardinalities is not implemented")

  // Objectives
  def _getExprForTypeObjective(objtype: TypeObjective, list: Array[XVarInteger]): IntExpression = {
    objtype match {
      case TypeObjective.MAXIMUM => Max(list.map(i => varHashMap(i.id)))
      case TypeObjective.MINIMUM => Min(list.map(i => varHashMap(i.id)))
      case TypeObjective.SUM => Sum(list.map(i => varHashMap(i.id)))
      case TypeObjective.PRODUCT => Prod(list.map(i => varHashMap(i.id)))
      case TypeObjective.EXPRESSION => throw new XCSP3ParseException("TypeObjective.EXPRESSION should not be called without a tree")
      case TypeObjective.LEX => ???
      case TypeObjective.NVALUES => ???
    }
  }

  def _getExprForTypeObjective(objtype: TypeObjective, list: Array[XVarInteger], coefs: Array[Int]): IntExpression = {
    objtype match {
      case TypeObjective.MAXIMUM => Max(list.zip(coefs).map(i => varHashMap(i._1.id)*i._2))
      case TypeObjective.MINIMUM => Min(list.zip(coefs).map(i => varHashMap(i._1.id)*i._2))
      case TypeObjective.SUM => WeightedSum(list.map(i => varHashMap(i.id)), coefs)
      case TypeObjective.EXPRESSION => throw new XCSP3ParseException("TypeObjective.EXPRESSION should not be called without a tree")
      case TypeObjective.PRODUCT => Prod(list.map(i => varHashMap(i.id))) //ignore coefs...
      case TypeObjective.LEX => ???
      case TypeObjective.NVALUES => ???
    }
  }

  override def buildObjToMinimize(id: String, objtype: TypeObjective, list: Array[XVarInteger]): Unit = {
    modelDeclaration.minimize(_getExprForTypeObjective(objtype, list).reify())
  }

  override def buildObjToMaximize(id: String, objtype: TypeObjective, list: Array[XVarInteger]): Unit = {
    modelDeclaration.maximize(_getExprForTypeObjective(objtype, list).reify())
  }

  override def buildObjToMinimize(id: String, x: XVarInteger): Unit = {
    modelDeclaration.minimize(varHashMap(x.id).reify())
  }

  override def buildObjToMaximize(id: String, x: XVarInteger): Unit = {
    modelDeclaration.maximize(varHashMap(x.id).reify())
  }

  override def buildObjToMinimize(id: String, objtype: TypeObjective, list: Array[XVarInteger], coeffs: Array[Int]): Unit = {
    modelDeclaration.maximize(_getExprForTypeObjective(objtype, list, coeffs).reify())
  }

  override def buildObjToMaximize(id: String, objtype: TypeObjective, list: Array[XVarInteger], coeffs: Array[Int]): Unit = {
    modelDeclaration.maximize(_getExprForTypeObjective(objtype, list, coeffs).reify())
  }

  override def buildObjToMinimize(id: String, syntaxTreeRoot: XNodeParent[XVar]): Unit = {
    modelDeclaration.minimize(_recursiveIntentionBuilder(syntaxTreeRoot).reify())
  }

  override def buildObjToMaximize(id: String, syntaxTreeRoot: XNodeParent[XVar]): Unit = {
    modelDeclaration.maximize(_recursiveIntentionBuilder(syntaxTreeRoot).reify())
  }

  ////////////

  override def buildCtrNValuesExcept(id: String, list: Array[XVarInteger], except: Array[Int], condition: Condition): Unit = ???

  override def buildCtrNValues(id: String, list: Array[XVarInteger], condition: Condition): Unit = ???

  override def buildCtrNotAllEqual(id: String, list: Array[XVarInteger]): Unit = ???

  override def buildCtrClause(id: String, pos: Array[XVarInteger], neg: Array[XVarInteger]): Unit = ???

  override def buildCtrRegular(id: String, list: Array[XVarInteger], transitions: Array[Array[AnyRef]], startState: String, finalStates: Array[String]): Unit = ???

  override def buildCtrStretch(id: String, list: Array[XVarInteger], values: Array[Int], widthsMin: Array[Int], widthsMax: Array[Int]): Unit = ???

  override def buildCtrStretch(id: String, list: Array[XVarInteger], values: Array[Int], widthsMin: Array[Int], widthsMax: Array[Int], patterns: Array[Array[Int]]): Unit = ???


}

object XCSP3Parser2 {
  /**
    * Parse a XCSP3 instance
    * @param modelDeclarator the OscaR-Modeling ModelDeclator to be used
    * @param filename the path to the file to parse
    * @return a list of variables, and a solution generator.
    *         The solution generator, to be called when all variables are bound, will return a XCSP3 instantiation
    *         constraint equivalent to the solution found.
    */
  def parse(modelDeclarator: ModelDeclaration, filename: String): (Iterable[IntVar], () => String) = {
    val parser = new XCSP3Parser2(modelDeclarator, filename)
    val vars = parser.varHashMap.values.map(i => i.asInstanceOf[IntVar]).toArray
    (vars, () => parser.generateInstantiationWithSymbolic(vars.map(x => x.getRepresentativeName.get), vars.map(x => x.evaluate())))
  }
}

object Parser2 extends DistributedCPApp[String] with App {
  override lazy val config = new DistributedCPAppConfig {
    val instance = trailArg[String](descr = "Path to the file to parse")
    val check = opt[Boolean]("c", descr = "Check results with the XCSP3 checker")
  }

  val (vars, solutionGenerator) = XCSP3Parser2.parse(this.modelDeclaration, config.instance.get.get)

  setSearch(Branching.binaryFirstFail(vars.toSeq))
  onSolution {
    solutionGenerator()
  }

  setDecompositionStrategy(new CartProdRefinement(vars, Branching.binaryFirstFail(vars.toSeq)))
  val (stats, solutions) = solve()
  println(stats)
  println("-->")
  println(solutions.mkString("\n\n"))


  def testSolution(instancePath: String, solution: String): Unit = {
    val sc = new CheckerLib(instancePath, solution)
    if(sc.getInvalidObjs.nonEmpty)
      throw new XCSP3ValidationException()
    if(sc.getViolatedCtrs.nonEmpty) {
      println(sc.getViolatedCtrs)
      throw new XCSP3ValidationException()
    }
  }
  if(config.check.get.contains(true)) {
    solutions.foreach(x => testSolution(config.instance.get.get, x))
    println("<!-- solutions verified -->")
  }
}

object RunEverything extends DistributedCPApp[String] with App {
  //Basic/Basic-m1-s1/Allergy.xml.lzma -> symbolic
  //Basic/Basic-m1-s1/Purdey.xml.lzma -> symbolic
  //Bibd -> lex
  //PROBLEM WITH KNIGHTS

  def getFolderContent(dir: String):List[File] = {
    val d = new File(dir)
    if (d.exists && d.isDirectory) {
      d.listFiles.toList
    } else {
      List[File]()
    }
  }

  val inputs = getFolderContent("/Users/dervalguillaume/Downloads/xcsp3medium/")
    .filter(_.isDirectory)
    .map(f => (f.getName, f))
    .flatMap(x => getFolderContent(x._2.getAbsolutePath).filter(_.isDirectory).map(f => (x._1+"/"+f.getName, f)))
      .flatMap(x => {
        getFolderContent(x._2.getAbsolutePath).filter(_.getName.endsWith(".lzma")).sortBy(f => f.length()).map(f => (x._1, f.getName, f.getAbsolutePath))

      })

  val working = scala.collection.mutable.MutableList[String]()
  val notworking = scala.collection.mutable.MutableList[String]()

  val results = scala.util.Random.shuffle(inputs).map(tuple => {
    val (instanceName, fileName, instancePath) = tuple
    modelDeclaration.apply(modelDeclaration.getCurrentModel) {
      try {
        println("<!--" + instancePath)
        val (vars, solutionGenerator) = XCSP3Parser2.parse(this.modelDeclaration, instancePath)

        setSearch(Branching.binaryFirstFail(vars.toSeq))
        onSolution {
          solutionGenerator()
        }

        setDecompositionStrategy(new CartProdRefinement(vars, Branching.binaryFirstFail(vars.toSeq)))
        val (stats, solutions) = solve()
        //println(stats)
        println("-->")
        //println(solutions.mkString("\n\n"))
        solutions.foreach(x => testSolution(instancePath, x))
        //println("<!-- solutions verified -->")

        def testSolution(instancePath: String, solution: String): Unit = {
          if(!new CheckerLib(instancePath, solution).valid)
            throw new XCSP3ValidationException()
        }

        (instanceName, instancePath, "ok")
      }
      catch {
        case a: XCSP3TimeoutException => (instanceName, instancePath, "timeout")
        case a: XCSP3ValidationException => (instanceName, instancePath, "validation error")
        case a: Exception => (instanceName, instancePath, "exception")
        case a: NotImplementedError => (instanceName, instancePath, "not implemented")
      }
    }
  }).seq.sortBy(x => x._3)
  println(results.mkString("\n"))
}