import java.io.File
import java.util

import algebra._
import constraints._
import models.ModelDeclaration
import org.xcsp.common.XEnums._
import org.xcsp.common.predicates.{XNodeExpr, XNodeLeaf, XNodeParent}
import org.xcsp.parser.XParser.{Condition, ConditionVal, ConditionVar}
import org.xcsp.parser.XVariables._
import solvers.cp.branchings.Branching
import solvers.cp.decompositions.CartProdRefinement
import solvers.cp.{DistributedCPApp, DistributedCPAppConfig}
import vars.IntVar

/**
  * An XCSP3 parser that converts an XCSP3 instance to an OscaR-Modeling model
  * @param modelDeclaration OscaR-Modeling base model declaration
  * @param filename path to the instance
  */
private class XCSP3Parser(modelDeclaration: ModelDeclaration, filename: String) extends XCallbacksDecomp {
  implicit val implModelDeclaration = modelDeclaration
  val varHashMap = collection.mutable.LinkedHashMap[String, IntExpression]() //automagically maintains the order of insertion

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
      case TypeArithmeticOperator.DIST => Abs(x - y)
      case TypeArithmeticOperator.DIV => x / y
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
      case TypeArithmeticOperator.DIST => Abs(x - y)
      case TypeArithmeticOperator.DIV => x / y
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
    val expr2: BoolExpression = operator match {
      case c: ConditionVal =>
        c.operator match {
          case TypeConditionOperator.EQ => expr === c.k
          case TypeConditionOperator.GE => expr >= c.k
          case TypeConditionOperator.GT => expr > c.k
          case TypeConditionOperator.LE => expr <= c.k
          case TypeConditionOperator.LT => expr < c.k
          case TypeConditionOperator.NE => expr !== c.k
          case TypeConditionOperator.IN => ??? // ???
          case TypeConditionOperator.NOTIN => ??? // ???
        }
      case c: ConditionVar =>
        c.operator match {
          case TypeConditionOperator.EQ => expr === varHashMap(c.x.id)
          case TypeConditionOperator.GE => expr >= varHashMap(c.x.id)
          case TypeConditionOperator.GT => expr > varHashMap(c.x.id)
          case TypeConditionOperator.LE => expr <= varHashMap(c.x.id)
          case TypeConditionOperator.LT => expr < varHashMap(c.x.id)
          case TypeConditionOperator.NE => expr !== varHashMap(c.x.id)
          case TypeConditionOperator.IN => ??? // ???
          case TypeConditionOperator.NOTIN => ??? // ???
        }
    }
    val cst = expr2.toConstraint
    modelDeclaration.add(cst)
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

  def _recursiveIntentionBuilder[V](node: XNodeExpr[V]): IntExpression = {
    node match {
      case x: XNodeLeaf[V] => _recursiveIntentionBuilderLeafNode(x)
      case x: XNodeParent[V] => _recursiveIntentionBuilderParentNode(x)
    }
  }

  def _recursiveIntentionBuilderLeafNode[V](node: XNodeLeaf[V]): IntExpression = {
    node.getType match {
      case TypeExpr.VAR => varHashMap(node.value.toString)
      case TypeExpr.LONG => node.value.asInstanceOf[Long].toInt
    }
  }

  def _recursiveIntentionBuilderParentNode[V](tree: XNodeParent[V]): IntExpression = {
    tree.getType match {
      case TypeExpr.IN => {
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
      }
      case TypeExpr.OR => Or(tree.sons.map(_recursiveIntentionBuilder(_).asInstanceOf[BoolExpression]))
      case TypeExpr.AND => And(tree.sons.map(_recursiveIntentionBuilder(_).asInstanceOf[BoolExpression]))
      case TypeExpr.EQ => _recursiveIntentionBuilder(tree.sons(0)) === _recursiveIntentionBuilder(tree.sons(1))
      case TypeExpr.LT => _recursiveIntentionBuilder(tree.sons(0)) < _recursiveIntentionBuilder(tree.sons(1))
      case TypeExpr.LE => _recursiveIntentionBuilder(tree.sons(0)) <= _recursiveIntentionBuilder(tree.sons(1))
      case TypeExpr.GE => _recursiveIntentionBuilder(tree.sons(0)) >= _recursiveIntentionBuilder(tree.sons(1))
      case TypeExpr.GT => _recursiveIntentionBuilder(tree.sons(0)) > _recursiveIntentionBuilder(tree.sons(1))
      case TypeExpr.NE => _recursiveIntentionBuilder(tree.sons(0)) !== _recursiveIntentionBuilder(tree.sons(1))
      case TypeExpr.ADD => Sum(tree.sons.map(_recursiveIntentionBuilder))
      case TypeExpr.SUB => _recursiveIntentionBuilder(tree.sons(0)) - _recursiveIntentionBuilder(tree.sons(1))
      case TypeExpr.DIV => _recursiveIntentionBuilder(tree.sons(0)) / _recursiveIntentionBuilder(tree.sons(1))
      case TypeExpr.ABS => Abs(_recursiveIntentionBuilder(tree.sons(0)))
      case TypeExpr.NEG => -_recursiveIntentionBuilder(tree.sons(0))
      case TypeExpr.MIN => Min(tree.sons.map(_recursiveIntentionBuilder))
      case TypeExpr.MAX => Max(tree.sons.map(_recursiveIntentionBuilder))
      case TypeExpr.DIST => Abs(_recursiveIntentionBuilder(tree.sons(0)) - _recursiveIntentionBuilder(tree.sons(1)))
      case TypeExpr.NOT => !_recursiveIntentionBuilder(tree.sons(0))
      case TypeExpr.MOD =>
        assert(tree.sons(1).getType == TypeExpr.LONG)
        _recursiveIntentionBuilder(tree.sons(0)) % tree.sons(1).asInstanceOf[XNodeLeaf[V]].value.asInstanceOf[Long].toInt
      case TypeExpr.MUL =>
        //TODO improve for non-binary cases
        val ret = tree.sons.map(_recursiveIntentionBuilder).foldLeft(null: IntExpression)((cur, next) => {
          if(cur == null) next
          else cur * next
        })
        ret
      case TypeExpr.IFF =>
        val csts = tree.sons.map(_recursiveIntentionBuilder).foldLeft((List[BoolExpression](), null: IntExpression))((cur, next) => {
          if(cur._2 == null) (List[BoolExpression](), next)
          else ((cur._2 === next) :: cur._1, next)
        })._1
        if(csts.length == 1)
          csts.last
        else
          And(csts.toArray) //TODO improve this, need for an n-ary eq
      case TypeExpr.SQR => ??? //1
      case TypeExpr.POW => ??? //2
      case TypeExpr.SET => ??? //0, Integer.MAX_VALUE
      case TypeExpr.XOR => ??? //2, Integer.MAX_VALUE
      case TypeExpr.IMP => ??? //2
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
      case TypeExpr.LONG => ??? //0
      case TypeExpr.RATIONAL => ??? //0
      case TypeExpr.DECIMAL => ??? //0
      case TypeExpr.VAR => ??? //0
      case TypeExpr.PAR => ??? //0
      case TypeExpr.SYMBOL => ??? //0
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
    list.map(i => varHashMap(i.id)).sliding(2).map{x => rel(x(1), x(2))}.foreach(modelDeclaration.post)
  }

  override def buildCtrExtension(id: String, x: XVarInteger, values: Array[Int], positive: Boolean, flags: util.Set[TypeFlag]): Unit = {
    if(positive) {
      //InSet constraint
      modelDeclaration.add(InSet(varHashMap(x.id), values.toSet))
    }
    else {
      val a: Array[IntExpression] = Array(varHashMap(x.id))
      val b: Array[Array[Int]] = Array(values)
      val v = varHashMap(x.id)
      values.foreach(x => modelDeclaration.add(v !== x))
    }
  }

  override def buildCtrExtension(id: String, list: Array[XVarInteger], tuples: Array[Array[Int]], positive: Boolean, flags: util.Set[TypeFlag]): Unit = {
    //println(list.map(x => x.id).mkString(" "))
    val cst: Constraint = if(positive) {
      Table(list.map(x => varHashMap(x.id)), tuples)
    }
    else {
      NegativeTable(list.map(x => varHashMap(x.id)), tuples)
    }
    modelDeclaration.add(cst)
  }

  override def buildCtrInstantiation(id: String, list: Array[XVarInteger], values: Array[Int]): Unit = {
    val csts = list.zip(values).map(x => (varHashMap(x._1.id) === x._2).toConstraint)
    csts.foreach(modelDeclaration.add)
  }

  override def buildCtrCircuit(id: String, list: Array[XVarInteger], startIndex: Int): Unit = throw new Exception("Single subcircuit constraint is not implemented")
  override def buildCtrCircuit(id: String, list: Array[XVarInteger], startIndex: Int, size: Int): Unit = throw new Exception("Single subcircuit constraint is not implemented")
  override def buildCtrCircuit(id: String, list: Array[XVarInteger], startIndex: Int, size: XVarInteger): Unit = throw new Exception("Single subcircuit constraint is not implemented")
  override def buildCtrCardinality(id: String, list: Array[XVarInteger], closed: Boolean, values: Array[XVarInteger], occurs: Array[XVarInteger]): Unit = throw new Exception("GCC with var cardinalities is not implemented")
  override def buildCtrCardinality(id: String, list: Array[XVarInteger], closed: Boolean, values: Array[XVarInteger], occurs: Array[Int]): Unit = throw new Exception("GCC with var cardinalities is not implemented")
  override def buildCtrCardinality(id: String, list: Array[XVarInteger], closed: Boolean, values: Array[XVarInteger], occursMin: Array[Int], occursMax: Array[Int]): Unit = throw new Exception("GCC with var cardinalities is not implemented")

  // Objectives
  def _getExprForTypeObjective(objtype: TypeObjective, list: Array[XVarInteger]): IntExpression = {
    objtype match {
      case TypeObjective.MAXIMUM => Max(list.map(i => varHashMap(i.id)))
      case TypeObjective.MINIMUM => Min(list.map(i => varHashMap(i.id)))
      case TypeObjective.SUM => Sum(list.map(i => varHashMap(i.id)))
      case TypeObjective.EXPRESSION => ???
      case TypeObjective.LEX => ???
      case TypeObjective.NVALUES => ???
      case TypeObjective.PRODUCT => ???
    }
  }

  def _getExprForTypeObjective(objtype: TypeObjective, list: Array[XVarInteger], coefs: Array[Int]): IntExpression = {
    objtype match {
      case TypeObjective.MAXIMUM => Max(list.zip(coefs).map(i => varHashMap(i._1.id)*i._2))
      case TypeObjective.MINIMUM => Min(list.zip(coefs).map(i => varHashMap(i._1.id)*i._2))
      case TypeObjective.SUM => WeightedSum(list.map(i => varHashMap(i.id)), coefs)
      case TypeObjective.EXPRESSION => ???
      case TypeObjective.LEX => ???
      case TypeObjective.NVALUES => ???
      case TypeObjective.PRODUCT => ???
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

  override def buildCtrCumulative(id: String, origins: Array[XVarInteger], lengths: Array[Int], heights: Array[Int], condition: Condition): Unit = ???

  override def buildCtrCumulative(id: String, origins: Array[XVarInteger], lengths: Array[Int], heights: Array[XVarInteger], condition: Condition): Unit = ???

  override def buildCtrCumulative(id: String, origins: Array[XVarInteger], lengths: Array[XVarInteger], heights: Array[Int], condition: Condition): Unit = ???

  override def buildCtrCumulative(id: String, origins: Array[XVarInteger], lengths: Array[XVarInteger], heights: Array[XVarInteger], condition: Condition): Unit = ???

  override def buildCtrCumulative(id: String, origins: Array[XVarInteger], lengths: Array[Int], ends: Array[XVarInteger], heights: Array[Int], condition: Condition): Unit = ???

  override def buildCtrCumulative(id: String, origins: Array[XVarInteger], lengths: Array[Int], ends: Array[XVarInteger], heights: Array[XVarInteger], condition: Condition): Unit = ???

  override def buildCtrCumulative(id: String, origins: Array[XVarInteger], lengths: Array[XVarInteger], ends: Array[XVarInteger], heights: Array[Int], condition: Condition): Unit = ???

  override def buildCtrCumulative(id: String, origins: Array[XVarInteger], lengths: Array[XVarInteger], ends: Array[XVarInteger], heights: Array[XVarInteger], condition: Condition): Unit = ???

  override def buildCtrNoOverlap(id: String, origins: Array[XVarInteger], lengths: Array[Int], zeroIgnored: Boolean): Unit = ???

  override def buildCtrNoOverlap(id: String, origins: Array[XVarInteger], lengths: Array[XVarInteger], zeroIgnored: Boolean): Unit = ???

  override def buildCtrNoOverlap(id: String, origins: Array[Array[XVarInteger]], lengths: Array[Array[Int]], zeroIgnored: Boolean): Unit = ???

  override def buildCtrNoOverlap(id: String, origins: Array[Array[XVarInteger]], lengths: Array[Array[XVarInteger]], zeroIgnored: Boolean): Unit = ???

  override def buildCtrCount(id: String, list: Array[XVarInteger], values: Array[Int], condition: Condition): Unit = ???

  override def buildCtrCount(id: String, list: Array[XVarInteger], values: Array[XVarInteger], condition: Condition): Unit = ???

  override def buildCtrElement(id: String, list: Array[XVarInteger], value: XVarInteger): Unit = ???

  override def buildCtrElement(id: String, list: Array[XVarInteger], value: Int): Unit = ???

  override def buildCtrElement(id: String, list: Array[XVarInteger], startIndex: Int, index: XVarInteger, rank: TypeRank, value: XVarInteger): Unit = ???

  override def buildCtrElement(id: String, list: Array[XVarInteger], startIndex: Int, index: XVarInteger, rank: TypeRank, value: Int): Unit = ???

  override def buildCtrAllEqual(id: String, list: Array[XVarInteger]): Unit = ???

  override def buildCtrNotAllEqual(id: String, list: Array[XVarInteger]): Unit = ???

  override def buildCtrAtMost(id: String, list: Array[XVarInteger], value: Int, k: Int): Unit = ???

  override def buildCtrClause(id: String, pos: Array[XVarInteger], neg: Array[XVarInteger]): Unit = ???

  override def buildCtrExactly(id: String, list: Array[XVarInteger], value: Int, k: Int): Unit = ???

  override def buildCtrExactly(id: String, list: Array[XVarInteger], value: Int, k: XVarInteger): Unit = ???

  override def buildCtrChannel(id: String, list: Array[XVarInteger], startIndex: Int): Unit = ???

  override def buildCtrChannel(id: String, list1: Array[XVarInteger], startIndex1: Int, list2: Array[XVarInteger], startIndex2: Int): Unit = ???

  override def buildCtrChannel(id: String, list: Array[XVarInteger], startIndex: Int, value: XVarInteger): Unit = ???

  override def buildCtrRegular(id: String, list: Array[XVarInteger], transitions: Array[Array[AnyRef]], startState: String, finalStates: Array[String]): Unit = ???

  override def buildCtrLex(id: String, lists: Array[Array[XVarInteger]], operator: TypeOperator): Unit = ???

  override def buildCtrMDD(id: String, list: Array[XVarInteger], transitions: Array[Array[AnyRef]]): Unit = ???

  override def buildCtrAllDifferentList(id: String, lists: Array[Array[XVarInteger]]): Unit = ???

  override def buildCtrLexMatrix(id: String, matrix: Array[Array[XVarInteger]], operator: TypeOperator): Unit = ???

  override def buildCtrMinimum(id: String, list: Array[XVarInteger], condition: Condition): Unit = ???

  override def buildCtrMinimum(id: String, list: Array[XVarInteger], startIndex: Int, index: XVarInteger, rank: TypeRank, condition: Condition): Unit = ???

  override def buildCtrAtLeast(id: String, list: Array[XVarInteger], value: Int, k: Int): Unit = ???

  override def buildCtrAllDifferentExcept(id: String, list: Array[XVarInteger], except: Array[Int]): Unit = ???

  override def buildCtrStretch(id: String, list: Array[XVarInteger], values: Array[Int], widthsMin: Array[Int], widthsMax: Array[Int]): Unit = ???

  override def buildCtrStretch(id: String, list: Array[XVarInteger], values: Array[Int], widthsMin: Array[Int], widthsMax: Array[Int], patterns: Array[Array[Int]]): Unit = ???

  override def buildCtrMaximum(id: String, list: Array[XVarInteger], condition: Condition): Unit = ???

  override def buildCtrMaximum(id: String, list: Array[XVarInteger], startIndex: Int, index: XVarInteger, rank: TypeRank, condition: Condition): Unit = ???

  override def buildCtrAmong(id: String, list: Array[XVarInteger], values: Array[Int], k: Int): Unit = ???

  override def buildCtrAmong(id: String, list: Array[XVarInteger], values: Array[Int], k: XVarInteger): Unit = ???

  override def buildCtrExtension(id: String, x: XVarSymbolic, values: Array[String], positive: Boolean, flags: util.Set[TypeFlag]): Unit = ???

  override def buildCtrExtension(id: String, list: Array[XVarSymbolic], tuples: Array[Array[String]], positive: Boolean, flags: util.Set[TypeFlag]): Unit = ???

  override def buildCtrIntension(id: String, scope: Array[XVarSymbolic], syntaxTreeRoot: XNodeParent[XVar]): Unit = ???

  override def buildVarSymbolic(x: XVarSymbolic, values: Array[String]): Unit = ???

  override def buildCtrAllDifferent(id: String, list: Array[XVarSymbolic]): Unit = ???
}

object XCSP3Parser {
  def parse(modelDeclarator: ModelDeclaration, filename: String): Iterable[IntVar] = {
    val parser = new XCSP3Parser(modelDeclarator, filename)
    parser.varHashMap.values.map(i => i.asInstanceOf[IntVar])
  }
}

object Parser2 extends DistributedCPApp[String] with App {
  override lazy val config = new DistributedCPAppConfig {
    val instance = trailArg[String](descr = "Path to the file to parse")
    val check = opt[Boolean]("c", descr = "Check results with the XCSP3 checker")
  }

  val vars = XCSP3Parser.parse(this.modelDeclaration, config.instance.get.get)

  setSearch(Branching.binaryFirstFail(vars.toSeq))
  onSolution {
    val str1 = "<instantiation>\n\t<list>\n\t\t"
    val elems = vars.map(x => x.getRepresentativeName.get).mkString(" ")
    var str2 = "\n\t</list>\n\t<values>\n\t\t"
    val vals = vars.map(x => x.evaluate().toString).mkString(" ")
    val str3 = "\n\t</values>\n</instantiation>"
    str1 + elems + str2 + vals + str3
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
        val vars = XCSP3Parser.parse(this.modelDeclaration, instancePath)

        setSearch(Branching.binaryFirstFail(vars.toSeq))
        onSolution {
          val str1 = "<instantiation>\n\t<list>\n\t\t"
          val elems = vars.map(x => x.getRepresentativeName.get).mkString(" ")
          var str2 = "\n\t</list>\n\t<values>\n\t\t"
          val vals = vars.map(x => x.evaluate().toString).mkString(" ")
          val str3 = "\n\t</values>\n</instantiation>"
          str1 + elems + str2 + vals + str3
        }

        setDecompositionStrategy(new CartProdRefinement(vars, Branching.binaryFirstFail(vars.toSeq)))
        val (stats, solutions) = solve()
        //println(stats)
        println("-->")
        //println(solutions.mkString("\n\n"))
        solutions.foreach(x => testSolution(instancePath, x))
        //println("<!-- solutions verified -->")

        def testSolution(instancePath: String, solution: String): Unit = {
          val sc = new CheckerLib(instancePath, solution)
          if(sc.getInvalidObjs.nonEmpty || sc.getViolatedCtrs.nonEmpty)
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