package oscar.xcsp3

import java.io.{ByteArrayInputStream, File}
import java.util

import org.xcsp.checker.SolutionChecker
import org.xcsp.common.XEnums._
import org.xcsp.common.predicates.{XNodeExpr, XNodeLeaf, XNodeParent}
import org.xcsp.parser.XCallbacks.XCallbacksParameters
import org.xcsp.parser.XConstraints.{CEntry, XBlock, XGroup, XSlide}
import org.xcsp.parser.XObjectives.OEntry
import org.xcsp.parser.XParser.{Condition, ConditionIntvl, ConditionVal, ConditionVar}
import org.xcsp.parser.XVariables._
import org.xcsp.parser._
import oscar.cp.CPIntVar
import oscar.cp._
import oscar.cp.constraints.EqCons
import oscar.cp.core.{CPSolver, Constraint}

import scala.collection.mutable.ArrayBuffer

class XCSP3Parser(filename: String) extends XCallbacksDecomp {

  // Force parameter update
  XCallbacks.callbacksParameters.clear()
  XCallbacks.callbacksParameters.put(XCallbacksParameters.RECOGNIZE_SPECIAL_UNARY_INTENSION_CASES, new Object)
  XCallbacks.callbacksParameters.put(XCallbacksParameters.RECOGNIZE_SPECIAL_BINARY_INTENSION_CASES,  new Object)
  XCallbacks.callbacksParameters.put(XCallbacksParameters.RECOGNIZE_SPECIAL_TERNARY_INTENSION_CASES,  new Object)
  XCallbacks.callbacksParameters.put(XCallbacksParameters.RECOGNIZE_SPECIAL_COUNT_CASES,  new Object)
  XCallbacks.callbacksParameters.put(XCallbacksParameters.RECOGNIZE_SPECIAL_NVALUES_CASES,  new Object)
  XCallbacks.callbacksParameters.put(XCallbacksParameters.INTENSION_TO_EXTENSION_ARITY_LIMIT, 0:java.lang.Integer) // included
  XCallbacks.callbacksParameters.put(XCallbacksParameters.INTENSION_TO_EXTENSION_SPACE_LIMIT, 1000000:java.lang.Integer)
  XCallbacks.callbacksParameters.put(XCallbacksParameters.INTENSION_TO_EXTENSION_PRIORITY, false:java.lang.Boolean)
  XCallbacks.callbacksParameters.remove(XCallbacksParameters.RECOGNIZE_SPECIAL_COUNT_CASES)
  //XCallbacks.callbacksParameters.add(XCallbacksParameters.)


  implicit val cp = CPSolver()

  val varHashMap = collection.mutable.LinkedHashMap[String, CPIntVar]() //automagically maintains the order of insertion
  val cstHashMap = collection.mutable.HashMap[String, Constraint]()

  loadInstance(filename)

  // For now, structure don't really matters...
  override def beginInstance(`type`: TypeFramework): Unit = {}
  override def endInstance(): Unit = {}
  override def beginVariables(vEntries: util.List[VEntry]): Unit = {}
  override def endVariables(): Unit = {}
  override def beginArray(a: XArray): Unit = {}
  override def endArray(a: XArray): Unit = {}
  override def beginConstraints(cEntries: util.List[CEntry]): Unit = {}
  override def endConstraints(): Unit = {}
  override def beginGroup(g: XGroup): Unit = {}
  override def endGroup(g: XGroup): Unit = {}
  override def beginObjectives(oEntries: util.List[OEntry], `type`: TypeCombination): Unit = {}
  override def endObjectives(): Unit = {}
  override def beginBlock(b: XBlock): Unit = {}
  override def endBlock(b: XBlock): Unit = {}
  override def beginSlide(s: XSlide): Unit = {}
  override def endSlide(s: XSlide): Unit = {}

  // Variables
  override def buildVarInteger(x: XVarInteger, minValue: Int, maxValue: Int): Unit = {
    //println("Adding var "+x.id)
    varHashMap += (x.id -> CPIntVar(minValue,maxValue,x.id)(cp)) // Some(x.id)
  }

  override def buildVarInteger(x: XVarInteger, values: Array[Int]): Unit = {
    varHashMap += (x.id -> CPIntVar(values,x.id)(cp))
  }

  // Constraints
  override def buildCtrAllDifferent(id: String, list: Array[XVarInteger]): Unit = {
    //println("Adding AllDifferent "+id)
    val cons = allDifferent(list.map(elem => varHashMap(elem.id)))
    cp.add(cons)
    cstHashMap += (id->cons)
  }

  override def buildCtrPrimitive(id: String, xvi: XVarInteger, opa: TypeArithmeticOperator, yvi: XVarInteger, op: TypeConditionOperatorRel, k: Int): Unit = {
    //println(id)
    val x = varHashMap(xvi.id)
    val y = varHashMap(yvi.id)
    val r: CPIntVar = opa match {
      case TypeArithmeticOperator.ADD => x + y
      case TypeArithmeticOperator.DIST => (x-y).abs
      case TypeArithmeticOperator.DIV => throw new Exception("Division between vars is not implemented")
      case TypeArithmeticOperator.MUL => x * y
      case TypeArithmeticOperator.SUB => x - y
      case TypeArithmeticOperator.MOD => throw new Exception("Modulo between vars is not implemented")
    }
    val r2: Constraint = (op match {
      case TypeConditionOperatorRel.EQ => r === k
      case TypeConditionOperatorRel.GE => r >= k
      case TypeConditionOperatorRel.GT => r > k
      case TypeConditionOperatorRel.LE => r <= k
      case TypeConditionOperatorRel.LT => r < k
      case TypeConditionOperatorRel.NE => r !== k
    })
    cp.add(r2)
    cstHashMap += ((id, r2))
  }

  override def buildCtrPrimitive(id: String, xvi: XVarInteger, op: TypeConditionOperatorRel, k: Int): Unit = {
    val r = varHashMap(xvi.id)
    val r2: Constraint = (op match {
      case TypeConditionOperatorRel.EQ => r === k
      case TypeConditionOperatorRel.GE => r >= k
      case TypeConditionOperatorRel.GT => r > k
      case TypeConditionOperatorRel.LE => r <= k
      case TypeConditionOperatorRel.LT => r < k
      case TypeConditionOperatorRel.NE => r !== k
    })
    cp.add(r2)
    cstHashMap += ((id, r2))
  }

  override def buildCtrPrimitive(id: String, xvi: XVarInteger, opa: TypeArithmeticOperator, yvi: XVarInteger, op: TypeConditionOperatorRel, zvi: XVarInteger): Unit = {
    val x = varHashMap(xvi.id)
    val y = varHashMap(yvi.id)
    val z = varHashMap(zvi.id)
    val r: CPIntVar = opa match {
      case TypeArithmeticOperator.ADD => x + y
      case TypeArithmeticOperator.DIST => (x-y).abs
      case TypeArithmeticOperator.DIV => throw new Exception("Division between vars is not implemented")
      case TypeArithmeticOperator.MUL => x * y
      case TypeArithmeticOperator.SUB => x - y
      case TypeArithmeticOperator.MOD => throw new Exception("Modulo between vars is not implemented")
    }
    val r2: Constraint = (op match {
      case TypeConditionOperatorRel.EQ => r === z
      case TypeConditionOperatorRel.GE => r >= z
      case TypeConditionOperatorRel.GT => r > z
      case TypeConditionOperatorRel.LE => r <= z
      case TypeConditionOperatorRel.LT => r < z
      case TypeConditionOperatorRel.NE => r !== z
    })
    cp.add(r2)
    cstHashMap += (id->r2)
  }

  def _buildCrtWithCondition(id: String, expr: CPIntVar, operator: Condition): Unit = {
    val cons: Constraint = operator match {
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
    cp.add(cons)
    cstHashMap += (id->cons)
  }
  override def buildCtrSum(id: String, list: Array[XVarInteger], condition: Condition): Unit = {
    _buildCrtWithCondition(id, sum(list.map(i => varHashMap(i.id))), condition)
  }

  override def buildCtrSum(id: String, list: Array[XVarInteger], coeffs: Array[Int], condition: Condition): Unit = {
    _buildCrtWithCondition(id, weightedSum(coeffs,list.map(i => varHashMap(i.id))) , condition)
  }

  override def buildCtrSum(id: String, list: Array[XVarInteger], coeffs: Array[XVarInteger], condition: Condition): Unit = {
    _buildCrtWithCondition(id, sum(list.zip(coeffs).map(i => varHashMap(i._1.id)*varHashMap(i._2.id))), condition)
  }


  override def buildCtrCardinality(id: String, list: Array[XVarInteger], closed: Boolean, values: Array[Int], occurs: Array[Int]): Unit = {
    buildCtrCardinality(id,list,closed,values,occurs,occurs)
  }

  override def buildCtrCardinality(id: String, list: Array[XVarInteger], closed: Boolean, values: Array[Int], occursMin: Array[Int], occursMax: Array[Int]): Unit = {
    val minValue = values.min
    val maxValue = values.max
    val cardMin = Array.fill(maxValue-minValue+1)(0)
    val cardMax = Array.fill(maxValue-minValue+1)(0)
    for (i <- 0 until values.size) {
      cardMin(values(i)-minValue) = occursMin(i)
      cardMax(values(i)-minValue) = occursMax(i)
    }
    val cst: Constraint = gcc(list.map(i => varHashMap(i.id)), minValue, cardMin, cardMax)
    cp.add(cst)
    cstHashMap += (id -> cst)
  }

  override def buildCtrCardinality(id: String, list: Array[XVarInteger], closed: Boolean, values: Array[Int], occurs: Array[XVarInteger]): Unit = {
    ???
  }

  override def buildCtrOrdered(id: String, list: Array[XVarInteger], operator: TypeOperator): Unit = {
    val rel: (CPIntVar, CPIntVar) => Constraint = operator match {
      case TypeOperator.GE => (a, b) => a >= b
      case TypeOperator.GT => (a, b) => a > b
      case TypeOperator.LE => (a, b) => a <= b
      case TypeOperator.LT => (a, b) => a < b
      case TypeOperator.SUBSEQ => ???
      case TypeOperator.SUBSET => ???
      case TypeOperator.SUPSEQ => ???
      case TypeOperator.SUPSET => ???
    }
    val csts = list.map(i => varHashMap(i.id)).sliding(2).map{x => rel(x(0),x(1))}

    cp.add(csts.toArray)
  }

  override def buildCtrExtension(id: String, x: XVarInteger, values: Array[Int], positive: Boolean, flags: util.Set[TypeFlag]): Unit = {
    if (positive) {
      cp.add(varHashMap(x.id).in(values.toSet))
    }
    else {
      cp.add(values.map(varHashMap(x.id) !== _))
    }
  }

  override def buildCtrExtension(id: String, list: Array[XVarInteger], tuples: Array[Array[Int]], positive: Boolean, flags: util.Set[TypeFlag]): Unit = {
    //println(list.map(x => x.id).mkString(" "))
    val cst: Constraint = if (positive) {
      table(list.map(x => varHashMap(x.id)), tuples)
    }
    else {
     negativeTable(list.map(x => varHashMap(x.id)), tuples)
    }
    cp.add(cst)
    cstHashMap += ((id, cst))
  }

  override def buildCtrInstantiation(id: String, list: Array[XVarInteger], values: Array[Int]): Unit = {
    val csts = list.zip(values).map{case(x,v) => varHashMap(x.id) === v}
    cp.add(csts)
  }

  override def buildCtrCircuit(id: String, list: Array[XVarInteger], startIndex: Int): Unit = throw new Exception("Single subcircuit constraint is not implemented")
  override def buildCtrCircuit(id: String, list: Array[XVarInteger], startIndex: Int, size: Int): Unit = throw new Exception("Single subcircuit constraint is not implemented")
  override def buildCtrCircuit(id: String, list: Array[XVarInteger], startIndex: Int, size: XVarInteger): Unit = throw new Exception("Single subcircuit constraint is not implemented")
  override def buildCtrCardinality(id: String, list: Array[XVarInteger], closed: Boolean, values: Array[XVarInteger], occurs: Array[XVarInteger]): Unit = throw new Exception("GCC with var cardinalities is not implemented")
  override def buildCtrCardinality(id: String, list: Array[XVarInteger], closed: Boolean, values: Array[XVarInteger], occurs: Array[Int]): Unit = throw new Exception("GCC with var cardinalities is not implemented")
  override def buildCtrCardinality(id: String, list: Array[XVarInteger], closed: Boolean, values: Array[XVarInteger], occursMin: Array[Int], occursMax: Array[Int]): Unit = throw new Exception("GCC with var cardinalities is not implemented")

  // Objectives
  def _getExprForTypeObjective(objtype: TypeObjective, list: Array[XVarInteger]): CPIntVar = {
    objtype match {
      case TypeObjective.MAXIMUM => maximum(list.map(i => varHashMap(i.id)))
      case TypeObjective.MINIMUM => minimum(list.map(i => varHashMap(i.id)))
      case TypeObjective.SUM => sum(list.map(i => varHashMap(i.id)))
      case TypeObjective.EXPRESSION => ???
      case TypeObjective.LEX => ???
      case TypeObjective.NVALUES => ???
      case TypeObjective.PRODUCT => ???
    }
  }

  def _getExprForTypeObjective(objtype: TypeObjective, list: Array[XVarInteger], coefs: Array[Int]): CPIntVar = {
    objtype match {
      case TypeObjective.MAXIMUM => maximum(list.zip(coefs).map(i => varHashMap(i._1.id)*i._2))
      case TypeObjective.MINIMUM => minimum(list.zip(coefs).map(i => varHashMap(i._1.id)*i._2))
      case TypeObjective.SUM => weightedSum(coefs,list.map(i => varHashMap(i.id)))
      case TypeObjective.EXPRESSION => ???
      case TypeObjective.LEX => ???
      case TypeObjective.NVALUES => ???
      case TypeObjective.PRODUCT => ???
    }
  }

  override def buildObjToMinimize(id: String, objtype: TypeObjective, list: Array[XVarInteger]): Unit = {
    cp.minimize(_getExprForTypeObjective(objtype, list))
  }

  override def buildObjToMaximize(id: String, objtype: TypeObjective, list: Array[XVarInteger]): Unit = {
    cp.maximize(_getExprForTypeObjective(objtype, list))
  }

  override def buildObjToMinimize(id: String, x: XVarInteger): Unit = {
    cp.minimize(varHashMap(x.id))
  }

  override def buildObjToMaximize(id: String, x: XVarInteger): Unit = {
    cp.maximize(varHashMap(x.id))
  }

  override def buildObjToMinimize(id: String, objtype: TypeObjective, list: Array[XVarInteger], coeffs: Array[Int]): Unit = {
    cp.maximize(_getExprForTypeObjective(objtype, list, coeffs))
  }

  override def buildObjToMaximize(id: String, objtype: TypeObjective, list: Array[XVarInteger], coeffs: Array[Int]): Unit = {
    cp.maximize(_getExprForTypeObjective(objtype, list, coeffs))
  }

  /*
  override def buildObjToMinimize(id: String, syntaxTreeRoot: XNodeParent[XVar]): Unit = {
    ???
    //cp.minimize(_recursiveIntentionBuilder(syntaxTreeRoot).reify())
  }

  override def buildObjToMaximize(id: String, syntaxTreeRoot: XNodeParent[XVar]): Unit = {
    ???
    //modelDeclaration.maximize(_recursiveIntentionBuilder(syntaxTreeRoot).reify())
  }

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

*/
  override def buildCtrCount(id: String, list: Array[XVarInteger], values: Array[Int], condition: Condition): Unit = {
    val setOfVal = values.toSet
    val counterVar: CPIntVar = sum(list.map(x => varHashMap(x.id)).map(_.isIn(setOfVal).asInstanceOf[CPIntVar]))
    condition match {
      case condition if (condition.isInstanceOf[ConditionIntvl]) => {
        val min = condition.asInstanceOf[ConditionIntvl].min
        val max = condition.asInstanceOf[ConditionIntvl].max
        condition.operator match {
          case TypeConditionOperator.IN => {
            cp.add(counterVar <= max)
            cp.add(counterVar >= min)
          }
          case TypeConditionOperator.NOTIN => {
            for (v <- min to max) {
              cp.add(counterVar !== v)
            }
          }
        }
      }
      case condition if (condition.isInstanceOf[ConditionVal]) => {
        val c = condition.asInstanceOf[ConditionVal].k
        condition.operator match {
          case TypeConditionOperator.LE => cp.add(counterVar <= c)
          case TypeConditionOperator.GE => cp.add(counterVar >= c)
          case TypeConditionOperator.GT => cp.add(counterVar > c)
          case TypeConditionOperator.LT => cp.add(counterVar < c)
          case TypeConditionOperator.EQ => cp.add(counterVar === c)
          case _ => throw new RuntimeException("not supported operator")
        }
      }
      case condition if (condition.isInstanceOf[ConditionVar]) => {
        val c = varHashMap(condition.asInstanceOf[ConditionVar].x.id)
        condition.operator match {
          case TypeConditionOperator.LE => cp.add(counterVar <= c)
          case TypeConditionOperator.GE => cp.add(counterVar >= c)
          case TypeConditionOperator.GT => cp.add(counterVar > c)
          case TypeConditionOperator.LT => cp.add(counterVar < c)
          case TypeConditionOperator.EQ => cp.add(counterVar === c)
          case _ => throw new RuntimeException("not supported operator")
        }
      }
    }
  }

/*

  override def buildCtrCount(id: String, list: Array[XVarInteger], values: Array[XVarInteger], condition: Condition): Unit = ???

  override def buildCtrElement(id: String, list: Array[XVarInteger], value: XVarInteger): Unit = ???

  override def buildCtrElement(id: String, list: Array[XVarInteger], value: Int): Unit = ???

  override def buildCtrElement(id: String, list: Array[XVarInteger], startIndex: Int, index: XVarInteger, rank: TypeRank, value: XVarInteger): Unit = ???

  override def buildCtrElement(id: String, list: Array[XVarInteger], startIndex: Int, index: XVarInteger, rank: TypeRank, value: Int): Unit = ???

  override def buildCtrAllEqual(id: String, list: Array[XVarInteger]): Unit = ???

  override def buildCtrNotAllEqual(id: String, list: Array[XVarInteger]): Unit = ???

  override def buildCtrAtMost(id: String, list: Array[XVarInteger], value: Int, k: Int): Unit = ???

  override def buildCtrAllDifferentMatrix(id: String, matrix: Array[Array[XVarInteger]]): Unit = ???

  override def buildCtrClause(id: String, pos: Array[XVarInteger], neg: Array[XVarInteger]): Unit = ???

  override def buildCtrExactly(id: String, list: Array[XVarInteger], value: Int, k: Int): Unit = ???

  override def buildCtrExactly(id: String, list: Array[XVarInteger], value: Int, k: XVarInteger): Unit = ???

  override def buildCtrChannel(id: String, list: Array[XVarInteger], startIndex: Int): Unit = ???

  override def buildCtrChannel(id: String, list1: Array[XVarInteger], startIndex1: Int, list2: Array[XVarInteger], startIndex2: Int): Unit = ???

  override def buildCtrChannel(id: String, list: Array[XVarInteger], startIndex: Int, value: XVarInteger): Unit = ???

  override def buildCtrIntension(id: String, scope: Array[XVarSymbolic], syntaxTreeRoot: XNodeParent[XVar]): Unit = ???

  override def buildCtrRegular(id: String, list: Array[XVarInteger], transitions: Array[Array[AnyRef]], startState: String, finalStates: Array[String]): Unit = ???

*/

  override def buildCtrLex(id: String, lists: Array[Array[XVarInteger]], operator: TypeOperator): Unit = {
    operator match {
      case TypeOperator.GE => {
        for (i <- 0 until lists.size-1) {
          cp.add(lexLeq(lists(i+1).map(x => varHashMap(x.id)),lists(i).map(x =>varHashMap(x.id))))
        }
      }
      case TypeOperator.LE => {
        for (i <- 0 until lists.size-1) {
          cp.add(lexLeq(lists(i).map(x => varHashMap(x.id)),lists(i+1).map(x =>varHashMap(x.id))))
        }
      }
      case TypeOperator.LT =>
      case TypeOperator.GT =>
      case _ => ???
    }

  }

  /*
  override def buildCtrExtension(id: String, x: XVarSymbolic, values: Array[String], positive: Boolean, flags: util.Set[TypeFlag]): Unit = ???

  override def buildCtrExtension(id: String, list: Array[XVarSymbolic], tuples: Array[Array[String]], positive: Boolean, flags: util.Set[TypeFlag]): Unit = ???

  override def buildVarSymbolic(x: XVarSymbolic, values: Array[String]): Unit = ???
  */

  override def buildCtrMDD(id: String, list: Array[XVarInteger], transitions: Array[Array[AnyRef]]): Unit = {

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


  override def buildCtrAllDifferentList(id: String, lists: Array[Array[XVarInteger]]): Unit = ???

  override def buildCtrLexMatrix(id: String, matrix: Array[Array[XVarInteger]], operator: TypeOperator): Unit = ???

  override def buildCtrMinimum(id: String, list: Array[XVarInteger], condition: Condition): Unit = ???

  override def buildCtrMinimum(id: String, list: Array[XVarInteger], startIndex: Int, index: XVarInteger, rank: TypeRank, condition: Condition): Unit = ???

  override def buildCtrAtLeast(id: String, list: Array[XVarInteger], value: Int, k: Int): Unit = ???

  override def buildCtrAllDifferentExcept(id: String, list: Array[XVarInteger], except: Array[Int]): Unit = ???

  override def buildCtrAllDifferent(id: String, list: Array[XVarSymbolic]): Unit = ???

  override def buildCtrStretch(id: String, list: Array[XVarInteger], values: Array[Int], widthsMin: Array[Int], widthsMax: Array[Int]): Unit = ???

  override def buildCtrStretch(id: String, list: Array[XVarInteger], values: Array[Int], widthsMin: Array[Int], widthsMax: Array[Int], patterns: Array[Array[Int]]): Unit = ???

  override def buildCtrMaximum(id: String, list: Array[XVarInteger], condition: Condition): Unit = ???

  override def buildCtrMaximum(id: String, list: Array[XVarInteger], startIndex: Int, index: XVarInteger, rank: TypeRank, condition: Condition): Unit = ???

  override def buildCtrAmong(id: String, list: Array[XVarInteger], values: Array[Int], k: Int): Unit = ???

  override def buildCtrAmong(id: String, list: Array[XVarInteger], values: Array[Int], k: XVarInteger): Unit = ???
}

object XCSP3Parser {
  def apply(filename: String): XCSP3Parser = new XCSP3Parser(filename)
}

object RunXCSP3 extends App {

  val instance = "data/xcsp3/instancesTest/Blackhole-04-3-00.xml"
  val parser = XCSP3Parser(instance)

  val vars = parser.varHashMap.values.toArray
  parser.cp.search {
    conflictOrderingSearch(vars,i => vars(i).size, i => vars(i).min)
    //binaryFirstFail(vars)
  }

  var solutions = ArrayBuffer[String]()
  parser.cp.onSolution {
    println("solution")
    val str1 = "<instantiation>\n\t<list>\n\t\t"
    val elems = vars.map(x => x.name).mkString(" ")
    var str2 = "\n\t</list>\n\t<values>\n\t\t"
    val vals = vars.map(x => x.value.toString).mkString(" ")
    val str3 = "\n\t</values>\n</instantiation>"
    solutions += str1 + elems + str2 + vals + str3
  }

  val stat = parser.cp.start(1)
  println(stat)


  println(solutions.mkString("\n\n"))


  def testSolution(instancePath: String, solution: String): Unit = {
    if(!new CheckerLib(instancePath, solution).valid)
      throw new XCSP3ValidationException()
  }
  solutions.foreach(sol => testSolution(instance, sol))
  println("<!-- solutions verified -->")


}
