import org.xcsp.parser.XConstants
import org.xcsp.parser.XConstraints
import org.xcsp.parser.XConstraints.{CCtr, CEntry}
import org.xcsp.parser.XDomains.{DDom, DDomInteger, DDomSymbolic}
import org.xcsp.parser.XEnums.TypeAtt
import org.xcsp.parser.XEnums.TypeChild
import org.xcsp.parser.XEnums.TypeConditionOperator
import org.xcsp.parser.XEnums.TypeCtr
import org.xcsp.parser.XEnums.TypeExpr
import org.xcsp.parser.XEnums.TypeOperator
import org.xcsp.parser.XNodeExpr
import org.xcsp.parser.XNodeExpr.XNodeLeaf
import org.xcsp.parser.XNodeExpr.XNodeParent
import org.xcsp.parser.XParser
import org.xcsp.parser.XValues.IntegerEntity
import org.xcsp.parser.XValues.IntegerInterval
import org.xcsp.parser.XValues.IntegerValue
import org.xcsp.parser.XValues.SimpleValue
import org.xcsp.parser.XVariables.{VVar, VArray, VEntry}
import oscar.cp._

import scala.collection.JavaConversions._


/**
 * @author Pierre Schaus pschaus@gmail.com
 */
object XCSP3OscaR extends App {

  val parser = new XParser("data/xcsp3/inst1.xml")
  implicit val cp = CPSolver()
  val mapVar: Map[VVar,CPIntVar]  = loadVariables(parser,cp)
  loadConstraints(parser,cp,mapVar)
  cp.search {
    binaryFirstFail(mapVar.values.toArray[CPIntVar])
  }
  cp.onSolution {
    println("solution")
    for ((k,v) <- mapVar) {
      println(k.id+" "+v.value)
    }
  }
  val stat = cp.start()
  println(stat)


  def loadConstraints(parser: XParser,cp: CPSolver, mapVar: Map[VVar,CPIntVar] ): Unit = {
    for (c: CEntry <- parser.cEntries) {
      c match {
        case cons: CCtr => {
          loadConstraint(cons,cp,mapVar)
        }
        case _ => throw new IllegalArgumentException("not yet implemented")
      }
    }
  }

  def loadConstraint(c: CCtr, cp: CPSolver, mapVar: Map[VVar,CPIntVar] ): Unit = {
    println("load constraint"+c.getType())
    val childs = c.childs;
    c.getType() match {
      case TypeCtr.intension => {
        println("intention no implemented")
        val exprNode = childs(0).value.asInstanceOf[XNodeExpr]
      }
      case TypeCtr.extension => {

        val tuples = trIntegerTuples(childs(1).value)
        val variables = c.vars().asInstanceOf[Array[VVar]].map(mapVar(_))

        cp.add(table(variables,tuples))
      }
      case _ => {
        println("not implemented")
      }
    }
  }

  def trIntegerTuples(value: Object): Array[Array[Int]] = {
    if (value != null) {
      value match {
        case array: Array[Array[Short]] => Array.tabulate(array.size,array(0).size) { case (i, j) => array(i)(j).toInt }
        case array: Array[Array[Byte]] => Array.tabulate(array.size,array(0).size) { case (i, j) => array(i)(j).toInt }
        case array: Array[Array[Int]] => array
        case array: Array[Array[Long]] => Array.tabulate(array.size,array(0).size) { case (i, j) => array(i)(j).toInt }
      }
    } else {
      Array.ofDim[Int](0,2)
    }
  }





  def readDomain(d: DDom): Set[Int] = {
    d match {
      case idom: DDomInteger => {
        val set = idom.values.map { _ match {
          case value: IntegerValue => {
            Set(value.v.toInt)
          }
          case interval: IntegerInterval => {
            (interval.inf.toInt to interval.sup.toInt).toSet
          }
        }}.foldLeft(Set[Int]()){case(a,b) => a union b}
        //println(idom.values.map(_.v).mkString("::"))
        //println("dom integer:"+sets)
        //println(idom.values.mkString(","))
        set
      }
      case sdom: DDomSymbolic => {
        throw new IllegalArgumentException("cannot treat symbolic")
      }
      case _ => throw new IllegalArgumentException("unknown domain")
    }

  }

  def loadVariables(parser: XParser,cp: CPSolver): Map[VVar,CPIntVar] = {
    var mapVar = Map[VVar,CPIntVar]()
    for (entry: VEntry  <- parser.vEntries) {
      val id = entry.id
      val note = entry.note
      val classes = entry.classes
      entry match {
        case v: VVar => {
          if (v.degree > 0) {
            val dom = v.dom
            mapVar = mapVar + (v -> CPIntVar(readDomain(dom))(cp))
          }
        }
        case va: VArray => {

          val dim = va.size
          dim.size match {
            case 1 => for {i <- 0 until dim(0)
                           if va.varAt(i).degree > 0} {
              val dom = va.varAt(i).dom
              mapVar = mapVar + (va.varAt(i) -> CPIntVar(readDomain(dom))(cp))
            }
            case 2 => for {i <- 0 until dim(0)
                           j <- 0 until dim(1)
                           if va.varAt(i,j).degree > 0} {
              val dom = va.varAt(i,j).dom
              mapVar = mapVar + (va.varAt(i,j) -> CPIntVar(readDomain(dom))(cp))
            }
            case 3 => for {i <- 0 until dim(0)
                           j <- 0 until dim(1)
                           k <- 0 until dim(2)
                           if va.varAt(i,j,k).degree > 0} {
              val dom = va.varAt(i,j,k).dom
              mapVar = mapVar + (va.varAt(i,j,k) -> CPIntVar(readDomain(dom))(cp))
            }
            case 4 => for {i <- 0 until dim(0)
                           j <- 0 until dim(1)
                           k <- 0 until dim(2)
                           l <- 0 until dim(3)
                           if va.varAt(i,j,k,l).degree > 0} {
              val dom = va.varAt(i,j,k,l).dom
              mapVar = mapVar + (va.varAt(i,j,k,l) -> CPIntVar(readDomain(dom))(cp))
            }
            case 5 => for {i <- 0 until dim(0)
                           j <- 0 until dim(1)
                           k <- 0 until dim(2)
                           l <- 0 until dim(3)
                           m <- 0 until dim(4)
                           if va.varAt(i,j,k,l,m).degree > 0} {
              val dom = va.varAt(i,j,k,l,m).dom
              mapVar = mapVar + (va.varAt(i,j,k,l,m) -> CPIntVar(readDomain(dom))(cp))
            }
          }
        }
        case _ => throw new IllegalArgumentException("unknown variable type")
      }
    }
    mapVar



  }

}

