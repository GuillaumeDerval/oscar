package oscar.cp.test

import oscar.cp.core.Constraint
import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import oscar.cp._
import oscar.cp.constraints.tables.{TableDecomp,TableSTR2,TableData,TableAC5TCRecomp}

abstract class TestTableConstraint(val tableConstraintName: String, val nTests: Int = 100) extends FunSuite with ShouldMatchers {

  type Sol = List[Int]

  case class TableInstance(val table: Array[Array[Int]], val domainSize : Int) {
    override def toString: String = {
      val tablePrint = "table : \n " + table.map(t => t.mkString(", ")).mkString("\n")  
      "domainSize : " + domainSize + "\n" + tablePrint + "\n" 
    }
  }

  class CPTable(instance: TableInstance) extends CPSolver {
    implicit val solver = this
    silent = true
    val nVars = instance.table(0).size
    val Vars = 0 until nVars
    
    val varCP = Array.tabulate(nVars)(t => CPIntVar(0 until instance.domainSize))
    val table = instance.table
  }

  def table(X: Array[CPIntVar], table: Array[Array[Int]]): Constraint

  def generateRandomTableProblem(maxArity: Int): TableInstance = {
    val rand = new scala.util.Random()
    val arity = 2 + rand.nextInt(maxArity - 2)
    val nTuples = 1 + rand.nextInt(500)
    val nValues = 2 + rand.nextInt(7)
    val table = Array.tabulate(nTuples)(_ => Array.tabulate(arity)(_ => rand.nextInt(nValues)).toVector).distinct map (_ toArray)
    TableInstance(table,nValues)
  }

  def solveAll(cp: CPTable, decomp: Boolean): Set[Sol] = {
    if (!decomp) cp.post(table(cp.varCP,cp.table))
    else cp.post(new TableDecomp(cp.varCP,cp.table))
    var sols: List[Sol] = List()

    cp.search {
      binaryStatic(cp.varCP )
    }

    cp.onSolution {
      val sol = cp.varCP.map(_.value).toList
      sols = sol :: sols
    }

    cp.start()

    sols.toSet
  }

  def compare(sols1: Set[Sol], sols2: Set[Sol]): Boolean = {
    if (sols1.size != sols2.size) false
    else sols1.forall(s => sols2 contains s)
  }

  test("test solveAll " + tableConstraintName) {
    (1 to nTests).forall(i => {
      //print("test " + cumulativeName + " instance " + i + ": ")
      val instance = generateRandomTableProblem(5)
      val cpDecomp = new CPTable(instance)
      val cpTable = new CPTable(instance)
      val allSolsDecomp = solveAll(cpDecomp, true)
      val allSolsCumul = solveAll(cpTable, false)
      if (compare(allSolsDecomp, allSolsCumul)) {
        //println("success " + allSolsDecomp.size + " " + allSolsCumul.size)
        true
      } else {
        print("test " + tableConstraintName + " instance " + i + ": ")
        println("failed !")
        println("expected number of solutions: " + allSolsDecomp.size)
        println("number of solutions: " + allSolsCumul.size)
        println("INSTANCE")
        println(instance)
        
        false
      }
    }) should be(true)
  }
}



class TestSTR2AgainstDecomp extends TestTableConstraint("TableSTR2") {
  override def table(X: Array[CPIntVar], table: Array[Array[Int]]) : Constraint = {
    new TableSTR2(X,table)
  }
}

class TestAC5TCRecompAgainstDecomp extends TestTableConstraint("TestAC5TCRecompAgainstDecomp") {
  override def table(X: Array[CPIntVar], table: Array[Array[Int]]) : Constraint = {
    val data = new TableData(X.size)
    table.foreach(t => data.add(t: _*))
    new TableAC5TCRecomp(data, X: _*)
  }
}

