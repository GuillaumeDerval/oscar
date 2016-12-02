package oscar.modeling.test
import oscar.cp.{CPBoolVar, CPIntVar}
import oscar.cp.core.CPPropagStrength
import oscar.modeling.constraints.ConvertCPConstraint
import oscar.modeling.models.{ModelDeclaration, UninstantiatedModel}
import oscar.modeling.models.operators.CPInstantiate
import oscar.modeling.testUtils.TestSuite
import oscar.modeling.vars.{BoolVar, IntVar}

abstract class MyTestConstraints extends oscar.cp.Constraint(null) {
  def print: String
  override def setup(l: CPPropagStrength): Unit = {}
}

final class StdTypeTest(a: Int, b: Long, c: String, d: Double, e: Boolean, f: Float) extends MyTestConstraints {
  override def print: String = (a,b,c,d,e,f).toString()
}

final class StdCollectionsTest(a: Array[Int], b: Set[Long], c: Seq[Boolean]) extends MyTestConstraints {
  override def print: String = (a.map(_.toString).mkString("-"), b.map(_.toString).mkString("-"), c.map(_.toString).mkString("-")).toString()
}

final class StdCollectionsTest2(a: Array[Array[Int]]) extends MyTestConstraints {
  override def print: String = a.flatten.map(_.toString).mkString("-")
}

final class IntVarTest(a: CPIntVar, b: CPBoolVar) extends MyTestConstraints {
  override def print: String = (a.min, b.min).toString()
}

final class ArrayTest(a: Array[CPIntVar], b: Seq[CPBoolVar]) extends MyTestConstraints {
  override def print: String = (a.map(_.min).mkString("-"), b.map(_.min).mkString("-")).toString()
}


final class ArrayTest2(a: Array[Array[Array[CPIntVar]]]) extends MyTestConstraints {
  override def print: String = a.flatten.flatten.map(_.min.toString).mkString("-")
}

class TestCPConstraintConversion extends TestSuite {
  test("CP Constraint Conversion - std types") {
    val ist = ConvertCPConstraint.getConstraint[StdTypeTest](1, 1L, "1", 1.0d, true, 1.0)
    assert(ist.print == "(1,1,1,1.0,true,1.0)")
  }

  test("CP Constraint Conversion - std types with collections") {
    val ist = ConvertCPConstraint.getConstraint[StdCollectionsTest](Array[Int](1,2), Set(1L, 2L), Seq(true, false))
    assert(ist.print == "(1-2,1-2,true-false)")
  }

  test("CP Constraint Conversion - std types with collections - multiple") {
    val ist = ConvertCPConstraint.getConstraint[StdCollectionsTest2](Array(Array(1,2), Array(3,4)))
    assert(ist.print == "1-2-3-4")
  }

  implicit val modelDecl = new ModelDeclaration()
  val intvars = Array.fill(2)(IntVar(2, 4))
  val mintvars = Array.fill(2,2,2)(IntVar(1, 2))
  val boolvars = Seq.fill(2)(BoolVar())
  val cpModel = CPInstantiate(modelDecl.getCurrentModel.asInstanceOf[UninstantiatedModel])

  test("CP Constraint Conversion - intvar and boolvar") {
    modelDecl.apply(cpModel) {
      val ist = ConvertCPConstraint.getConstraint[IntVarTest](intvars.head, boolvars.head)
      assert(ist.print == "(2,0)")
    }
  }

  test("CP Constraint Conversion - intvar and boolvar in collections") {
    modelDecl.apply(cpModel) {
      val ist = ConvertCPConstraint.getConstraint[ArrayTest](intvars, boolvars)
      assert(ist.print == "(2-2,0-0)")
    }
  }

  test("CP Constraint Conversion - intvar and boolvar in collections - multiple") {
    modelDecl.apply(cpModel) {
      val ist = ConvertCPConstraint.getConstraint[ArrayTest2](mintvars)
      assert(ist.print == "1-1-1-1-1-1-1-1")
    }
  }
}
