package oscar.anytime.lns.models

import oscar.anytime.lns.Benchmark
import oscar.anytime.lns.utils.IOUtils
import oscar.cp.{CPIntVar, CPSolver}
import oscar.modeling.models._
import oscar.modeling.models.cp.CPModel
import oscar.modeling.models.operators.CPInstantiate
import oscar.xcsp3.XCSP3Parser2

class XCSP(val instance: String) extends Benchmark{
  val md = new ModelDeclaration

  //Parsing the instance and instantiating model declaration
  val (vars, solutionGenerator) = XCSP3Parser2.parse(md, instance)

  val model: CPModel = CPInstantiate(md.getCurrentModel.asInstanceOf[UninstantiatedModel])

  override def decisionVariables: Array[CPIntVar] = vars.map(model.getRepresentative(_).realCPVar)

  override def solver: CPSolver = model.cpSolver

  override def bestKnownObjective: Int = Int.MaxValue

  override def problem: String = IOUtils.getParentName(instance)
}
