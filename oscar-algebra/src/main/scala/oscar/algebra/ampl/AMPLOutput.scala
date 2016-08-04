//
//package oscar.algebra.ampl
//
//import java.io.PrintWriter
//
//import com.sun.org.apache.xpath.internal.operations.VariableSafeAbsRef
//import oscar.algebra._
//
//import scala.collection.mutable
//import scala.sys.process._
//
//object AMPLOutput {
//  val defaultIndices = List("i", "j", "k", "m", "n", "p", "q", "r", "s")
//}
//
//class AMPLOutput[O <: AnyType, C <: AnyType](implicit val model: Model[O, C,Double]) {
//
//
//  val modStream = new PrintWriter("test.mod")
//  val constraintsStream = new PrintWriter("constraints.mod")
//
//  val dataStream = new PrintWriter("test.dat")
//
//
//def stringOfConstant(v: Any) = {
//  if ( v.isInstanceOf[Int]) v.toString
//  else if ( v.isInstanceOf[Double]) v.toString
//  else s""""$v""""
//}
//
//  def stringOfVar(v: Var[Double]): String = {
//    v match{
//      case vr: Var1Ref[_,Double] if vr.v.range.values.head.isInstanceOf[Int] => v.toString
//      case vr: Var1Ref[_,Double] => s"""${vr.v.name}["${vr.index}"]"""
//      case vr: Var2Ref[_,_,Double] =>
//        val iA = if (vr.v.rangeA.values.head.isInstanceOf[Int]) vr.indexA.toString else s""""${vr.indexA}""""
//        val iB = if (vr.v.rangeB.values.head.isInstanceOf[Int]) vr.indexB.toString else s"""" ${vr.indexB}""""
//        s"""${vr.v.name}[$iA,$iB]"""
//      case _ => v.toString
//    }
//  }
//
//  def stringOf[T <: AnyType](term: Term[T,Double]): String = {
//    term match{
//      case v: Var[Double] => stringOfVar(v)
//      case s: SumLoop[_,T,Double] => s"""sum {${s.aLoop.iterator(AMPLOutput.defaultIndices)}}(${s.aLoop.body(AMPLOutput.defaultIndices)})"""
//      case p: Param1Ref[_,Double] => s"${p.param.name}[${stringOfConstant(p.index)}]"
//    }
//  }
//
//  def stringOf[T <: AnyType](p: Prod[T,Double], first: Boolean): String = {
//    if (p.coef.d == 0) ""
//    else if (p.coef.d == -1) " - " + p.vars.map(stringOf(_)).mkString("*")
//    else if (p.coef.d < 0) {
//      if ( p.vars.nonEmpty) p.coef.d + "*" + p.vars.map(stringOf(_)).mkString("*")
//      else p.coef.d.toString
//    }
//    else {
//      val plus = if (!first) " + " else ""
//      plus + {
//        if (p.vars.isEmpty) p.coef.d
//        else if (p.coef.d == 1) p.vars.map(stringOf(_)).mkString("*")
//        else p.coef.d + "*" + p.vars.map(stringOf(_)).mkString("*")
//      }
//    }
//  }
//
//  def stringOf[T <: AnyType](e: Expression[T,Double]): String = {
//    {
//      e.terms match {
//        case head #:: tail => stringOf(head, true) :: tail.map(stringOf(_, false)).toList
//      }
//    }.mkString
//  }
//
//  def stringOf[T <: AnyType](e: Equation[T,Double]): String = {
//    stringOf(e.expr) + " " + e.sense + " 0"
//  }
//
//  def write {
//
//
//    // Write variables
//    constraintsStream.println(
//      s"""
//         |##############
//         |#  Variables #
//         |##############
//       """.stripMargin)
//    for ((vd, id) <- model.variables) {
//      val bounds = vd.lowerBound.map {lb => s">= $lb"}.toList ++ vd.upperBound.map {ub => s"<= $ub"}.toList mkString (", ")
//      constraintsStream.println(
//        vd match {
//          case Var0(n, _, _) => s"var ${vd.name} $bounds;"
//          case Var1(indices, name, _, _) => s"var ${name} {${indices}} $bounds;"
//          case Var2(indicesA,indicesB, name,_,_) => s"var ${name} {${indicesA}, ${indicesB}} $bounds;"
//        }
//      )
//    }
//
//    //Write Constraints
//    constraintsStream.println(
//      s"""
//         |################
//         |#  Constraints #
//         |################
//       """.stripMargin)
//    for (system <- model.constraints) {
//      system match {
//        case loops: LoopSystem[C,Double] =>
//          for (loop <- loops.loops) {
//            constraintsStream.println(
//              s"""subject to ${loop.name} {${loop.iterator(AMPLOutput.defaultIndices)}}:
//                  |    ${stringOf(loop.body(AMPLOutput.defaultIndices))};""".stripMargin
//            )
//          }
//        case eqs: StreamSystem[C,Double] =>
//          for (eq <- eqs.equations) {
//            constraintsStream.println(
//              s"subject to ${
//                eq.name
//              }: ${stringOf(eq)};"
//            )
//          }
//      }
//
//    }
//
//
//    // Write Indices
//    modStream.println(
//      s"""
//         |#########
//         |#  Sets #
//         |#########
//       """.stripMargin)
//    for (ind <- model.indices) {
//      val dat = ind.values match {
//        case range: Range =>
//          if (range.step == 1) s"${ range.start} .. ${ range.end}"
//          else s"${ range.start} .. ${ range.end} by ${ range.step}"
//        case seq => seq.mkString("{\"", "\",\"","\"}")
//      }
//
//      modStream.println(s"set ${ ind.name};")
//      dataStream.println(s"let ${ ind.name} := $dat;")
//    }
//
//
//    modStream.println(
//      s"""
//         |###############
//         |#  Parameters #
//         |###############
//       """.stripMargin)
//    for (ind <- model.params) {
//
//        ind match {
//          //case Var0(n, _, _) => s"var ${vd.name} $bounds;"
//          case Param1(name, indices, values, default) =>
//            modStream.println(s"param ${name} {${indices}} ${default.toList.map(v => s"default $v").mkString};")
//            for((k,v) <- values){
//              dataStream.println(s"let ${ind.name}[${stringOfConstant(k)}] := $v;")
//            }
//          //case Var2(indicesA,indicesB, name,_,_) => s"var ${name} {${r(indicesA)}, ${r(indicesB)}} $bounds;"
//        }
//
//
//
//    }
//
//
//    modStream.println("include constraints.mod;")
//
//    modStream.println(
//      s"""
//         |###############
//         |#  Objectives #
//         |###############
//       """.stripMargin)
//    for (eq <- model.objectives) {
//
//      modStream.println(
//        s"minimize ${
//          eq.name
//        }: ${stringOf(eq)};"
//      )
//    }
//
//    modStream.close()
//    dataStream.close()
//    constraintsStream.close()
//
//    val runStream = new PrintWriter("test.run")
//    runStream.println(
//      s"""
//         |model test.mod;
//         |data test.dat;
//         |option solver cplexamp;
//         |solve;
//         |
//       """.stripMargin)
//
//    runStream.println(
//      s"""
//         | ##################
//         | # Print Solution #
//         | ##################
//       """.stripMargin)
//
//    runStream.println(s"""printf "========\\n";""")
//
//    for ((v, id) <- model.variables) {
//      v match {
//        case Var0(n, _, _) => runStream.println(s"""printf "${v.name} = %d\\n", ${v.name};""")
//        case v1@Var1(indices, _, _, _) =>
//          val pattern = s"""${v.name} =""" + (" %d" * indices.values.size)
//          val variables = indices.values.map { value => stringOf(v1.get(value)) }.mkString(", ")
//          runStream.println(s"""printf "$pattern\\n", $variables;""")
//        case v2@Var2(indicesA, indicesB, _, _, _) =>
//          val pattern = s"""${v.name} =""" + (" %d" * indicesA.values.size * indicesB.values.size)
//          val variables = (for(a <- indicesA.values; b <- indicesB.values) yield stringOf(v2.get(a)(b)) ).mkString(", ")
//          runStream.println(s"""printf "$pattern\\n", $variables;""")
//      }
//    }
//
//    runStream.close()
//
//  }
//
//  def solve: ComputationResult[SolutionBuffer[O, C,Double]] = {
//    println("ampl test.run".!!)
//
//    val successRegex = ".+: optimal solution; objective (.+)".r
//
//    val output = "ampl test.run".lineStream
//    output.head match {
//      case successRegex(objValue) =>
//        val valuesOfVariables = output.dropWhile(_ != "========").drop(1).map {
//          line =>
//            val eqIndex = line.indexOf(" = ")
//            line.take(eqIndex) -> line.drop(eqIndex + 3)
//        }.toIterator
//
//        val solution = new SolutionBuffer[O, C,Double]()
//
//        for ((v, id) <- model.variables) {
//          val line = valuesOfVariables.next
//          println(line)
//          val (vName, values) = line
//          assume(vName == v.name)
//          v match {
//            case v0@Var0(name, _, _) =>
//              solution.update(v0, values.toDouble)
//            case v1@Var1(indices, _, _, _) =>
//              solution(v1) = values.split(" ").toIndexedSeq.map(_.toDouble)
//            case v2@Var2(indicesA, indicesB, _, _, _) =>
//              solution(v2) = values.split(" ").toIndexedSeq.map(_.toDouble)
//          }
//        }
//
//        solution.objectiveValue = objValue.toDouble
//        Optimality(solution)
//      case _ => Error(output.mkString("\n"))
//    }
//
//
//  }
//}
//
//trait ComputationResult[+T] {
//
//}
//
//case class Optimality[+T](result: T) extends ComputationResult[T]
//
//case object Infeasability extends ComputationResult[Nothing]
//
//case class Error(msg: String) extends ComputationResult[Nothing]