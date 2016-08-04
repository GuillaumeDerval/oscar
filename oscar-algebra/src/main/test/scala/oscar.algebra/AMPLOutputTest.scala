//
//package oscar.algebra
//
//import org.scalatest.FunSuite
//import org.scalatest.matchers.ShouldMatchers
//import oscar.algebra.ampl.{AMPLOutput, Indices, Optimality}
//
//import scala.collection.immutable.IndexedSeq
//
//class AMPLTests extends FunSuite with ShouldMatchers {
//
//  test("Model") {
//
//    implicit val model = new Model[Linear, Linear,Double]()
//
//    val Set = Indices("VS", 0 until 10)
//
//    val vs = Var1(Set, "X", Some(0.0), Some(20.0))
//
//    model.subjectTo(for (i <- Set) yield "E1" ||: Const(10.0) >=[Linear,Double] vs(i))
//
//    model.subjectTo("C1" ||: Const(2.0) <= vs(2))
//
//    model.minimize("Main_Objective" ||: vs(2))
//
//    println(model)
//
//    println(model.maxIndex)
//    val out = new AMPLOutput
//
//    out.write
//    out.solve match {
//      case Optimality(solution: SolutionBuffer[Linear,Linear,Double]) => solution(vs.get(2)) shouldBe 2
//    }
//
//
//  }
//
//  test("Var2"){
//
//    implicit val model = new Model[Linear, Linear,Double]()
//
//    val SetA = Indices("A", 0 until 10)
//    val SetB = Indices("B", 0 until 10)
//
//    val vars = Var2(SetA,SetB, "X", Some(10.0))
//
//    model.subjectTo("C" ||: (for(a <- SetA) yield s"C_$a" ||: Const(10.0) <= vars(a)(3)-vars(a)(2) ))
//
//    model.minimize(vars(0)(3))    //for(a<- SetA; b <- SetB) yield vars(a)(b))
//
//    println(model)
//
//    println(model.maxIndex)
//    val out = new AMPLOutput
//
//    out.write
//    out.solve match {
//      case Optimality(solution: SolutionBuffer[Linear,Linear,Double]) => solution(vars.get(0)(3)) shouldBe 20
//    }
//  }
//
//  test("Strings in Sets"){
//    implicit val model = new Model[Linear, Linear,Double]()
//
//    val SetA = Indices("A", IndexedSeq("a","b"))
//
//
//    val vars = Var1(SetA, "X", Some(10.0))
//
//    model.subjectTo("E" ||: vars("a") <= vars("b"))
//
//    val out = new AMPLOutput
//
//    out.write
//    out.solve
//  }
//
//  test{"Symbolic Sums"}{
//    implicit val model = new Model[Linear, Linear,Double]()
//
//    val SetA = Indices("A", IndexedSeq("a","b"))
//
//    val vars = Var1(SetA, "X", Some(10.0))
//
//    model.subjectTo("E" ||: Const(2.0) >= new SumLoop[String,Linear,Double](for(a <- SetA) yield vars(a).toExpression).toExpression )
//
//    val out = new AMPLOutput
//
//    out.write
//    out.solve
//  }
//
//  test("Parameters"){
//    implicit val model = new Model[Linear, Linear,Double]()
//
//    val SetA = Indices("A", IndexedSeq("a","b"))
//
//    val param = new Param1("P",SetA,Map("a" -> 3.0, "b" -> 4.0), None)
//
//    model.subjectTo("E" ||: Const(2.0) >= new SumLoop[String,Linear,Double](for(a <- SetA) yield param(a)) )
//    val out = new AMPLOutput
//
//    out.write
//    out.solve
//  }
//
//  test("Size"){
//
//    implicit val model = new Model[Linear, Linear,Int]()
//
//    val rng = 1 to 10000000 toStream;
//
////    val SetA = Indices("A", rng)
////    val SetB = Indices("B", 2 to 100000000)
//
////    val v1: Stream[Var0] = rng.toStream.map{ i => Var0( s"X_${i}", Some(0))}
////    val v2 = rng.toStream.map{ i => Var0( s"Y_${i}", Some(0))}
////
////    println("vars created")
////    model.subjectTo(new StreamSystem(for ( (v1p,v2p) <- v1.zip(v2)) yield{
////      s"C_${v1p.name}_${v2p.name}" ||: v1p <= v2p -10
////    }))
////
////    val lb = new Array[Double](rng.size)
////    var i = 0
////    v1.foreach { v =>
////      lb(i) = v.lowerBound.getOrElse(-1.0)
////      i += 1
////    }
//
////    val v1 = Var1(SetA, "X", Some(0))
////    val v2 = Var1(SetA, "Y", Some(0))
////    model.subjectTo(for (a <- SetA) yield s"E_$a" ||: v1(a) <= v2(a) - 10)
//
//
////    val out = new AMPLOutput
////
////    out.write
//  }
//}
//
