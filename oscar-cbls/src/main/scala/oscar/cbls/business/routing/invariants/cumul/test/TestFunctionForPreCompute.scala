package oscar.cbls.business.routing.invariants.cumul.test

import oscar.cbls.business.routing.invariants.base.{FetchFromPreCompute, FetchFromPreComputeReverseWithOperator, FromScratch}
import oscar.cbls.business.routing.invariants.group.{FetchFromPreCompute, FetchFromPreComputeReverseWithOperator, FromScratch, FunctionForPreCompute}


/**
  * @author Quentin Meurisse
  */
class TestFunctionForPreCompute extends FunSuite with Matchers{

  test("insert node with no pre-compute"){
    val f = FunctionForPreCompute(IntSequence(0 to 7))
    val g = FunctionForPreCompute.updateFunctionForInsert(f, 10, 5)

    for(i <- 0 to 4){g.fun(i) should be(i)}

    (g.fun(5) > g.maxValueWithPreCompute) should be(true)

    for(i <- 6 to 8){(g.fun(i) == i-1) should be(true)}

  }


  test("delete node with pre-compute"){
    val f = FunctionForPreCompute(IntSequence(0 to 7))
    val g = FunctionForPreCompute.updateFunctionForDelete(f, 5)

    for(x <- 0 to 4){g.fun(x) should be(x)}

    for(x <- 5 to 6){g.fun(x) should be(x+1)}


  }

  test("delete last node"){
    val f = FunctionForPreCompute(IntSequence(0 to 7))
    val g = FunctionForPreCompute.updateFunctionForDelete(f, 7)

    f.externalPositionOfLastRoutedNode should be(7)
    g.externalPositionOfLastRoutedNode should be(6)
  }


  test("move segment and flip it"){
    val f = FunctionForPreCompute(IntSequence(0 to 10))
    val g = FunctionForPreCompute.updateFunctionForMove(f, 2, 4, 7, flip = true)

    g.fun(0) should be(0)
    g.fun(1) should be(1)

    for (x <- 2 to 4){g.fun(x) should be(x+3)}

    for (x <- 5 to 7){g.fun(x) should be(9-x)}

    for (x <- 8 to 10){g.fun(x) should be(x)}
  }

  test("kindOfPrecompute test"){
    val a = FunctionForPreCompute(IntSequence(0 to 42))
    val b = FunctionForPreCompute.updateFunctionForInsert(a, 43, 43)
    val c = FunctionForPreCompute.updateFunctionForInsert(b, 44, 44)
    val d = FunctionForPreCompute.updateFunctionForInsert(c, 50, 27)
    val e = FunctionForPreCompute.updateFunctionForInsert(d, 51, 28)
    val f = FunctionForPreCompute.updateFunctionForMove(e, 10, 20, 9, true)

    val x = f.kindOfComputation(0, 46)

    x(0) should be(an [FetchFromPreCompute])
    x(0).fromPos should be(0)
    x(0).toPos should be(9)

    x(1) should be(an [FetchFromPreComputeReverseWithOperator])
    x(1).fromPos should be(10)
    x(1).toPos should be(20)

    x(2) should be(an [FetchFromPreCompute])
    x(2).fromPos should be(21)
    x(2).toPos should be(26)

    x(3) should be(an [FromScratch])
    x(3).fromPos should be(27)
    x(3).toPos should be(28)
    x(3).asInstanceOf[FromScratch].topOfStack should be(false)




    x(4) should be(an [FetchFromPreCompute])
    x(4).fromPos should be(29)
    x(4).toPos should be(44)

    x(5) should be(an [FromScratch])
    x(5).fromPos should be(45)
    x(5).toPos should be(46)
    x(5).asInstanceOf[FromScratch].topOfStack should be(false)

  }

  test("test kindOfComputation not starting with pivot 0"){
    val f = FunctionForPreCompute(IntSequence(0 to 42))
    val g = FunctionForPreCompute.updateFunctionForMove(f, 10, 20, 30, flip = false)

    val x = g.kindOfComputation(15, 25)

    x(0) should be(an [FetchFromPreCompute])
    x(0).asInstanceOf[FetchFromPreCompute].fromPos should be(15)
    x(0).asInstanceOf[FetchFromPreCompute].toPos should be(19)

    x(1) should be(an [FetchFromPreCompute])
    x(1).asInstanceOf[FetchFromPreCompute].fromPos should be(20)
    x(1).asInstanceOf[FetchFromPreCompute].toPos should be(25)
  }

  test("test obtains kind of computation step with invalid bounds"){
    val f = FunctionForPreCompute(IntSequence(0 to 7))

    an [IllegalArgumentException] should be thrownBy f.kindOfComputation(2, 42)

    an [IllegalArgumentException] should be thrownBy f.kindOfComputation(-1, 5)
  }

  test("stack insert"){
    val seq = IntSequence(Array(0, 15, 42, 1, 67, 7, 2, 25, 36, 9))
    val f = FunctionForPreCompute(seq)
    val g = FunctionForPreCompute.updateFunctionForDelete(f, 4)
    val h = FunctionForPreCompute.updateFunctionForDelete(g, 6)

    h.fun(4) should be(5)
    h.fun(6) should be(8)

    val i = FunctionForPreCompute.stackInsert(h, 10, 3)

    val x = i.kindOfComputation(0, i.externalPositionOfLastRoutedNode)

    x(0).fromPos should be(0)
    x(0).toPos should be(2)

    x(1) should be(an [FromScratch])
    x(1).fromPos should be(3)
    x(1).toPos should be(3)
    x(1).asInstanceOf[FromScratch].topOfStack should be(true)

    x(2).fromPos should be(3)
    x(2).toPos should be(3)

    x(3).fromPos should be(4)
    x(3).toPos should be(5)

    x(4).fromPos should be(6)
    x(4).toPos should be(7)

    var a = seq.delete(4)
    a = a.delete(6)
    a = a.insertAtPosition(10, 3)

    val posOf1 = a.explorerAtAnyOccurrence(1).get.position
    val y = i.kindOfComputation(0, posOf1)

    y(0).fromPos should be(0)
    y(0).toPos should be(2)

    y(1).fromPos should be(3)
    y(1).toPos should be(3)

    val fromPos = a.explorerAtAnyOccurrence(2).get.position
    val toPos = i.externalPositionOfLastRoutedNode
    val z = i.kindOfComputation(fromPos, toPos)

    z(0).fromPos should be(5)
    z(0).toPos should be(5)

    z(1).fromPos should be(6)
    z(1).toPos should be(7)

  }

  test("stack insert at last position"){
    val seq = IntSequence(Array(0, 15, 42, 1, 67, 7, 2, 25, 36, 9))
    val f = FunctionForPreCompute(seq)
    val g = FunctionForPreCompute.updateFunctionForDelete(f, 4)
    val h = FunctionForPreCompute.updateFunctionForDelete(g, 6)

    val i = FunctionForPreCompute.stackInsert(h, 10, 8)

    val x = i.kindOfComputation(0, 8)

    x should have length 4
    x(0).fromPos should be(0)
    x(0).toPos should be(3)
    x(1).fromPos should be(4)
    x(1).toPos should be(5)
    x(2).fromPos should be(6)
    x(2).toPos should be(7)
    x(3) should be(an [FromScratch])
    x(3).fromPos should be(8)
    x(3).toPos should be(8)
    x(3).asInstanceOf[FromScratch].topOfStack should be(true)

  }

  test("stack delete"){
    val f = FunctionForPreCompute(IntSequence(Array(0, 15, 42, 1, 67, 7, 2, 25, 36, 9)))
    val g = FunctionForPreCompute.updateFunctionForDelete(f, 4)
    val h = FunctionForPreCompute.updateFunctionForDelete(g, 6)

    val i = FunctionForPreCompute.stackDelete(h, 2)

    val x = i.kindOfComputation(0, i.externalPositionOfLastRoutedNode)

    x(0).fromPos should be(0)
    x(0).toPos should be(1)

    x(1).fromPos should be(3)
    x(1).toPos should be(3)

    x(2).fromPos should be(4)
    x(2).toPos should be(5)

    x(3).fromPos should be(6)
    x(3).toPos should be(7)

  }

  test("stack delete and get segment for a vehicle"){
    val f = FunctionForPreCompute(IntSequence(Array(0, 1, 2, 27, 28, 13, 18, 7, 3, 4)))
    val g = FunctionForPreCompute.stackDelete(f, 6)

    val x = g.kindOfComputation(2, 6)

    x should have length 2
    x(0).fromPos should be(2)
    x(0).toPos should be(5)
    x(1).fromPos should be(7)
    x(1).toPos should be(7)

    val y = g.kindOfComputation(7, 7)
    y should have length 1
    y(0).fromPos should be(8)
    y(0).toPos should be(8)
  }

  test("stacked flip"){
    val f = FunctionForPreCompute(IntSequence(Array(0, 15, 42, 1, 67, 7, 2, 25, 36, 9)))
    val g = FunctionForPreCompute.updateFunctionForDelete(f, 4)
    val h = FunctionForPreCompute.updateFunctionForDelete(g, 6)

    val i = FunctionForPreCompute.stackMove(f, 3, 7, 2, true)

    val x = i.kindOfComputation(0, 9)

    x(0).fromPos should be(0)
    x(0).toPos should be(2)

    x(1) should be(an [FetchFromPreComputeReverseWithOperator])
    x(1).fromPos should be(7)
    x(1).toPos should be(3)

    x(2).fromPos should be(8)
    x(2).toPos should be(9)

    val j = FunctionForPreCompute.stackMove(h, 3, 7, 2, true)

    val y = j.kindOfComputation(0, 7)

    y(1).fromPos should be(7)
    y(1).toPos should be(6)

    y(2).fromPos should be(5)
    y(2).toPos should be(4)

    y(3).fromPos should be(3)
    y(3).toPos should be(3)

    val k = FunctionForPreCompute.updateFunctionForMove(h, 3, 7, 2, true)
    val l = FunctionForPreCompute.stackMove(k, 5, 7, 4, true)

    val z = l.kindOfComputation(0, 7)

    z(1) should be(an [FetchFromPreComputeReverseWithOperator])
    z(1).fromPos should be(3)
    z(1).toPos should be(4)

    z(2) should be(an [FetchFromPreCompute])
    z(2).fromPos should be(7)
    z(2).toPos should be(7)

    z(3) should be(an [FetchFromPreCompute])
    z(3).fromPos should be(6)
    z(3).toPos should be(5)

    l.fun(7) should be(3)
  }


  test("stack move a segment upwards on a same vehicle"){
    val f = FunctionForPreCompute(IntSequence(Array(0, 15, 42, 1, 67, 7, 2, 25, 36, 9)))
    val g = FunctionForPreCompute.updateFunctionForDelete(f, 4)
    val h = FunctionForPreCompute.updateFunctionForDelete(g, 6)


    val i = FunctionForPreCompute.stackMove(h, 4, 5, 7, false)

    val x = i.kindOfComputation(0, 7)


    x(0).fromPos should be(0)
    x(0).toPos should be(3)

    x(1).fromPos should be(6)
    x(1).toPos should be(7)

    x(2).fromPos should be(4)
    x(2).toPos should be(5)

    val j = FunctionForPreCompute.stackMove(h, 4, 5, 7, true)

    val y = j.kindOfComputation(0, 7)

    y(0).fromPos should be(0)
    y(0).toPos should be(3)

    y(1).fromPos should be(6)
    y(1).toPos should be(7)

    y(2) should be(an [FetchFromPreComputeReverseWithOperator])
    y(2).fromPos should be(5)
    y(2).toPos should be(4)

  }

  test("stack move a segment downwards on same vehicle"){
    val f = FunctionForPreCompute(IntSequence(Array(0, 15, 42, 1, 67, 7, 2, 25, 36, 9)))
    val g = FunctionForPreCompute.updateFunctionForDelete(f, 4)
    val h = FunctionForPreCompute.updateFunctionForDelete(g, 6)


    val i = FunctionForPreCompute.stackMove(h, 6, 7, 3, false)

    val x = i.kindOfComputation(0, 7)


    x(0).fromPos should be(0)
    x(0).toPos should be(3)

    x(1).fromPos should be(6)
    x(1).toPos should be(7)

    x(2).fromPos should be(4)
    x(2).toPos should be(5)

    val j = FunctionForPreCompute.stackMove(h, 6, 7, 3, true)

    val y = j.kindOfComputation(0, 7)

    y(0).fromPos should be(0)
    y(0).toPos should be(3)

    y(1) should be(an [FetchFromPreComputeReverseWithOperator])
    y(1).fromPos should be(7)
    y(1).toPos should be(6)

    y(2).fromPos should be(4)
    y(2).toPos should be(5)
  }

  test("stack move a segment upwards with different vehicles"){
    val seq = IntSequence(Array(0, 15, 42, 32, 1, 67, 7, 27, 17, 2, 25, 36, 9, 16, 4))
    val f = FunctionForPreCompute(seq)

    val g = FunctionForPreCompute.stackMove(f, 5, 6, 8, false)
    var updatedSeq = seq.moveAfter(5, 6, 8, false)

    var steps = g.kindOfComputation(4, 8)

    steps(0).fromPos should be(4)
    steps(0).toPos should be(4)

    steps(1).fromPos should be(7)
    steps(1).toPos should be(8)

    steps(2).fromPos should be(5)
    steps(2).toPos should be(6)

    steps = g.kindOfComputation(0, 3)

    steps(0).fromPos should be(0)
    steps(0).toPos should be(3)

    steps = g.kindOfComputation(9, 14)
    steps(0).fromPos should be(9)
    steps(0).toPos should be(14)

    val h = FunctionForPreCompute.stackMove(f, 1, 2, 11, true)
    updatedSeq = seq.moveAfter(1, 2, 11, true)

    val from0 = 0
    val from1 = updatedSeq.positionOfAnyOccurrence(1).get
    val from2 = updatedSeq.positionOfAnyOccurrence(2).get

    steps = h.kindOfComputation(from0, from1 -1)
    steps should have length 2
    steps(0).fromPos should be(0)
    steps(0).toPos should be(0)
    steps(1).fromPos should be(3)
    steps(1).toPos should be(3)

    steps = h.kindOfComputation(from1, from2 -1)
    steps should have length 1
    steps(0).fromPos should be(4)
    steps(0).toPos should be(8)

    steps = h.kindOfComputation(from2, h.externalPositionOfLastRoutedNode)
    steps should have length 3
    steps(0).fromPos should be(9)
    steps(0).toPos should be(11)
    steps(1) should be(an [FetchFromPreComputeReverseWithOperator])
    steps(1).fromPos should be(2)
    steps(1).toPos should be(1)
    steps(2).fromPos should be(12)
    steps(2).toPos should be(14)
  }

  test("stack move upwards to end of vehicle route"){
    val seq = IntSequence(Array(0, 15, 42, 32, 1, 67, 7, 27, 17, 2, 25, 36, 9, 16, 4))
    val f = FunctionForPreCompute(seq)

    val g = FunctionForPreCompute.stackMove(f, 1, 3, 14, false)
    val updatedSeq = seq.moveAfter(1, 3, 14, false)

    val from = updatedSeq.positionOfAnyOccurrence(2).get

    val x = g.kindOfComputation(from, g.externalPositionOfLastRoutedNode)

    x(0).fromPos should be(9)
    x(0).toPos should be(14)
    x(1).fromPos should be(1)
    x(1).toPos should be(3)

    val y = g.kindOfComputation(0, 0)
    y should have length 1
    y(0).fromPos should be(0)
    y(0).toPos should be(0)
  }

  test("satck move a segment downwards with different vehicles"){
    val seq = IntSequence(Array(0, 15, 42, 32, 1, 67, 7, 27, 17, 2, 25, 36, 9, 16, 4))
    val f = FunctionForPreCompute(seq)

    val g = FunctionForPreCompute.stackMove(f, 7, 8, 4, false)

    var steps = g.kindOfComputation(4, 8)

    steps(0).fromPos should be(4)
    steps(0).toPos should be(4)
    steps(1).fromPos should be(7)
    steps(1).toPos should be(8)
    steps(2).fromPos should be(5)
    steps(2).toPos should be(6)

    steps = g.kindOfComputation(9, 14)
    steps(0).fromPos should be(9)
    steps(0).toPos should be(14)

    val h = FunctionForPreCompute.stackMove(f, 11, 13, 1, true)
    val updatedSeq = seq.moveAfter(11, 13, 1, true)

    val from0 = 0
    val from1 = updatedSeq.positionOfAnyOccurrence(1).get
    val from2 = updatedSeq.positionOfAnyOccurrence(2).get

    steps = h.kindOfComputation(from0, from1 - 1)
    steps should have length 3
    steps(0).fromPos should be(0)
    steps(0).toPos should be(1)
    steps(1) should be(an [FetchFromPreComputeReverseWithOperator])
    steps(1).fromPos should be(13)
    steps(1).toPos should be(11)
    steps(2).fromPos should be(2)
    steps(2).toPos should be(3)

    steps = h.kindOfComputation(from1, from2 - 1)
    steps should have length 1
    steps(0).fromPos should be(4)
    steps(0).toPos should be(8)

    steps = h.kindOfComputation(from2, h.externalPositionOfLastRoutedNode)
    steps should have length 2
    steps(0).fromPos should be(9)
    steps(0).toPos should be(10)
    steps(1).fromPos should be(14)
    steps(1).toPos should be(14)
  }

  test("satck move segment downwards to end of vehicle route"){
    val seq = IntSequence(Array(0, 15, 42, 32, 1, 67, 7, 27, 17, 2, 25, 36, 9, 16, 4))
    val f = FunctionForPreCompute(seq)

    val g = FunctionForPreCompute.stackMove(f, 12, 14, 8, false)
    val updatedSeq = seq.moveAfter(12, 14, 8, false)

    val from1 = updatedSeq.positionOfAnyOccurrence(1).get
    val from2 = updatedSeq.positionOfAnyOccurrence(2).get

    val x = g.kindOfComputation(from1, from2 - 1)
    x should have length 2
    x(0).fromPos should be(4)
    x(0).toPos should be(8)
    x(1).fromPos should be(12)
    x(1).toPos should be(14)

    val y = g.kindOfComputation(from2, g.externalPositionOfLastRoutedNode)
    y should have length 1
    y(0).fromPos should be(9)
    y(0).toPos should be(11)
  }

  test("stack move segment upwards with function not equal to identity"){
    val seq = IntSequence(Array(0, 7, 14, 21, 28, 35, 42, 49, 56, 63, 70, 1, 9, 16, 25, 36, 81, 100, 121, 144 ,2, 4, 8, 16, 32, 64, 128, 256, 512, 1024))
    val f = FunctionForPreCompute(seq)

    var g = FunctionForPreCompute.updateFunctionForDelete(f, 3)
    var updatedSeq = seq.delete(3)
    g = FunctionForPreCompute.updateFunctionForDelete(g, 14)
    updatedSeq = updatedSeq.delete(14)
    g = FunctionForPreCompute.updateFunctionForMove(g, 12, 16, 11, true)
    updatedSeq = updatedSeq.moveAfter(12, 16, 11, true)
    g = FunctionForPreCompute.updateFunctionForMove(g, 20, 21, 27, true)
    updatedSeq = updatedSeq.moveAfter(20, 21, 27, true)

     val h = FunctionForPreCompute.stackMove(g, 1, 5, 20, false)
    updatedSeq = updatedSeq.moveAfter(1, 5, 20, false)

    val from0 = 0
    val from1 = updatedSeq.positionOfAnyOccurrence(1).get
    val from2 = updatedSeq.positionOfAnyOccurrence(2).get

    var steps = h.kindOfComputation(from0, from1 - 1)
    steps should have length 2
    steps(0).fromPos should be(0)
    steps(0).toPos should be(0)
    steps(1).fromPos should be(6)
    steps(1).toPos should be(9)

    steps = h.kindOfComputation(from1, from2 -1)
    steps should have length 4
    steps(0).fromPos should be(10)
    steps(0).toPos should be(11)
    steps(1).fromPos should be(12)
    steps(1).toPos should be(14)
    steps(2).fromPos should be(15)
    steps(2).toPos should be(16)
    steps(3).toPos should be(17)
    steps(3).toPos should be(17)

    steps = h.kindOfComputation(from2, g.externalPositionOfLastRoutedNode)
    steps should have length 6
    steps(0).fromPos should be(18)
    steps(0).toPos should be(19)
    steps(1).fromPos should be(20)
    steps(1).toPos should be(20)
    steps(2).fromPos should be(1)
    steps(2).toPos should be(2)
    steps(3).fromPos should be(3)
    steps(3).toPos should be(5)
    steps(4).fromPos should be(21)
    steps(4).toPos should be(25)
    steps(5).fromPos should be(26)
    steps(5).toPos should be(27)
  }

  test("stack move segment downwards with function not equal to identity"){
    val seq = IntSequence(Array(0, 7, 14, 21, 28, 35, 42, 49, 56, 63, 70, 1, 9, 16, 25, 36, 81, 100, 121, 144 ,2, 4, 8, 16, 32, 64, 128, 256, 512, 1024))
    val f = FunctionForPreCompute(seq)

    var g = FunctionForPreCompute.updateFunctionForDelete(f, 3)
    var updatedSeq = seq.delete(3)
    g = FunctionForPreCompute.updateFunctionForDelete(g, 14)
    updatedSeq = updatedSeq.delete(14)
    g = FunctionForPreCompute.updateFunctionForMove(g, 12, 16, 11, true)
    updatedSeq = updatedSeq.moveAfter(12, 16, 11, true)
    g = FunctionForPreCompute.updateFunctionForMove(g, 20, 21, 27, true)
    updatedSeq = updatedSeq.moveAfter(20, 21, 27, true)

    val h = FunctionForPreCompute.stackMove(g, 20, 23, 9, true)
    updatedSeq = updatedSeq.moveAfter(20, 23, 9, true)

    val from0 = 0
    val from1 = updatedSeq.positionOfAnyOccurrence(1).get
    val from2 = updatedSeq.positionOfAnyOccurrence(2).get

    var steps = h.kindOfComputation(from0, from1 - 1)
    steps should have length 3
    steps(0).fromPos should be(0)
    steps(0).toPos should be(2)
    steps(1).fromPos should be(3)
    steps(1).toPos should be(9)
    steps(2).fromPos should be(23)
    steps(2).toPos should be(20)

    steps = h.kindOfComputation(from1, from2 -1)
    steps should have length 4
    steps(0).fromPos should be(10)
    steps(0).toPos should be(11)
    steps(1).fromPos should be(12)
    steps(1).toPos should be(14)
    steps(2).fromPos should be(15)
    steps(2).toPos should be(16)
    steps(3).toPos should be(17)
    steps(3).toPos should be(17)

    steps = h.kindOfComputation(from2, g.externalPositionOfLastRoutedNode)
    steps should have length 3
    steps(0).fromPos should be(18)
    steps(0).toPos should be(19)
    steps(1).fromPos should be(24)
    steps(1).toPos should be(25)
    steps(2).fromPos should be(26)
    steps(2).toPos should be(27)

  }
}
