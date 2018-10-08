package oscar.examples.cbls.routing

object TestComposeFunction extends App{

  private def composeFunction (f1: TransfertFunction, f2: TransfertFunction, m: Int): TransfertFunction ={

    val earliestArrivalTimeAt2 = f1.l + m
    val latestArrivalTimeAt2 = f1.d + f1.l - f1.e + m

    val earliestArrivalTimeAt2_earlier_or_equal_than_earliestStartingTimeAt2 = earliestArrivalTimeAt2 <= f2.e
    val earliestArrivalTimeAt2_earlier_or_equal_than_latestStartingTimeAt2 = earliestArrivalTimeAt2 <= f2.d
    val latestArrivalTimeAt2_earlier_or_equal_than_earliestStartingTimeAt2 = latestArrivalTimeAt2 <= f2.e
    val latestArrivalTimeAt2_earlier_or_equal_than_latestStartingTimeAt2 = latestArrivalTimeAt2 <= f2.d

    val (e3, d3, l3) =
      (earliestArrivalTimeAt2_earlier_or_equal_than_earliestStartingTimeAt2,
        earliestArrivalTimeAt2_earlier_or_equal_than_latestStartingTimeAt2,
        latestArrivalTimeAt2_earlier_or_equal_than_earliestStartingTimeAt2,
        latestArrivalTimeAt2_earlier_or_equal_than_latestStartingTimeAt2) match{
        case (true,true,true,true) =>
          (f1.d, f1.d, f2.l)
        case (true,true,false,true) =>
          (f2.e - f1.l - m + f1.e, f1.d, f2.l)
        case (false,true,false,true) =>
          (f1.e, f1.d,f1.l + f2.l - f2.e + m)
        case (true,true,false,false) =>
          (f2.e - f1.l - m + f1.e, f2.d - f1.l - m + f1.e, f2.l)
        case (false,true,false,false) =>
          (f1.e, f2.d - f1.l - m + f1.e, f1.l + f2.l - f2.e + m)
        case (false,false,false,false) =>
          (1, -1, -1)
        case _ =>
          throw new Error("Unhandled case : " + (earliestArrivalTimeAt2_earlier_or_equal_than_earliestStartingTimeAt2,
            earliestArrivalTimeAt2_earlier_or_equal_than_latestStartingTimeAt2,
            latestArrivalTimeAt2_earlier_or_equal_than_earliestStartingTimeAt2,
            latestArrivalTimeAt2_earlier_or_equal_than_latestStartingTimeAt2))
      }

    if(e3 > d3)
      new TransfertFunction(Int.MaxValue,Int.MaxValue,Int.MaxValue)
    else
      new TransfertFunction(e3, d3, l3)
  }

  private def permutationMethod(): Unit ={
    val permutationValues = List(0,1,2,4,8,16,32)
    var permutations: Set[Array[Int]] = Set()

    def permute(permValues: List[Int], currentPermutation:  Array[Int] = Array.fill(permutationValues.size)(0)): Unit ={
      if(permValues.isEmpty)
        permutations = permutations + currentPermutation.clone()
      else
        for(curValue <- permValues){
          currentPermutation(currentPermutation.length - permValues.size) = curValue
          permute(permValues.diff(List(curValue)), currentPermutation)
        }
    }
    permute(permutationValues)

    for(permutation <- permutations if permutation.toSet.size == permutation.length){
      val it = permutation.toIterator
      val e1 = it.next()
      val d1 = it.next()
      val l1 = it.next()
      val m = it.next()
      val e2 = it.next()
      val d2 = it.next()
      val l2 = it.next()
      if(e1 <= d1 && e2 <= d2 && e1 <= l1 && e2 <= l2) {
        val f1 = new TransfertFunction(e1, d1, l1)
        val f2 = new TransfertFunction(e2, d2, l2)
        test(f1,f2,m, (0 until permutationValues.max * 2).toArray)
      }
    }
  }

  private def easyMethodWithEqualities() {
    val n = 10
    val timeMatrix = Array.tabulate(n)(x => Array.tabulate(n)(y => 10))

    val earlylines = Array.tabulate(n)(x => 100 + 20 * x)
    val deadlines = Array.tabulate(n)(x => 200 + 20 * x)
    val taskDurations = Array.tabulate(n)(x => 0)

    val transfertFunctions = Array.tabulate(n)(x => new TransfertFunction(earlylines(x), deadlines(x), earlylines(x)))

    for (x <- 0 until n) {
      for (y <- 0 until n) {
        test(transfertFunctions(y), transfertFunctions(x), timeMatrix(y)(x), (0 until 100).toArray.map(_*5))
      }
    }
  }

  private def test(f1: TransfertFunction, f2: TransfertFunction, travelDuration: Int, ts: Array[Int]): Unit ={
    val composedFunction = composeFunction(f1, f2, travelDuration)
    val t2s = Array.fill(ts.length)(0)
    for (i <- ts.indices) {
      val composedRes = composedFunction.apply(ts(i)).getOrElse(Int.MaxValue)
      val trueRes = f2.apply(
        f1.apply(ts(i)).getOrElse(Int.MaxValue - travelDuration) + travelDuration).
        getOrElse(Int.MaxValue)

      require(composedRes == trueRes,
        "ComposedRes : " + composedRes + " is equal to trueRes : " + trueRes +
          "\n t : " + ts(i) +
          "\n m : " + travelDuration +
          " \n " + composedFunction.toString +
          "\n " + f1.toString + "\n " + f2.toString)

      t2s(i) = trueRes
    }
    val l3 = t2s.head
    val l1m = f1.l + travelDuration
    val l1d1e1m = f1.l + f1.d - f1.e + travelDuration
    val l1mLessE2 = l1m <= f2.e
    val l1mLessD2 = l1m <= f2.d
    val l1d1e1mLessE2 = l1d1e1m <= f2.e
    val l1d1e1mLessD2 = l1d1e1m <= f2.d
    val e3 = t2s.reverse.dropWhile(x => x != l3).length - 1
    val d3 = t2s.reverse.dropWhile(x => x == Int.MaxValue).length - 1
    println("l1mLessE2 : " + l1mLessE2 +
      "\nl1mLessD2 : " + l1mLessD2 +
      "\nl1d1e1mLessE2 : " + l1d1e1mLessE2 +
      "\nl1d1e1mLessD2 : " + l1d1e1mLessD2 +
      "\ne1 : " + f1.e + ", d1 : " + f1.d + ", l1 : " + f1.l +
      "\ne2 : " + f2.e + ", d2 : " + f2.d + ", l2 : " + f2.l +
      "\ne3 : " + e3 + ", d3 : " + d3 +", l3 : " + l3 + ", m : " + travelDuration)
  }

  easyMethodWithEqualities()
}


class TransfertFunction (val e: Int, val d: Int, val l: Int){
  require(d >= e && l >= e)
  def apply(t: Int): Option[Int] ={
    if(t <= e)
      Some(l)
    else if(t <= d)
      Some(t + l - e)
    else
      None
  }

  override def toString: String = {
    "e : " + e + "\n d : " + d + "\n l : " + l
  }
}