package oscar.cbls.test.scheduling

import oscar.cbls.Store
import oscar.cbls.business.scheduling.model._
import oscar.cbls.business.scheduling.neighborhood.{ReinsertActivity, SwapActivity}
import oscar.cbls.core.objective.Objective
import oscar.cbls.lib.search.combinators.{BestSlopeFirst, Profile}

import scala.util.Random

object BigExample {
  val nbAct = 100
  val nbRes = 100
  val minDuration = 1L
  val maxDuration = 25L
  val minCapacity = 1L
  val maxCapacity = 25L
  val minRMRes = 0
  val maxRMRes = 100
  val densityUsageRes = 25
  val minSetupTimeRM = 0L
  val maxSetupTimeRM = 25L
  val densityPrecedencies = 5
  val densityMultiResources = 50
  // Random Generator
  val randomGen = new Random()

  def randomBoolean(density: Int): Boolean = {
    val rdVal = randomGen.nextInt(100)
    rdVal < density
  }

  def randomInterval(inf: Int, sup: Int): Int = {
    require(inf <= sup)
    val offset = if (inf == sup) 0 else randomGen.nextInt(sup - inf)
    inf + offset
  }

  def randomInterval(inf: Long, sup: Long): Long = {
    require(inf <= sup)
    val rdVal = if (inf == sup) 0L else Math.abs(randomGen.nextLong()) % (sup - inf)
    inf + rdVal
  }

  def createRandomProblem(m: Store): Schedule = {
    // Activities
    //val nAct = randomInterval(1, nbAct)
    val nAct = nbAct
    val durations: Array[Long] = Array.tabulate(nAct)(_ => randomInterval(minDuration, maxDuration))
    // Precedencies
    val seqPairs =  for {i <- 0 until nAct
                         j <- 0 until i
                         if randomBoolean(densityPrecedencies)} yield (i, j)
    val precPairs = seqPairs.toList
    // Resources
    //val nRes = randomInterval(0, nbRes)
    val nRes = nbRes
    val resources: Array[ResourceConstraint] = Array.tabulate(nRes) { ind =>
      // Capacity for resource i
      val resCap = randomInterval(minCapacity, maxCapacity)
      // Setup Times
      //val nRMs = randomInterval(minRMRes, maxRMRes)
      val nRMs = maxRMRes
      val initialMode = randomInterval(0, nRMs-1)
      // Activity usages
      val usageRes = for {i <- 0 until nAct
                          if randomBoolean(densityUsageRes)} yield (i, (randomInterval(1, resCap), randomInterval(0, nRMs-1)))
      val tupMaps = usageRes.map { tuple => ((tuple._1, tuple._2._1), (tuple._1, tuple._2._2)) }.unzip
      val mapUsedCaps = tupMaps._1.toMap
      val mapRMs = tupMaps._2.toMap
      val stSeq = for {i <- 0 until nRMs
                       j <- 0 until nRMs
                       if i != j} yield ((i,j), randomInterval(minSetupTimeRM, maxSetupTimeRM))
      val mapSetupTimes = stSeq.toMap
      val setupTimes = SetupTimes(initialMode, mapRMs, mapSetupTimes)
      if (resCap == 1L) {
        new DisjunctiveResourceWithSetupTimes(mapUsedCaps.keys, setupTimes)
      } else if (randomBoolean(densityMultiResources)) {
        new CumulativeMultiResourceWithSetupTimes(resCap, mapUsedCaps, setupTimes)
      } else {
        new CumulativeResourceWithSetupTimes(resCap, mapUsedCaps, setupTimes)
      }
    }
    new Schedule(m, durations, precPairs, resources)
  }

  def main(args: Array[String]): Unit = {
    // The CBLS store
    val model = Store(checker = None, noCycle=false)
    println(s"Generating problem...")
    val scProblem = createRandomProblem(model)
    val objFunc = Objective(scProblem.makeSpan)
    model.close()
    println("Model closed.")
    // Neighborhoods
    val swapNH = new SwapActivity(scProblem, "Swap")
    val reinsertNH = new ReinsertActivity(scProblem, "Reinsert")
    val combinedNH = BestSlopeFirst(List(Profile(reinsertNH), Profile(swapNH)))
    // This is the search strategy
    println(s"Initial list = ${scProblem.activitiesPriorList.value.toList}")
    println("Computing solution...")
    val t0 = System.nanoTime()
    combinedNH.verbose = 1
    combinedNH.doAllMoves(obj = objFunc)
    val t1 = System.nanoTime()
    println(combinedNH.profilingStatistics)
    // And here, the results
    println(s"*************** RESULTS ***********************************")
    println(s"Elapsed time : ${t1-t0} ns")
    println(s"Schedule makespan = ${scProblem.makeSpan.value}")
    println(s"Scheduling sequence = ${scProblem.activitiesPriorList.value.toList}")
    println("Scheduling start times = [  ")
    scProblem.startTimes.foreach(v => println(s"    $v"))
    println("]")
  }

}
