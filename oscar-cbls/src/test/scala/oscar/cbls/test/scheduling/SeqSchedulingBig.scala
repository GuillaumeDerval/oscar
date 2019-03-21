package oscar.cbls.test.scheduling

import oscar.cbls.algo.boundedArray.BoundedArray
import oscar.cbls.business.seqScheduling.model._
import oscar.cbls.business.seqScheduling.neighborhood.{ReinsertActivity, SwapActivity}
import oscar.cbls.core.computation.Store
import oscar.cbls.lib.search.combinators.{BestSlopeFirst, Profile}

import scala.util.Random

object SeqSchedulingBig {
  // Model
  val nbAct = 100
  val nbRes = 100
  val minDuration = 1
  val maxDuration = 25
  val minCapacity = 1
  val maxCapacity = 25
  val minRMRes = 0
  val maxRMRes = 100
  val densityUsageRes = 25
  val minSetupTimeRM = 0
  val maxSetupTimeRM = 25
  val densityPrecedencies = 5
  // Random Generator
  val randomGen = new Random()

  def randomBoolean(density: Int = 50): Boolean = {
    val rdVal = randomGen.nextInt(100)
    rdVal < density
  }

  def randomInterval(inf: Int, sup: Int): Int = {
    require(inf <= sup)
    val offset = if (inf == sup) 0 else randomGen.nextInt(sup - inf)
    inf + offset
  }

  def createRandomProblem(m: Store): SchedulingProblem_B = {
    // Resources
    //val nRes = randomInterval(0, nbRes)
    val nRes = nbRes
    val resources: Array[Resource_B] = new Array[Resource_B](nbRes)
    for { i <- 0 until nRes } {
      // Capacity for resource i
      val resCap = randomInterval(minCapacity, maxCapacity)
      // Running Modes for resource i
      //val nRMs = randomInterval(minRMRes, maxRMRes)
      val nRMs = maxRMRes
      val rms: Array[RunningMode_B] = new Array[RunningMode_B](nRMs)
      var withModes = WithModes_B()
      val indInitial = randomInterval(0, nRMs-1)
      for { j <- 0 until nRMs } {
        // Running Mode
        rms(j) = RunningMode_B(s"RM $j - Res $i")
        withModes = withModes.withMode(rms(j), j==indInitial)
        for { k <- 0 until j } {
          val seTime1 = randomInterval(minSetupTimeRM, maxSetupTimeRM)
          val seTime2 = randomInterval(minSetupTimeRM, maxSetupTimeRM)
          withModes = withModes.withSetupTime(rms(k), rms(j), seTime1)
          withModes = withModes.withSetupTime(rms(j), rms(k), seTime2)
        }
      }
      resources(i) = Resource_B(s"Resource $i", resCap, withModes)
    }
    // Activities and precedences
    //val nAct = randomInterval(1, nbAct)
    val nAct = nbAct
    val activities: Array[Activity_B] = new Array[Activity_B](nbAct)
    for { i <- 0 until nAct } {
      val actDur = randomInterval(minDuration, maxDuration)
      // Add Resource Usages
      var resUsed: Set[UsesResource_B] = Set()
      for { j <- 0 until nRes } {
        if (randomBoolean(densityUsageRes)) {
          val res = resources(j)
          val capRes = res.capacity
          val rmsRes = res.withModes.modes.toList
          // select mode
          val nbMode = randomInterval(0, rmsRes.size-1)
          val rm = rmsRes(nbMode)
          val usedCap = randomInterval(1, capRes)
          resUsed += UsesResource_B(res, usedCap, Some(rm))
        }
      }
      // Add Precedences
      var successors: Set[Activity_B] = Set()
      for { j <- 0 until i } {
        val actJ = activities(j)
        if (randomBoolean(densityPrecedencies)) {
          successors += actJ
        }
      }
      activities(i) = Activity_B(s"Activity $i", actDur, resUsed, Precedes_B(successors))
    }
    new SchedulingProblem_B(m, activities, resources)
  }

  def main(args: Array[String]): Unit = {
    // The CBLS store
    val model = Store(checker=None, noCycle=false)
    println(s"Generating problem...")
    val scProblem = createRandomProblem(model)
    model.close()
    println("Model closed.")
    // Neighborhoods
    val swapNH = new SwapActivity(scProblem, "Swap")
    val reinsertNH = new ReinsertActivity(scProblem, "Reinsert")
    val combinedNH = BestSlopeFirst(List(Profile(reinsertNH), Profile(swapNH)))
    // This is the search strategy
    println("Computing solution...")
    val t0 = System.nanoTime()
    combinedNH.verbose = 1
    combinedNH.doAllMoves(obj = scProblem.mkspObj)
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
    println(s"Number of setup changes in scheduling: ${scProblem.setupTimes.setupTimesList.length}")
  }

  /*
  val nbAct = 100
  val nbRes = 100
  val minDuration = 1
  val maxDuration = 25
  val minCapacity = 1
  val maxCapacity = 25
  val minRMRes = 0
  val maxRMRes = 100
  val densityUsageRes = 25
  val minSetupTimeRM = 0
  val maxSetupTimeRM = 25
  val densityPrecedencies = 5
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

  def createRandomProblem(m: Store): SchedulingProblem = {
    // Resources
    //val nRes = randomInterval(0, nbRes)
    val nRes = nbRes
    val resources = new BoundedArray[Resource](nRes, Resource.setIndex)
    for { i <- 0 until nRes } {
      // Capacity for resource i
      val resCap = randomInterval(minCapacity, maxCapacity)
      // Running Modes for resource i
      //val nRMs = randomInterval(minRMRes, maxRMRes)
      val nRMs = maxRMRes
      val runModesRes = new RunningModeResources(nRMs)
      for { j <- 0 until nRMs } {
        // Running Mode
        val rm = RunningMode(s"RM $j - Res $i")
        runModesRes.addRunningMode(rm)
        for { k <- 0 until j } {
          val rmk = runModesRes.getRunningModeAt(k)
          val seTime1 = randomInterval(minSetupTimeRM, maxSetupTimeRM)
          val seTime2 = randomInterval(minSetupTimeRM, maxSetupTimeRM)
          runModesRes.addSetupTime(rm, rmk, seTime1)
          runModesRes.addSetupTime(rmk, rm, seTime2)
        }
      }
      val res = new Resource(m, s"Resource $i", resCap, runModesRes)
      resources :+ res
    }
    // Activities and precedences
    //val nAct = randomInterval(1, nbAct)
    val nAct = nbAct
    val activities = new BoundedArray[Activity](nAct, Activity.setIndex)
    val precedences = new Precedences(nAct)
    val resourceUsages = new ActivityResourceUsages(nAct, nRes)
    for { i <- 0 until nAct } {
      val actDur = randomInterval(minDuration, maxDuration)
      val act = new Activity(m, s"Activity $i", actDur)
      activities :+ act
      // Add precedence
      for { j <- 0 until i } {
        val actJ = activities(j)
        if (randomBoolean(densityPrecedencies)) {
          precedences.addPrecedence(act, actJ)
        }
      }
      // Add Resource Usages
      for { j <- 0 until nRes } {
        val res = resources(j)
        val capRes = res.valCapacity
        val rmRes = res.getRunningModes
        for { rm <- rmRes } {
          if (randomBoolean(densityUsageRes)) {
            val usedCap = randomInterval(1, capRes)
            resourceUsages.addActivityResourceUsage(act, res, rm, usedCap)
          }
        }
      }
    }
    new SchedulingProblem(m, activities, resources, precedences, resourceUsages)
  }

  def main(args: Array[String]): Unit = {
    // The CBLS store
    val model = Store(checker = None, noCycle=false)
    println(s"Generating problem...")
    val scProblem = createRandomProblem(model)
    model.close()
    println("Model closed.")
    // Neighborhoods
    val swapNH = new SwapActivity(scProblem, "Swap")
    val reinsertNH = new ReinsertActivity(scProblem, "Reinsert")
    val combinedNH = BestSlopeFirst(List(Profile(reinsertNH), Profile(swapNH)))
    // This is the search strategy
    println("Computing solution...")
    val t0 = System.nanoTime()
    combinedNH.verbose = 1
    combinedNH.doAllMoves(obj = scProblem.mkspObj)
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
    println(s"Number of setup changes in scheduling: ${scProblem.setupTimes.setupTimesList.length}")
  }
  */
}
