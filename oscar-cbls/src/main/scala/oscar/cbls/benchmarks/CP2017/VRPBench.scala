package oscar.cbls.benchmarks.CP2017

/*******************************************************************************
  * OscaR is free software: you can redistribute it and/or modify
  * it under the terms of the GNU Lesser General Public License as published by
  * the Free Software Foundation, either version 2.1 of the License, or
  * (at your option) any later version.
  *
  * OscaR is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU Lesser General Public License  for more details.
  *
  * You should have received a copy of the GNU Lesser General Public License along with OscaR.
  * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
  ******************************************************************************/

import java.io.{File, PrintWriter}

import oscar.cbls.business.routing.model._
import oscar.cbls.business.routing.neighborhood._
import oscar.cbls.core.computation.Store
import oscar.cbls.core.objective.Objective
import oscar.cbls.lib.invariant.seq.Size
import oscar.cbls.lib.search.combinators.{BestSlopeFirst, Profile}
import oscar.cbls.modeling.Algebra._
import oscar.cbls.util.StopWatch

import scala.io.Source

class MySimpleRoutingWithUnroutedPoints(n:Int,v:Int,symmetricDistance:Array[Array[Int]],m:Store, maxPivot:Int)
  extends VRP(n,v,m,maxPivot)
  with TotalConstantDistance with ClosestNeighbors with RoutedAndUnrouted{
//setAsymmetricDistanceMatrix(symmetricDistance)
  setSymmetricDistanceMatrix(symmetricDistance)

  override protected def getDistance(from : Int, to : Int) : Int = symmetricDistance(from)(to)

  val penaltyForUnrouted  = 10000

  val obj = Objective(totalDistance + (penaltyForUnrouted*(n - Size(routes))))

  this.addToStringInfo(() => "objective: " + obj.value)
  this.addToStringInfo(() => "n:" + n + " v:" + v)

  val closestNeighboursForward = computeClosestNeighborsForward()

  def size = routes.value.size
}

object TSProutePoints extends App {

  def printMatrix(m:Array[Array[Int]]){
    println(m.map(l => l.mkString(" ")).mkString("\n"))
  }

  def benchmarkOnProblem(fileName:String){
    warmUp(1000)
    val matrix = loadMatrixFromFile(fileName:String)
    val n = matrix.length
    val v = 1
    val percent = 3
    println("benchmarking " + fileName)
    print("n\tv\tpercent\ttime")
    print(n + "\t" + v + "\t" + percent + "\t")
    new TSPRoutePointsS(n, v, percent, 0, matrix)
  }

  def warmUp(n:Int = 10000){
    val verbose = 0
    val maxPivotPerValuePercent = 4
    val v = 100
    val symmetricDistanceMatrix = RoutingMatrixGenerator(n)._1
    new TSPRoutePointsS(n, v, maxPivotPerValuePercent, verbose, symmetricDistanceMatrix)
    System.gc()
  }

  def performRandomBenchmark() {
    warmUp()

    val nbTrials = 10

    println()
    print("n\tv\tpercent")
    for (t <- 1 to nbTrials) {
      print("\ttime")
    }
    println


    for (n <- 11000 to 11000 by 2000) {
      for (v <- List(100)) {
        for (maxPivotPerValuePercent <- List(0, 1, 2, 3, 4, 5, 20)) {
          print(n + "\t" + v + "\t" + maxPivotPerValuePercent + "\t")
          for (t <- 1 to nbTrials) {
            val symmetricDistanceMatrix = RoutingMatrixGenerator(n)._1
            new TSPRoutePointsS(n, v, maxPivotPerValuePercent, 0, symmetricDistanceMatrix)
            print("\t")
            System.gc()
          }
          println
        }
      }
    }
  }

  def loadMatrixFromFile(filename:String):Array[Array[Int]] = {
    val file = Source.fromFile(filename)
    val words: Array[String] = file.mkString.split("\\s+")
    file.close()
    val reader = words.iterator

    val n = reader.next().toInt

    val matrix = Array.tabulate(n)(_ => Array.fill(n)(-1))

    for(i <- 0 until n){
      for(j <- 0 until n){
        matrix(i)(j) = reader.next().toInt
      }
    }
    require(!reader.hasNext)
    matrix
  }

  def writeMatrix(writer:PrintWriter,matrix:Array[Array[Int]]){
    val n = matrix.length
    for(i <- 0 until n){
      for(j <- 0 until n){
        writer.write(matrix(i)(j) + " ")
      }
      writer.write("\n")
    }
  }

  def saveMatrixToFile(fileName:String,matrix:Array[Array[Int]]){
    val writer = new PrintWriter(new File(fileName))
    writer.write(matrix.length + "\n")
    writeMatrix(writer,matrix)
    writer.close()
  }

  def saveMatrixToLocalSolverFile(fileName:String,matrix:Array[Array[Int]]){
    val writer = new PrintWriter(new File(fileName))
    val n = matrix.length
    writer.write("NAME: RANDOM" + fileName + "\n")
    writer.write("TYPE: TSP\n")
    writer.write("COMMENT: " + n + " city problem\n")
    writer.write("DIMENSION: " + n + "\n")
    writer.write("EDGE_WEIGHT_TYPE: EXPLICIT\n")
    writer.write("EDGE_WEIGHT_FORMAT: FULL_MATRIX\n")
    writer.write("EDGE_WEIGHT_SECTION\n")
    writeMatrix(writer,matrix)
    writer.write("EOF\n")
    writer.close()
  }

  def generateAllBenchmarks(){
    for(n <- benchmarkSizes){
      println("generating TSP n:" + n + " to file:" + fileName)
      val symmetricDistanceMatrix = RoutingMatrixGenerator(n)._1
      saveMatrixToFile(fileName + n + ".bench",symmetricDistanceMatrix)
      saveMatrixToLocalSolverFile(fileName + "_localSolver_" + n + ".tsp",symmetricDistanceMatrix)
    }
  }

  def runAllBenchmarks(){
    new TSPRoutePointsS(1000, 1, 3, 0, RoutingMatrixGenerator(1000)._1)
    println()
    print("n\ttime\tobj")
    println

    for(n <- benchmarkSizes){
      print(n + "\t")
      val matrix = loadMatrixFromFile(fileName + n + ".bench")
      new TSPRoutePointsS(n, 1, 3, 0, matrix,true)
      print("\n")
      System.gc()
    }
  }

  def runBenchmark(fileName:String,n:Int){
    new TSPRoutePointsS(1000, 100, 3, 0, RoutingMatrixGenerator(1000)._1)

    println()
    print("n\ttime\tobj")
    println
      print(n + "\t")
      val matrix = loadMatrixFromFile(fileName + n + ".bench")
      new TSPRoutePointsS(n, 1, 4, 0, matrix,true)
      print("\n")
      System.gc()
  }

  val benchmarkSizes = 500 to 5000 by 500
  val fileName = "C:\\Users\\rdl\\Documents\\Oscar\\BitBucket3\\oscar-cbls\\src\\main\\examples\\oscar\\examples\\cbls\\routing\\data\\bench"
  runBenchmark(fileName,1000)
 // generateAllBenchmarks()
  //runAllBenchmarks()
}

class TSPRoutePointsS(n:Int,v:Int,maxPivotPerValuePercent:Int, verbose:Int, symmetricDistanceMatrix:Array[Array[Int]],printobj:Boolean = false) extends StopWatch{

  startWatch()
  //  println("restrictions:" + restrictions)
  val model = new Store() //checker = Some(new ErrorChecker()))

  val myVRP = new MySimpleRoutingWithUnroutedPoints(n,v,symmetricDistanceMatrix,model,maxPivotPerValuePercent)
  val nodes = myVRP.nodes

  model.close()

  val bestInsert = false

  val routeUnroutdPoint =  Profile(new InsertPointUnroutedFirst(myVRP.unrouted,()=>myVRP.kFirst(10,myVRP.closestNeighboursForward,myVRP.isRouted), myVRP,best=bestInsert,neighborhoodName = "InsertUF"))

  //TODO: using post-filters on k-nearest is probably crap
  val routeUnroutdPoint2 =  Profile(new InsertPointRoutedFirst(myVRP.routed,()=>myVRP.kFirst(10,myVRP.closestNeighboursForward,x => !myVRP.isRouted(x)),myVRP,best=bestInsert,neighborhoodName = "InsertRF")  guard(() => myVRP.size < n/2))

  def onePtMove(k:Int) = Profile(new OnePointMove(myVRP.routed, () => myVRP.kFirst(k,myVRP.closestNeighboursForward,myVRP.isRouted), myVRP))

  val twoOpt = Profile(new TwoOpt1(myVRP.routed, ()=>myVRP.kFirst(20,myVRP.closestNeighboursForward,myVRP.isRouted), myVRP))

  def threeOpt(k:Int, breakSym:Boolean) = Profile(new ThreeOpt(myVRP.routed, ()=>myVRP.kFirst(k,myVRP.closestNeighboursForward,myVRP.isRouted), myVRP,breakSymmetry = breakSym, neighborhoodName = "ThreeOpt(k=" + k + ")"))

  val search = (BestSlopeFirst(List(routeUnroutdPoint2, routeUnroutdPoint, onePtMove(10),twoOpt, threeOpt(10,true))) exhaust threeOpt(20,true))

  // val search = (new RoundRobin(List(routeUnroutdPoint2,onePtMove(10) guard (() => myVRP.unrouted.value.size != 0)),10)) exhaust BestSlopeFirst(List(onePtMove(20),twoOpt, threeOpt(10,true))) exhaust threeOpt(20,true)

  search.verbose = verbose
  //search.verboseWithExtraInfo(1, ()=> "" + myVRP)

  search.doAllMoves(obj=myVRP.obj)

  print(getWatch)
  if(printobj){
    print("\t" + myVRP.obj.value)
  }
}
