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
/**
 * @author Gustav Björdal
 */
package oscar.flatzinc.cbls
import java.io.File
import java.io.FileWriter
import java.io.PrintWriter
import oscar.flatzinc.Options
import java.util.Locale

object Benchmark extends App{

	def benchmark(path: String, filename: String, latexFile: PrintWriter, nbRuns: Int, timeOut: Int): String = {
    val file = Array("minizinc/" + path + "/" + filename);
    val outputFile = new File("minizinc/" + path + "/" + filename + ".output." + System.currentTimeMillis + ".txt");
    if (!outputFile.exists())
      outputFile.createNewFile();
    val fw = new FileWriter(outputFile, true);
    val pw = new PrintWriter(fw);
    var runtimes = List.empty[Long];
    var objectives = List.empty[Int];
    var iterations = List.empty[Int];
    var numOfFailures = 0;
    for (i <- 1 to nbRuns) {
      pw.println("Run " + i)
      val opts = new Options("",true,file)
      opts.timeOut = timeOut;

      val solutions = new FZCBLSSolver().solve(opts)
/*
      for ((runtime, objective, iteration, solution) <- solutions) {
        pw.println("Runtime: " + runtime + " Objective: " + objective + " Iterations: " + iteration)
        pw.println(solution);
      }
      if (solutions.length == 1) {
        numOfFailures = numOfFailures+1;
      } else {
        val best = solutions(1);
        runtimes +:= best._1;
        objectives +:= best._2;
        iterations +:= best._3;
      }*/
      pw.println("####################")
      pw.flush();
    }
    var latexString = filename + "&\t" + //Filename
      (nbRuns - numOfFailures) * nbRuns;
    runtimes = runtimes.sortWith(_ < _)
    objectives = objectives.sortWith(_ < _)
    iterations = iterations.sortWith(_ < _)
    def outputStatistics(l: List[Double]): Unit = {
      if (l.length == 0) {
        latexString = latexString + "&\t-&\t-&\t-&\t-&\t";
        return
      }
      val data = l;
      val minVal = data(0);
      val maxVal = data.last;
      val median = data(data.length / 2);
      val average = (data.foldLeft(0: Double)(_ + _).toDouble / data.length).toDouble
      def sq(x: Double): Double = { x * x }
      val deviation = Math.sqrt(data.foldLeft(0: Double)((acc, x) => acc + sq(x.toDouble - average)) / data.length)
      pw.println("Median: " + median +
        " Average: " + average + " Deviation: " + deviation + " Min: " + minVal + " Max: " + maxVal)
      latexString = latexString + "&\t" + "%.2f".formatLocal(Locale.ENGLISH, median) + "&\t" +
        "%.2f".formatLocal(Locale.ENGLISH, average) + "&\t" +
        "%.2f".formatLocal(Locale.ENGLISH, minVal) + "&\t" +
        "%.2f".formatLocal(Locale.ENGLISH, maxVal) + "&\t" +
        "%.2f".formatLocal(Locale.ENGLISH, deviation);
    }
    pw.println("Runtime")
    outputStatistics(runtimes.map(_.toDouble / 1000))
    pw.println("Objective")
    outputStatistics(objectives.map(_.toDouble))
    pw.println("Iterations")
    outputStatistics(iterations.map(_.toDouble))
    pw.println("Failures: " + numOfFailures)
    pw.flush();
    pw.close();
    latexFile.println(latexString + "\\\\")
    println("Done with " + filename)
    "Done with " + filename
  } /*
  for (i <- 0 to 0) {
    //val file = Array("minizinc/firstfile.fzn")
    //val file = Array("minizinc/benchmark/gridcoloring-15-16.fzn")
    //val file = Array("minizinc/benchmark/2010/costas-array_14.fzn")
    val file = Array("minizinc/benchmark/2010/costas-array_16.fzn")
    //val file = Array("minizinc/benchmark/2010/grid_colouring_15_16.fzn")
    //val file = Array("minizinc/benchmark/2010/depot_placement_a280_4.fzn")
    //val file = Array("minizinc/benchmark/2010/depot_placement_22_6.fzn")
    //val file = Array("minizinc/benchmark/2010/bacp_27.fzn")
    //val file = Array("minizinc/sudoku.fzn")
    //val file = Array("minizinc/4096queens.fzn")
    //val file = Array("minizinc/file3.fzn")
    //val file = Array("minizinc/benchmark/2010/sugiyama_g3_8_8_2.fzn")
    //val file = Array("minizinc/benchmark/2010/sugiyama_g5_7_7_7_7_2.fzn")
    //val file = Array("minizinc/benchmark/2011/fillomino_17.fzn")
    //val file = Array("minizinc/benchmark/2011/fillomino_15.fzn")
    //val file = Array("minizinc/benchmark/2011/open_stacks_30_15_1.fzn") //unbound domain variables
    //val file = Array("minizinc/benchmark/2011/prize_collecting_32_4_8_0.fzn")
    //val file = Array("minizinc/benchmark/2011/ship-schedule_5_mixed.fzn")
    //val file = Array("minizinc/benchmark/2011/solbat_12_12_5_3.fzn")
    //val file = Array("minizinc/benchmark/2011/table-layout_en-760-310-line795.fzn")
    //val file = Array("minizinc/benchmark/2011/vpr_A-n32-k5.vrp.fzn")
    //val file = Array("minizinc/benchmark/2012/radiation_m06.fzn")

    val opts = new Options(file)
    solve(opts)
  }*/

  val modelPaths = Array(
      //"benchmark/2010/grid-colouring/fzn",
      "benchmark/2010/costas-array/fzn",
      "benchmark/2010/sugiyama/fzn",
      "benchmark/2010/bacp/fzn",
      "benchmark/2010/depot-placement/fzn",
      "benchmark/2010/filters/fzn",
      //"benchmark/2010/ghoulomb/fzn",
      "benchmark/2010/solbat/fzn"
      );
  for (modelPath <- modelPaths) {
    var allBenchmarks: List[String] = List.empty[String]

    val f = new File("minizinc/" + modelPath);
    val latexFile = new File("minizinc/" + modelPath + "/latexData.tex");
    if (!latexFile.exists())
      latexFile.createNewFile()
    val latexWriter = new FileWriter(latexFile, true);
    val latexPWriter = new PrintWriter(latexWriter);
    for (x <- f.listFiles().filter(_.getName().endsWith(".fzn"))) {
      allBenchmarks +:= benchmark(modelPath, x.getName(), latexPWriter, 5, 60)
      latexPWriter.flush();
    }
    latexPWriter.close();
    for (s <- allBenchmarks)
      println(s)
  }
}