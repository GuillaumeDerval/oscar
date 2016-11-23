package oscar.modeling.misc

import oscar.modeling.misc.TimeHelper.getClockTime

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object ComputeTimeTaken {
  val results: ListBuffer[(String, String, Double)] = ListBuffer()

  def reset() = results.clear()

  def computeTimeTaken[T](name: String, category: String = "")(v: => T): T = {
    val t0 = getClockTime
    val r: T = v
    val t1 = getClockTime
    println("Time taken by " +name + "[" + category + "]: "+(t1.toDouble - t0.toDouble)/math.pow(10, 9) +" s")

    results += ((name, category, t1.toDouble-t0.toDouble))
    r
  }

  def showSummary() = {
    println("-------")
    println("Summary")
    val map = mutable.Map[String, Double]()
    println("-------")
    println("Name".padTo(20, ' ')+"\t\t"+"Category".padTo(20, ' ')+"\t\t"+"Time taken(s)".padTo(20, ' '))
    for((name, category, time) <- results) {
      println(name.padTo(20, ' ')+"\t\t"+category.padTo(20, ' ')+"\t\t"+(time/math.pow(10, 9)).toString.padTo(20, ' '))
      map += ((category, map.getOrElse(category, 0.0) + time))
    }
    println("-------")
    for((category, time) <- map) {
      println((" "*20)+"\t\t"+category.padTo(20, ' ')+"\t\t"+(time/math.pow(10, 9)).toString.padTo(20, ' '))
    }
    println("-------")
  }
}
