package oscar.cp.perf


import oscar.cp._
import oscar.cp.constraints.tables.TableAlgo
import oscar.util.InFile

import scala.util.Random

/**
 * Eternity Problem = Edge Matching Puzzle
 * A piece has a square shape with a color on each side.
 * The objective is to place all the (n x m) pieces on the n x m board such that two adjacent sides have the same color
 * @author Pierre Schaus pschaus@gmail.com
 */
object Eternity extends CPModel with App {

  val reader = new InFile("data/eternity8x8.txt");

  val nCols = reader.nextInt()
  val nRows = reader.nextInt()
  val nTiles = nCols * nRows
  val pieces = Array.tabulate(nTiles)(i => Array.fill(4)(reader.nextInt()).drop(0));

  val minColor = pieces.flatten.min
  val maxColor = pieces.flatten.max

  // create the variables
  val id = Array.fill(nCols, nRows)(CPIntVar.sparse(0, nTiles))
  val up = Array.fill(nCols, nRows)(CPIntVar.sparse(minColor, maxColor))
  val right = Array.fill(nCols, nRows)(CPIntVar.sparse(minColor, maxColor))
  val down = Array.fill(nCols, nRows)(CPIntVar.sparse(minColor, maxColor))
  val left = Array.fill(nCols, nRows)(CPIntVar.sparse(minColor, maxColor))

  // horizontal match on adjacent cells
  for (l <- 0 until nCols; c <- 0 until nRows - 1) {
    add(right(l)(c) === left(l)(c + 1))
  }

  // vertical match on adjacent cells
  for (l <- 0 until nCols - 1; c <- 0 until nRows) {
    add(down(l)(c) === up(l + 1)(c))
  }
  
  // make the link between id, orientation and up variable
  val tableData = for (i <- 0 until nTiles; r <- 0 until 4) yield {
    val row = pieces(i)
    Array(i, row((r + 0) % 4), row((r + 1) % 4), row((r + 2) % 4), row((r + 3) % 4))
  }
  
  for (i <- 0 until nCols; j <- 0 until nRows) {
    add(table(Array(id(i)(j), up(i)(j), right(i)(j), down(i)(j), left(i)(j)), tableData.toArray, TableAlgo.CompactTable));
  }

  add(allDifferent(id.flatten))

  // force 0 on horizontal borders
  for (c <- 0 until nRows) {
    add(up(0)(c) === 0)
    add(down(nCols - 1)(c) === 0)
  }

  // force 0 on vertical borders
  for (l <- 0 until nCols) {
    add(left(l)(0) === 0)
    add(right(l)(nRows - 1) === 0)
  }

  val fId = id.flatten
  val fUp = up.flatten
  val fRight = right.flatten
  val fDown = down.flatten
  val fLeft = left.flatten
  val rand = new Random()
  

  search {
      binaryFirstFail(fId) ++
      binaryFirstFail(fUp) ++
      binaryFirstFail(fRight) ++
      binaryFirstFail(fDown)
  }
  
  val stat = start(nSols = 1)
  println(stat)
}
