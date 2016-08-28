package oscar.examples.cp

import oscar.cp._
import oscar.util._
import oscar.visual.VisualFrame
import oscar.algo.search.VisualSearchTree
import oscar.util.tree.Tree
import oscar.visual.VisualGrid
import java.awt.Color

/**
 * n-queens model: place n-queens on a chess-board such that they don't attack each other.
 * this program search for all the solutions
 * Using Non Deterministic Search
 *
 * @author Pierre Schaus pschaus@gmail.com
 */
object QueensVisu extends CPModel with App {

  val nQueens = 8 // Number of queens
  val Queens = 0 until nQueens

  // Variables
  val queens = Array.fill(nQueens)(CPIntVar(Queens))
  

  val tree = new Tree()
  // -----------------------------------------------
  val f = new VisualFrame("n-Queens", 1, 2)
  val w1 = f.createFrame("Tree")
  val vt = new VisualSearchTree(tree)
  w1.add(vt)
  w1.pack()

  val w2 = f.createFrame("Queens")
  val vg = new VisualGrid(nQueens, nQueens)
  w2.add(vg)
  w2.pack()

  def updateVisu(doms: Seq[Set[Int]]) {
    for (q <- Queens; qv <- Queens) {
      val col = if (doms(q).contains(qv)) Color.green else Color.red
      vg(q)(qv).innerCol = col
    }
  }
  // -------------------------------------------------
  var currNode = 0

  onSolution { tree.addSuccess(currNode) }

  add(allDifferent(queens) /*,Strong*/ )
  add(allDifferent(for (i <- Queens) yield queens(i) + i) /*,Strong*/ )
  add(allDifferent(for (i <- Queens) yield queens(i) - i) /*,Strong*/ )

  search {
    selectMin(Queens)(i => !queens(i).isBound)(i => i) match {
      case None => noAlternative
      case Some(i) => {
        val x = queens(i)
        val parent = currNode
        val v = x.min
        branch {
          post(x === v)
          currNode += 1
          val doms = queens.map(_.toSet)
          tree.createBranch(parent, currNode, currNode.toString, "left") {
            updateVisu(doms)
          }
        } {
          post(x !== v)
          currNode += 1
          val doms = queens.map(_.toSet)
          tree.createBranch(parent, currNode, currNode.toString, "right") {
            updateVisu(doms)
          }
        }
      }
    }
  }

  val stats = start(nSols = 3)
  vt.update()

  //print some statistics
  println(stats)
  
  
}
