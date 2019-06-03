package oscar.cbls.algo.pareto

import oscar.util.RandomGenerator

/**
  * @author Cyrille Dejemeppe cyrille.dejemeppe@gmail.com
  * @author Renaud Hartert ren.hartert@gmail.com
  */
abstract class QTNode[U: Numeric]{


  def coordinates: Array[U]

  def objectives: Array[Double]

  val nObjectives = objectives.length

  val nCoordinates = coordinates.length

  def comp(a: Double, b: Double): Boolean = a < b

  /** Returns an integer +1 if this dominates other, -1 if other dominates this or
    *  0 if other and this don't dominate each other */
  def dominance(other: QTNode[U]): Int = {
    def areEquals(index: Int): Boolean = {
      if (index >= this.objectives.length) true
      else if (this.objectives(index) != other.objectives(index)) false
      else areEquals(index + 1)
    }
    def dominanceAux(index: Int, dom: Boolean, nDom: Boolean): Int = {
      if (index >= this.objectives.length) {
        if (dom && nDom) return 0
        else if (dom) return 1
        else return -1
      }
      if (this.comp(this.objectives(index), other.objectives(index))) {
        if (nDom) return 0
        else dominanceAux(index + 1, true, nDom)
      }
      else if (this.objectives(index) == other.objectives(index)) {
        dominanceAux(index + 1, dom, nDom)
      }
      else {
        if (dom) return 0
        else dominanceAux(index + 1, dom, true)
      }
    }
    if (areEquals(0)) 0
    else dominanceAux(0, false, false)
  }


  val nSuccessors = 1 << nObjectives
  val Successors = 0 until nSuccessors
  val NonDomSuccessors = 1 until nSuccessors - 1
  val bestSuccessor = Successors.max
  val worstSuccessor = Successors.min
  val successors: Array[Option[QTNode[U]]] = Array.fill(nSuccessors)(None)

  var father: Option[QTNode[U]] = None
  var kSucc: Int = -1

  def successorsToSet: Set[QTNode[U]] = {
    successors.filter(s => s match {
      case Some(suc) => true
      case None => false
    }).map(succ => succ.get).toSet
  }

  def detach() { // Cannot be applied on root
    val f = father.get
    f.successors(kSucc) = None
    father = None
    kSucc = -1
  }

  def successorship(k: Array[Double]): Int = successorship0(k, 0, 0, false, false)

  private def successorship0(k: Array[Double], d: Int, kSucc: Int, dom: Boolean, ndom: Boolean): Int = {
    if (d == nObjectives) if (dom && !ndom) bestSuccessor else kSucc
    else if (comp(k(d), objectives(d))) successorship0(k, d + 1, (kSucc << 1) + 1, true, ndom)
    else if (k(d) == objectives(d)) successorship0(k, d + 1, kSucc << 1, dom, ndom)
    else successorship0(k, d + 1, kSucc << 1, dom, true)
  }

  override def toString: String = objectives.mkString("QTNode(", ", ", ")")
}


/**
  *
  * maps a multi-dimentional key onto a value, keeps only Pareto-optimal minimizing keys
  *
  * @author Renaud Hartert ren.hartert@gmail.com
  * @author Cyrille Dejemeppe cyrille.dejemeppe@gmail.com
  */
class ParetoMap[U: Numeric](private val nDim: Int, private val cmp: (Int, Int) => Boolean)
  extends Traversable[QTNode[U]] {

  val nSuccessors = 1 << nDim
  val Successors = 0 until nSuccessors
  val NonDomSuccessors = 1 until nSuccessors - 1
  val bestSuccessor = Successors.max
  val worstSuccessor = Successors.min

  /** Successor functions */

  def opposite(kSucc: Int): Int = kSucc ^ bestSuccessor

  def asStrong(kSucc: Int): IndexedSeq[Int] = {
    (kSucc to NonDomSuccessors.max).filter(s => (s - (s ^ kSucc)) == kSucc)
  }

  def stronger(kSucc: Int): IndexedSeq[Int] = {
    (kSucc + 1 to NonDomSuccessors.max).filter(s => (s - (s ^ kSucc)) == kSucc)
  }

  def asWeak(kSucc: Int): IndexedSeq[Int] = {
    val dim = opposite(kSucc)
    (NonDomSuccessors.min to kSucc).filter(s => ((s ^ dim) - s) == dim)
  }

  def weaker(kSucc: Int): IndexedSeq[Int] = {
    val dim = opposite(kSucc)
    (NonDomSuccessors.min until kSucc).filter(s => ((s ^ dim) - s) == dim)
  }

  /** QuadTree functions */
  var root: Option[QTNode[U]] = None

  def process(cand: QTNode[U], subR: QTNode[U]) {

    val kSucc = subR.successorship(cand.objectives)

    // Discard the candidate node
    if (kSucc == worstSuccessor) {}

    // Replace the root by the dominated node
    else if (kSucc == bestSuccessor) {
      replace(cand, subR)
      for (son <- NonDomSuccessors if cand.successors(son).isDefined) {
        clean(cand, cand.successors(son).get)
      }
    } // Dominance + Clean
    else {
      // Check dominance
      for (son <- stronger(kSucc) if subR.successors(son).isDefined) {
        val dom = checkDominance(cand, subR.successors(son).get)
        if (dom) return
      }
      // Remove dominated node
      val weak = weaker(kSucc)
      for (son <- weak if subR.successors(son).isDefined) {
        clean(cand, subR.successors(son).get)
      }

      if (subR.successors(kSucc).isDefined) {
        process(cand, subR.successors(kSucc).get)
      } else {
        insert0NoCheck(cand, subR)
      }
    }
  }

  private def insert0NoCheck(cand: QTNode[U], root: QTNode[U]) {
    val kSucc = root.successorship(cand.objectives)
    // Recursive traversal
    if (root.successors(kSucc).isDefined) {
      insert0NoCheck(cand, root.successors(kSucc).get)
    } // Insertion
    else {
      cand.father = Some(root)
      cand.kSucc = kSucc
      root.successors(kSucc) = Some(cand)
    }
  }

  /** Check if the candidate node is dominated by some node in the tree rooted at root */

  def checkDominance(cand: QTNode[U], root: QTNode[U]): Boolean = {
    val kSucc = root.successorship(cand.objectives)
    // Is the candidate node dominated by the root ?
    if (kSucc == worstSuccessor) true
    // Is the candidate node dominated by a subtree ?
    else {
      val sons = asStrong(kSucc)
      for (son <- sons if root.successors(son).isDefined) {
        if (checkDominance(cand, root.successors(son).get)) return true
      }
      false // Not dominated by any node in the tree rooted at root
    }
  }

  /** Replace the root */

  def replace(cand: QTNode[U], root: QTNode[U]) {
    // Transplant
    if (root == this.root.get) {
      this.root = Some(cand)
    } else {
      val father = root.father.get
      father.successors(root.kSucc) = Some(cand)
      cand.father = Some(father)
      cand.kSucc = root.kSucc
    }
    // Reinsert sons
    for (son <- NonDomSuccessors if root.successors(son).isDefined) {
      reinsertIn(root.successors(son).get, this.root.get)
    }
  }

  /** Reinsert without dominance check all the subtree rooted at root in inNode */

  def reinsertIn(root: QTNode[U], inNode: QTNode[U]) {
    root.detach
    for (son <- NonDomSuccessors if root.successors(son).isDefined) {
      reinsertIn(root.successors(son).get, inNode)
    }
    insert0NoCheck(root, inNode)
  }

  /** Remove nodes that are dominated by the candidate node */

  def clean(cand: QTNode[U], root: QTNode[U]) {
    val kSucc = root.successorship(cand.objectives)
    // Is the root dominated by the candidate node ?
    if (kSucc == bestSuccessor) {
      val newRoot = deleteAndRepair(root)
      if (newRoot.isDefined) {
        clean(cand, newRoot.get)
      }
    } // Is the candidate node dominated by a subtree ?
    else {
      val sons = asWeak(kSucc)
      for (son <- sons if root.successors(son).isDefined) {
        clean(cand, root.successors(son).get)
      }
    }
  }

  def deleteAndRepair(root: QTNode[U]): Option[QTNode[U]] = {

    // Search first son
    var son = 1
    while (!root.successors(son).isDefined && son < bestSuccessor) {
      son += 1
    }

    // First son replace its father
    if (son < bestSuccessor) {
      val newRoot = root.successors(son).get
      newRoot.detach() // prevents newRoot to be reinserted into newRoot

      // Reinsert
      if (root == this.root.get) {
        this.root = Some(newRoot)
      }
      else {
        val father = root.father.get
        father.successors(root.kSucc) = Some(newRoot)
        newRoot.father = Some(father)
        newRoot.kSucc = root.kSucc
      }
      // Reinsert sons
      for (son <- NonDomSuccessors if root.successors(son).isDefined) {
        reinsertIn(root.successors(son).get, newRoot)
      }

      return Some(newRoot)

    } else {
      root.detach()

      return None
    }
  }

  override def foreach[T](f: QTNode[U] => T): Unit = {
    def forEach0(root: QTNode[U]) {
      f(root)
      for (son <- NonDomSuccessors if root.successors(son).isDefined) {
        forEach0(root.successors(son).get)
      }
    }
    if (root.isDefined) forEach0(root.get)
  }

  def getEvaluations: Set[Array[Double]] = {
    val qtNodeSet = this.toSet
    qtNodeSet.map((e: QTNode[U]) => e.objectives)
  }

  def insert(cand: QTNode[U]) {
    if (root.isDefined) process(cand, root.get)
    else {
      root = Some(cand)
    }
  }

  def randomElement: QTNode[U] = {
    val qtNodeList = this.toList
    val randIndex = RandomGenerator.nextInt(qtNodeList.length)
    qtNodeList(randIndex)
  }

  def score[T1 <: QTNode[U]](candidate: T1): Int = {
    var acc = 0
    for (elem <- this.toSeq) {
      val domi = candidate.dominance(elem)
      if (domi > 0) acc += 1
      else if (domi < 0) acc -= 1
    }
    acc
  }

  def contains[T1 <: QTNode[U]](elem: T1): Boolean = {
    for (element <- this.toSeq) {
      if (elem.objectives == element.objectives) return true
    }
    false
  }
}

object ParetoQTTree {
  def apply[U: Numeric](nDim: Int, comp: (Int, Int) => Boolean) = new ParetoMap(nDim, comp)
}

