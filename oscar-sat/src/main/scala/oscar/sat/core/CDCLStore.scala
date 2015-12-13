package oscar.sat.core

import oscar.algo.array.ArrayQueue
import oscar.algo.array.ArrayStack
import oscar.algo.array.ArrayStackInt
import oscar.algo.array.ArrayStackDouble
import oscar.sat.constraints.clauses.Clause
import oscar.sat.heuristics.Heuristic
import oscar.sat.constraints.Constraint
import oscar.algo.array.ArrayQueueInt
import oscar.sat.constraints.nogoods.Nogood

/** @author Renaud Hartert ren.hartert@gmail.com */

class CDCLStore {

  protected var heuristic: Heuristic = null

  // Clauses and literals
  protected[this] val constraints: ArrayStack[Constraint] = new ArrayStack(128)
  private[this] val learntClauses: ArrayStack[Nogood] = new ArrayStack(128)

  // Activity and Decay
  private final val scaleLimit: Double = 1000000
  protected var activityStep: Double = 0.5
  protected var activityDecay: Double = 0.5
  protected var variableStep: Double = 0.5
  protected var variableDecay: Double = 0.5

  // Watchers of each literal used by clause and nogoods
  private[this] val watchers: ArrayStack[Watchers] = new ArrayStack(128)

  // Trailing queue  
  private[this] val trail: ArrayStackInt = new ArrayStackInt(100)
  private[this] val trailLevels: ArrayStackInt = new ArrayStackInt(100)

  // Propagation queue
  private[this] val queue: ArrayQueueInt = new ArrayQueueInt(128)

  // Variables structure
  private[this] var values: Array[LiftedBoolean] = new Array(128)
  private[this] var reasons: Array[Constraint] = new Array(128)
  private[this] var levels: Array[Int] = new Array(128)
  private[this] var activities: Array[Double] = new Array(128)
  protected var varStoreSize: Int = 0

  /** Returns the clause responsible of the assignment */
  @inline final def assignReason(varId: Int): Constraint = reasons(varId)

  @inline final def isAssigned(varId: Int): Boolean = values(varId) != Unassigned

  @inline final def isTrue(varId: Int): Boolean = values(varId) == True

  @inline final def isFalse(varId: Int): Boolean = values(varId) == False

  @inline final def varActivity(varId: Int): Double = activities(varId)
  
  @inline final def level(varId: Int): Int = levels(varId)

  final def newVar(): Int = {
    if (varStoreSize == values.length) growVariableStore()
    val varId = varStoreSize
    values(varId) = Unassigned
    reasons(varId) = null
    levels(varId) = -1
    activities(varId) = 0.0
    varStoreSize += 1
    watchers.append(new Watchers(16))
    watchers.append(new Watchers(16))
    varId
  }

  @inline final def watch(clause: Clause, literal: Int): Unit = {
    watchers(literal).enqueue(clause)
  }

  private def recordNogood(): Boolean = {
    assert(outLearnt != null)
    assert(outLearnt.length > 0)
    if (outLearnt.length == 1) enqueue(outLearnt(0), null) // Unit fact
    else {
      // Allocate nogood
      val nogood = Nogood(this, outLearnt)
      learntClauses.append(nogood)
      nogood.setup()
    }
  }

  final def newClause(literals: Array[Int]): Boolean = {

    // check for initial satisfiability

    if (literals.length == 0) true
    else if (literals.length == 1) enqueue(literals(0), null) // Unit fact
    else {
      // Allocate clause
      val clause = Clause(this, literals)
      constraints.append(clause)
      watchers(literals(0) ^ 1).enqueue(clause)
      watchers(literals(1) ^ 1).enqueue(clause)
      true
    }
  }

  /**
   *  Empty the propagation queue
   *  Return the inconsistent clause if any
   */
  final def propagate(): Constraint = {
    var failReason: Constraint = null
    while (!queue.isEmpty && failReason == null) {
      val literal = queue.removeFirst()
      val constraints = watchers(literal)
      val nConstrains = constraints.length
      var i = nConstrains
      while (i > 0 && failReason == null) {
        i -= 1
        val constraint = constraints.dequeue()
        val consistent = constraint.propagate(literal)
        if (!consistent) failReason = constraint
      }
    }
    queue.clear()
    failReason
  }

  final def enqueue(litId: Int, from: Clause): Boolean = {
    val varId = litId / 2
    val value = values(varId)
    val unsigned = (litId & 1) == 0
    if (value != Unassigned) {
      if (unsigned) value == True
      else value == False
    } else {
      // new fact to store
      if (unsigned) values(varId) = True
      else values(varId) = False
      levels(varId) = trailLevels.size
      reasons(varId) = from
      trail.push(litId)
      queue.addLast(litId)
      true
    }
  }

  final def litValue(literal: Int): LiftedBoolean = {
    val assigned = values(literal / 2)
    if (assigned == Unassigned) Unassigned
    else if ((literal & 1) == 1) assigned.opposite
    else assigned
  }

  final def claBumpActivity(nogood: Nogood): Unit = {
    nogood.activity += activityStep
    if (nogood.activity >= scaleLimit) {
      varRescaleActivity()
    }
  }

  final def claRescaleActivity(): Unit = {
    var i = 0
    while (i < learntClauses.length) {
      learntClauses(i).activity /= scaleLimit
      i += 1
    }
  }

  final def claDecayActivity(): Unit = activityStep *= activityDecay

  final def varBumpActivity(literal: Int): Unit = {
    val varId = literal / 2
    activities(varId) += variableStep
    heuristic.updateActivity(varId)
    if (activities(varId) >= scaleLimit) {
      varRescaleActivity()
    }
  }

  final def varRescaleActivity(): Unit = {
    var i = 0
    while (i < varStoreSize) {
      activities(i) /= scaleLimit
      heuristic.updateActivity(i)
      i += 1
    }
  }

  final def nAssigns(): Int = trail.size
  final def nVars(): Int = varStoreSize

  final def varDecayActivity(): Unit = variableStep *= variableDecay

  final def decayActivities(): Unit = {
    varDecayActivity()
    claDecayActivity()
  }

  // ANALYZE

  // These structures are used to build the nogood returned by a conflict analysis.
  private[this] val outLearnt: ArrayStackInt = new ArrayStackInt(16)
  private[this] val pReason: ArrayStackInt = new ArrayStackInt(16)

  final def decisionLevel: Int = trailLevels.size

  // TRAIL

  @inline private def undoOne(): Unit = {
    assert(trail.size > 0)
    val literal = trail.pop()
    val varId = literal / 2
    values(varId) = Unassigned // unasign
    reasons(varId) = null
    levels(varId) = -1
    heuristic.undo(varId)
  }

  protected def analyze(initConflict: Constraint): Int = {

    val seen: Array[Boolean] = new Array(values.size) // FIXME
    var counter = 0
    var p: Int = -1
    var conflict: Constraint = initConflict
    var outBacktsLevel = 0

    outLearnt.clear()
    outLearnt.append(-1) // leave a room for the asserting literal

    do {

      pReason.clear
      if (p == -1) conflict.explainAll(pReason)
      else conflict.explain(pReason)

      // Trace reason for p
      for (literal <- pReason) { // FIXME 
        val varId = literal / 2
        if (!seen(varId)) {
          seen(varId) = true
          val level = levels(varId)
          if (level == decisionLevel) counter += 1
          else if (level > 0) {
            outLearnt.append(literal ^ 1)
            if (level > outBacktsLevel) outBacktsLevel = level
          }
        }
      }

      // Select next literal to look at
      do {
        p = trail.top
        conflict = reasons(p / 2)
        undoOne()
      } while (!seen(p / 2))

      counter -= 1

    } while (counter > 0)

    outLearnt(0) = p ^ 1
    outBacktsLevel
  }

  @inline def assume(literal: Int): Boolean = {
    trailLevels.push(trail.size)
    enqueue(literal, null)
  }

  @inline private def cancel(): Unit = {
    var nLevels = trail.size - trailLevels.pop()
    while (nLevels > 0) {
      nLevels -= 1
      undoOne()
    }
  }

  @inline protected def cancelUntil(level: Int): Unit = {
    while (trailLevels.size > level) cancel()
  }

  protected def handleConflict(constraint: Constraint): Unit = {
    val backtrackLevel = analyze(constraint)
    cancelUntil(backtrackLevel)
    recordNogood()
    decayActivities()
  }

  protected def untrailAll(): Unit = {
    while (trailLevels.size > 0) cancel()
  }

  // Used to adapt the length of inner structures.
  @inline private def growVariableStore(): Unit = {
    val newSize = varStoreSize * 2
    val newValues = new Array[LiftedBoolean](newSize)
    val newReasons = new Array[Constraint](newSize)
    val newLevels = new Array[Int](newSize)
    val newActivities = new Array[Double](newSize)
    System.arraycopy(values, 0, newValues, 0, varStoreSize)
    System.arraycopy(reasons, 0, newReasons, 0, varStoreSize)
    System.arraycopy(levels, 0, newLevels, 0, varStoreSize)
    System.arraycopy(activities, 0, newActivities, 0, varStoreSize)
    values = newValues
    reasons = newReasons
    levels = newLevels
    activities = newActivities
  }
}