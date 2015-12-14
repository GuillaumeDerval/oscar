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
  private[this] var _values: Array[LiftedBoolean] = new Array(128)
  private[this] var _reasons: Array[Constraint] = new Array(128)
  private[this] var _levels: Array[Int] = new Array(128)
  private[this] var _activities: Array[Double] = new Array(128)
  private[this] var _varStoreSize: Int = 0
  
  // Structures used for conflict analysis
  private[this] val outLearnt: ArrayStackInt = new ArrayStackInt(16)
  private[this] val explanation: ArrayStackInt = new ArrayStackInt(16)
  
  private[this] var _nogood: Array[Int] = new Array(16)
  private[this] var _nogoodSize: Int = 0
   
  // Literals seen (seen if _seen(i) == _magic) 
  private[this] var _seen: Array[Int] = new Array(128)
  private[this] var _magic: Int = 0

  /** Returns the clause responsible of the assignment */
  @inline final def assignReason(varId: Int): Constraint = _reasons(varId)

  @inline final def isAssigned(varId: Int): Boolean = _values(varId) != Unassigned

  @inline final def isTrue(varId: Int): Boolean = _values(varId) == True

  @inline final def isFalse(varId: Int): Boolean = _values(varId) == False

  @inline final def varActivity(varId: Int): Double = _activities(varId)
  
  @inline final def level(varId: Int): Int = _levels(varId)

  final def newVar(): Int = {
    if (_varStoreSize == _values.length) growVariableStore()
    val varId = _varStoreSize
    _values(varId) = Unassigned
    _reasons(varId) = null
    _levels(varId) = -1
    _activities(varId) = 0.0
    _varStoreSize += 1
    watchers.append(new Watchers(16))
    watchers.append(new Watchers(16))
    varId
  }

  @inline final def watch(constraint: Constraint, litId: Int): Unit = {
    watchers(litId).enqueue(constraint)
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

  final def add(constraint: Constraint): Boolean = {
    if (constraint == null) sys.error("null reference.")
    else {
      constraints.append(constraint)
      constraint.setup()
    }
  }
  
  final def addClause(literals: Array[Int]): Boolean = {
    if (literals == null) sys.error("null reference.")
    else if (literals.length == 0) true
    else if (literals.length == 1) enqueue(literals(0), null) // unit fact
    else add(Clause(this, literals)) 
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
    val value = _values(varId)
    val unsigned = (litId & 1) == 0
    if (value != Unassigned) {
      if (unsigned) value == True
      else value == False
    } else {
      // new fact to store
      if (unsigned) _values(varId) = True
      else _values(varId) = False
      _levels(varId) = trailLevels.size
      _reasons(varId) = from
      trail.push(litId)
      queue.addLast(litId)
      true
    }
  }

  final def litValue(literal: Int): LiftedBoolean = {
    val assigned = _values(literal / 2)
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
    _activities(varId) += variableStep
    heuristic.updateActivity(varId)
    if (_activities(varId) >= scaleLimit) {
      varRescaleActivity()
    }
  }

  final def varRescaleActivity(): Unit = {
    var i = 0
    while (i < _varStoreSize) {
      _activities(i) /= scaleLimit
      heuristic.updateActivity(i)
      i += 1
    }
  }

  final def nAssigns(): Int = trail.size
  final def nVars(): Int = _varStoreSize

  final def varDecayActivity(): Unit = variableStep *= variableDecay

  final def decayActivities(): Unit = {
    varDecayActivity()
    claDecayActivity()
  }

  final def decisionLevel: Int = trailLevels.size

  // TRAIL

  @inline private def undoOne(): Unit = {
    assert(trail.size > 0)
    val literal = trail.pop()
    val varId = literal / 2
    _values(varId) = Unassigned // unasign
    _reasons(varId) = null
    _levels(varId) = -1
    heuristic.undo(varId)
  }

  protected def analyze(initConflict: Constraint): Int = {

    var nPaths = 0
    var toExplainLit: Int = -1
    var conflict = initConflict
    var backtrackLevel = 0
    resetSeen()

    // New nogood
    outLearnt.clear()
    outLearnt.append(-1) // leave a room for the asserting literal

    do {
      // Build explanation
      explanation.clear
      if (toExplainLit == -1) conflict.explainAll(explanation)
      else conflict.explain(toExplainLit, explanation)

      // Fast access
      val explanationArray = explanation.innerArray()
      var i = explanation.length
      
      // Trace explanation
      while (i > 0) {
        i -= 1
        val literal = explanationArray(i)
        val varId = literal / 2
        if (_seen(varId) != _magic) {
          _seen(varId) = _magic
          val level = _levels(varId)
          if (level == decisionLevel) nPaths += 1
          else if (level > 0) {
            outLearnt.append(literal ^ 1)
            backtrackLevel = Math.max(backtrackLevel, level)
          }
        }
      }

      // Select the next literal to explain
      do {
        toExplainLit = trail.top
        conflict = _reasons(toExplainLit / 2)
        undoOne()
      } while (_seen(toExplainLit / 2) != _magic)

      nPaths -= 1

    } while (nPaths > 0)

    // Last literal is always the first UIP
    outLearnt(0) = toExplainLit ^ 1
    backtrackLevel
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
  
  private def resetSeen(): Unit = {
    if (_magic < Int.MaxValue) _magic += 1
    else {
      _magic = 1
      var i = _varStoreSize
      while (i > 0) {
        i -= 1
        // do not use Int.MinValue because
        // rescaled _seen are filled with 0
        _seen(i) = 0 
      }
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
    val newSize = _varStoreSize * 2
    val newValues = new Array[LiftedBoolean](newSize)
    val newReasons = new Array[Constraint](newSize)
    val newLevels = new Array[Int](newSize)
    val newActivities = new Array[Double](newSize)
    System.arraycopy(_values, 0, newValues, 0, _varStoreSize)
    System.arraycopy(_reasons, 0, newReasons, 0, _varStoreSize)
    System.arraycopy(_levels, 0, newLevels, 0, _varStoreSize)
    System.arraycopy(_activities, 0, newActivities, 0, _varStoreSize)
    _seen = new Array[Int](newSize) // no need to fill the array
    _values = newValues
    _reasons = newReasons
    _levels = newLevels
    _activities = newActivities
  }
}