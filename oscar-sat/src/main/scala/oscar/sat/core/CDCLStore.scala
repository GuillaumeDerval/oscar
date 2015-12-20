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
import oscar.algo.reversible.ReversibleContext
import oscar.algo.SortUtils

/** @author Renaud Hartert ren.hartert@gmail.com */

class CDCLStore {

  protected var heuristic: Heuristic = null

  // Constraints
  private[this] val constraints: ArrayStack[Constraint] = new ArrayStack(128)

  // Nogoods
  private[this] var _nogoods: Array[Nogood] = new Array(128)
  private[this] var _nNogoods: Int = 0

  // Activity and Decay
  private final val scaleLimit: Double = 100000000
  protected var activityStep: Double = 0.5
  protected var activityDecay: Double = 0.5
  protected var variableStep: Double = 0.5
  protected var variableDecay: Double = 0.5

  // Watchers of each literal used by clause and nogoods
  private[this] val watchers: ArrayStack[Watchers] = new ArrayStack(128)

  // Trails
  private[this] val trail: ArrayStackInt = new ArrayStackInt(100)
  private[this] val trailLevels: ArrayStackInt = new ArrayStackInt(100)
  private[this] val trailRev = new ReversibleContext()

  // Propagation queue
  private[this] val queue: ArrayQueueInt = new ArrayQueueInt(128)

  // Variables data (may be resized)
  private[this] var _values: Array[LiftedBoolean] = new Array(128)
  private[this] var _reasons: Array[Constraint] = new Array(128)
  private[this] var _levels: Array[Int] = new Array(128)
  private[this] var _activities: Array[Double] = new Array(128)
  private[this] var _varStoreSize: Int = 0

  // Structures used for conflict analysis
  private[this] val outLearnt: ArrayStackInt = new ArrayStackInt(16)
  private[this] val explanation: ArrayStackInt = new ArrayStackInt(16)
  private[this] var _seen: Array[Int] = new Array(128)
  private[this] var _magic: Int = 0 // seen if _seen(i) == _magic
  private[this] var _conflictingLiteral: Int = -1
  private[this] var _conflictingConstraint: Constraint = null

  /** Return a reference to the inner array containing the levels */
  @inline private[sat] def values: Array[LiftedBoolean] = _values

  /** Return a reference to the inner array containing the levels */
  @inline private[sat] def reasons: Array[Constraint] = _reasons

  /** Return a reference to the inner array containing the levels */
  @inline private[sat] def levels: Array[Int] = _levels

  /** Return a reference to the inner array containing the levels */
  @inline private[sat] def activities: Array[Double] = _activities

  /** Returns the clause responsible of the assignment */
  @inline final def assignReason(varId: Int): Constraint = _reasons(varId)

  @inline final def nNogoods: Int = _nNogoods
  
  @inline final def nConstraints: Int = constraints.length
  
  @inline final def isAssigned(varId: Int): Boolean = _values(varId) != Unassigned

  @inline final def isTrue(varId: Int): Boolean = _values(varId) == True

  @inline final def isFalse(varId: Int): Boolean = _values(varId) == False

  @inline final def varActivity(varId: Int): Double = _activities(varId)

  @inline final def varLevel(varId: Int): Int = _levels(varId)

  @inline final def trailReversible: ReversibleContext = trailRev

  @inline final def level: Int = trailLevels.size

  /** Resize internal structures to allocate n additional variables. */
  final def allocateVariables(n: Int): Unit = {
    if (_varStoreSize + n >= _values.length) {
      growVariableStore(_varStoreSize + n)
    }
  }

  /**
   *  Return the idea of a new variable.
   *  Resize internal structures if necessary.
   */
  final def newVar(): Int = {
    // Resize the internal structure
    if (_varStoreSize == _values.length) {
      growVariableStore(_varStoreSize * 2)
    }
    // Add a new variable
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

  // Record a new nogood based on the content of outLearnt
  private def recordNogood(): Boolean = {
    // Critical checks
    assert(outLearnt != null, "outLeart structure has not been instantiated.")
    assert(outLearnt.length > 0, "empty nogood.")
    assert(_reasons(outLearnt(0) / 2) == null, "first literal must be unassigned.")
    // Build and record the nogood
    if (outLearnt.length == 1) enqueue(outLearnt(0), null) // Unit fact
    else {
      // Allocate a new nogood
      val nogood = Nogood(this, outLearnt)
      addNogood(nogood)
      nogood.setup()
    }
  }

  // Add a new constraint to the store
  final def add(constraint: Constraint): Boolean = {
    assert(level == 0)
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
  final def propagate(): Boolean = {
    var feasible = true
    while (!queue.isEmpty && feasible) {
      val literal = queue.removeFirst()
      val constraints = watchers(literal)
      val nConstrains = constraints.length
      var i = nConstrains
      while (i > 0 && feasible) {
        i -= 1
        val constraint = constraints.dequeue()
        val consistent = constraint.propagate(literal)
        if (!consistent) feasible = false
      }
    }
    queue.clear()
    feasible
  }

  final def enqueue(litId: Int, reason: Constraint): Boolean = {
    val varId = litId / 2
    val value = _values(varId)
    val unsigned = (litId & 1) == 0
    if (value != Unassigned) {
      if (unsigned && value == True) true
      else if (!unsigned && value == False) true
      else {
        _conflictingLiteral = litId
        _conflictingConstraint = reason
        false
      }
    } else {
      // new fact to store
      if (unsigned) _values(varId) = True
      else _values(varId) = False
      _levels(varId) = trailLevels.size
      _reasons(varId) = reason
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
    while (i < _nNogoods) {
      _nogoods(i).activity /= scaleLimit
      i += 1
    }
    activityStep /= scaleLimit
  }

  final def claDecayActivity(): Unit = activityStep /= activityDecay

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
    variableStep /= scaleLimit
  }

  final def nAssigns(): Int = trail.size
  final def nVars(): Int = _varStoreSize

  final def varDecayActivity(): Unit = variableStep /= variableDecay

  final def decayActivities(): Unit = {
    varDecayActivity()
    claDecayActivity()
  }

  @inline private def undoOne(): Unit = {
    assert(trail.size > 0)
    val literal = trail.pop()
    val varId = literal / 2
    _values(varId) = Unassigned // unasign
    _reasons(varId) = null
    _levels(varId) = -1
    heuristic.undo(varId)
  }

  private def analyze(): Int = {

    val conflictLevel = level

    var nPaths = 0
    var toExplainLit: Int = _conflictingLiteral
    var conflict = _conflictingConstraint
    var backtrackLevel = 0
    resetSeen()

    // New nogood
    outLearnt.clear()
    outLearnt.append(-1) // leave a room for the asserting literal

    // Explanation
    explanation.clear
    explanation.append(toExplainLit ^ 1)

    do {

      // Build explanation
      conflict.explain(toExplainLit, explanation)

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
          if (level == conflictLevel) nPaths += 1
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
      explanation.clear()

    } while (nPaths > 0)

    // Last literal is always the first UIP
    outLearnt(0) = toExplainLit ^ 1
    backtrackLevel
  }

  @inline final def assume(literal: Int): Boolean = {
    push()
    enqueue(literal, null)
  }

  @inline private def push(): Unit = {
    trailLevels.push(trail.size)
    trailRev.pushState()
  }

  @inline private def cancel(): Unit = {
    trailRev.pop()
    // Pop trail literal level
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

  protected def handleConflict(): Unit = {
    val backtrackLevel = analyze()
    cancelUntil(backtrackLevel)
    recordNogood()
    decayActivities()
  }

  protected def untrailAll(): Unit = {
    while (trailLevels.size > 0) cancel()
  }

  final def filterDeletedConstraints(): Unit = {
    var i = watchers.length
    while (i > 0) {
      i -= 1
      watchers(i).filterDeleted()
    }
  }

  // Used to adapt the length of inner structures.
  @inline private def growVariableStore(newSize: Int): Unit = {
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

  @inline private def addNogood(nogood: Nogood): Unit = {
    if (_nNogoods == _nogoods.length) {
      val array = new Array[Nogood](_nNogoods * 2)
      System.arraycopy(_nogoods, 0, array, 0, _nNogoods)
      _nogoods = array
    }
    _nogoods(_nNogoods) = nogood
    _nNogoods += 1
  }

  final def reduceDb(): Unit = {

    sortNogoods()
    val lim = activityStep / _nNogoods

    var i = _nNogoods / 2
    var j = i

    while (i < _nNogoods) {
      if (!_nogoods(i).locked) _nogoods(i).delete()
      else {
        _nogoods(j) = _nogoods(i)
        j += 1
      }
      i += 1
    }
    
    _nNogoods = j
    i = 0
    j = 0
    while (i < _nNogoods) {
      if (!_nogoods(i).locked && _nogoods(i).activity < lim) _nogoods(i).delete()
      else {
        _nogoods(j) = _nogoods(i)
        j += 1
      }
      i += 1
    }
    
    _nNogoods = j
    filterDeletedConstraints()
  }

  // Decreasing order sort
  @inline private def sortNogoods(): Unit = {
    var i = 0
    var j = 0
    while (i < _nNogoods) {
      val tmp = _nogoods(i)
      val key = tmp.activity
      j = i
      while (j > 0 && key > _nogoods(j - 1).activity) {
        _nogoods(j) = _nogoods(j - 1)
        j -= 1
      }
      _nogoods(j) = tmp
      i += 1
    }
  }
}