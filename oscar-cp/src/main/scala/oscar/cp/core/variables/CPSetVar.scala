package oscar.cp.core.variables

import oscar.algo.reversible.ReversibleQueue
import oscar.algo.reversible.Reversible
import oscar.cp._
import oscar.cp.core.CPOutcome._
import oscar.cp.constraints.sets.Requires
import oscar.cp.constraints.sets.Excludes
import oscar.cp.constraints.SetCard
import oscar.cp.core.domains.SetDomain
import oscar.algo.reversible.ReversiblePointer
import oscar.cp.core.delta._
import oscar.cp.core.CPOutcome
import oscar.cp.core.Constraint
import oscar.cp.core.CPStore
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.watcher.PropagEventQueueVarSet
import oscar.cp.core.watcher.WatcherListL2
import scala.collection.JavaConversions.mapAsScalaMap

/**
 * @author Pierre Schaus pschaus@gmail.com
 * @author Renaud Hartert ren.hartert@gmail.com
 */
class CPSetVar(override val store: CPStore, min: Int, max: Int, override val name: String = "") extends CPVar {

  private val dom = new SetDomain(store, min, max)

  val onDomainL2 = new WatcherListL2(store)
  val onRequiredL1 = new ReversiblePointer[PropagEventQueueVarSet](store, null)
  val onExcludedL1 = new ReversiblePointer[PropagEventQueueVarSet](store, null)
  val onRequiredIdxL1 = new ReversiblePointer[PropagEventQueueVarSet](store, null)
  val onExcludedIdxL1 = new ReversiblePointer[PropagEventQueueVarSet](store, null)

  // cardinality variable
  val card = CPIntVar(0, max - min + 1)(store)
  store.post(new SetCard(this, card))

  /**
   * @return true if the domain of the variable has exactly one value,
   * false if the domain has more than one value
   */
  def isBound: Boolean = dom.possibleSize == dom.requiredSize

  /**
   * Test if a value is in the possible values
   * @param val
   * @return  true if value is in the possible values false otherwise
   */
  def isPossible(value: Int) = dom.isPossible(value)

  /**
   * Test if a value is in the required values
   * @param val
   * @return  true if value is in the required values false otherwise
   */
  def isRequired(value: Int) = dom.isRequired(value)

  /**
   * Level 2 registration: ask that the propagate() method of the constraint c is called whenever the domain of the variable changes
   * @param c
   * @see oscar.cp.core.Constraint#propagate()
   */
  def callPropagateWhenDomainChanges(c: Constraint) {
    onDomainL2.register(c)
  }

  def callPropagateOnChangesWithDelta(c: Constraint): DeltaSetVar = {
    val snap = delta(c)
    onDomainL2.register(c)
    snap
  }


  def delta(constraint: Constraint,id: Int = 0): DeltaSetVar = {
    val delta = new DeltaSetVar(this,id)
    constraint.registerDelta(delta)
    delta
  }


  def filterWhenDomainChangesWithDelta(idempotent: Boolean = false, priority: Int = CPStore.MaxPriorityL2 - 2)(filter: DeltaSetVar => CPOutcome): DeltaSetVar = {
    val propagator = new PropagatorSetVar(this, 0, filter)
    propagator.idempotent = idempotent
    propagator.priorityL2 = priority
    callPropagateWhenDomainChanges(propagator)
    propagator.snapshot
  }




  /**
   * Level 1 registration: ask that the propagate() method of the constraint c is called whenever ...
   * @param c
   * @see oscar.cp.core.Constraint#propagate()
   */
  def callValRequiredWhenRequiredValue(c: Constraint) {
    onRequiredL1.setValue(new PropagEventQueueVarSet(onRequiredL1.value, c, this))
  }

  /**
   * Level 1 registration: ask that the propagate() method of the constraint c is called whenever ...
   * @param c
   * @see oscar.cp.core.Constraint#propagate()
   */
  def callValExcludedWhenExcludedValue(c: Constraint) {
    onExcludedL1.setValue(new PropagEventQueueVarSet(onExcludedL1.value, c, this))
  }

  /**
   * Level 1 registration: ask that the propagate() method of the constraint c is called whenever ...
   * @param c
   * @see oscar.cp.core.Constraint#propagate()
   */
  def callValRequiredIdxWhenRequiredValue(c: Constraint, idx: Int) {
    onRequiredIdxL1.setValue(new PropagEventQueueVarSet(onRequiredIdxL1.value, c, this, idx))
  }

  /**
   * Level 1 registration: ask that the propagate() method of the constraint c is called whenever ...
   * @param c
   * @see oscar.cp.core.Constraint#propagate()
   */
  def callValExcludedIdxWhenExcludedValue(c: Constraint, idx: Int) {
    onExcludedIdxL1.setValue(new PropagEventQueueVarSet(onExcludedIdxL1.value, c, this, idx))
  }

  def requires(v: Int): CPOutcome = {
    if (dom.isPossible(v) && !dom.isRequired(v)) {
      // -------- AC3 notifications ------------
      onDomainL2.enqueue()
      // -------- AC5 notifications ------------
      store.notifyRequired(onRequiredL1.value, this, v)
      store.notifyRequiredIdx(onRequiredIdxL1.value, this, v)
    }
    val oc = dom.requires(v)
    if (oc != CPOutcome.Failure) {
      if (requiredSize == card.max) {
        for (a: Int <- possibleNotRequiredValues.toSet) {
          val r = excludes(a)
          assert(r != CPOutcome.Failure)
        }
      }
      card.updateMin(requiredSize)
    } else oc
  }

  def excludes(v: Int): CPOutcome = {
    if (dom.isPossible(v) && !dom.isRequired(v)) {
      // -------- AC3 notifications ------------
      onDomainL2.enqueue()
      // -------- AC5 notifications ------------
      store.notifyExcluded(onExcludedL1.value, this, v)
      store.notifyExcludedIdx(onExcludedIdxL1.value, this, v)
    }
    val oc = dom.excludes(v)
    if (oc != CPOutcome.Failure) {
      if (possibleSize == card.min) {
        for (a: Int <- possibleNotRequiredValues.toSet) {
          val r = requires(a)
          assert(r != CPOutcome.Failure)
        }
      }
      card.updateMax(possibleSize)
    } else oc
  }

  def requiresAll(): CPOutcome = {
    // -------- AC3 notifications ------------
    if (possibleSize > requiredSize) onDomainL2.enqueue()
    // -------- AC5 notifications ------------
    if (onRequiredL1.hasValue || onRequiredIdxL1.hasValue) {
      for (v <- dom.possibleNotRequiredValues) {
        if (onRequiredL1.hasValue) store.notifyRequired(onRequiredL1.value, this, v)
        if (onRequiredIdxL1.hasValue) store.notifyRequiredIdx(onRequiredIdxL1.value, this, v)
      }
    }
    val oc = dom.requiresAll()
    assert(oc != CPOutcome.Failure)
    card.assign(requiredSize)
  }

  def excludesAll(): CPOutcome = {
    // -------- AC3 notifications ------------
    if (possibleSize > requiredSize) onDomainL2.enqueue()
    // -------- AC5 notifications ------------
    if (onExcludedL1.hasValue || onExcludedIdxL1.hasValue) {
      for (v <- dom.possibleNotRequiredValues) {
        if (onExcludedL1.hasValue) store.notifyExcluded(onExcludedL1.value, this, v)
        if (onExcludedIdxL1.hasValue) store.notifyExcludedIdx(onExcludedIdxL1.value, this, v)
      }
    }
    val oc = dom.excludesAll()
    assert(oc != CPOutcome.Failure)
    card.assign(requiredSize)
  }

  def arbitraryPossibleNotRequired: Int = dom.arbitraryPossibleNotRequired

  def randomPossibleNotRequired: Int = dom.randomPossibleNotRequired

  def value(): Set[Int] = dom.requiredSet

  def requiredSet(): Set[Int] = dom.requiredSet

  def possibleSet(): Set[Int] = dom.possibleSet

  def possibleNotRequiredValues: Iterator[Int] = dom.possibleNotRequiredValues

  def requiredValues: Iterator[Int] = dom.requiredValues

  def possibleSize: Int = dom.possibleSize

  def requiredSize: Int = dom.requiredSize

  def ++(v: Int): Constraint = new Requires(this, v)
  def --(v: Int): Constraint = new Excludes(this, v)

  override def toString: String = {
    val required = requiredValues.toSet
    val possible = possibleNotRequiredValues.toSet
    s"$required $possible"
  }

  // ----------- delta methods ------------

  def changed(sn: DeltaSetVar): Boolean = possibleChanged(sn) || requiredChanged(sn)

  def possibleChanged(sn: DeltaSetVar): Boolean = sn.oldSizePossible != possibleSize

  def requiredChanged(sn: DeltaSetVar): Boolean = sn.oldSizeRequired != requiredSize

  def deltaPossibleSize(sn: DeltaSetVar): Int = sn.oldSizePossible - possibleSize

  def deltaRequiredSize(sn: DeltaSetVar): Int = requiredSize - sn.oldSizeRequired

  def deltaPossible(sn: DeltaSetVar): Iterator[Int] = dom.deltaPossible(sn.oldSizePossible)

  def deltaRequired(sn: DeltaSetVar): Iterator[Int] = dom.deltaRequired(sn.oldSizeRequired)

  def ==(y: CPSetVar): Constraint = new oscar.cp.constraints.SetEq(this, y)
}

object CPSetVar {
  
  /** Creates a new CP Var Set that can contain a set of possible values in which some are required */
  def apply(possible: Set[Int], required: Set[Int])(implicit store: CPStore): CPSetVar = {
    if (!required.forall(elem => possible.contains(elem))) {
      throw new RuntimeException("Required values should be possible")
    }
    val set = new CPSetVar(store, possible.min, possible.max) 
    // Initializes the possible element
    for (elem <- possible.min to possible.max if !possible.contains(elem)) {
      set.excludes(elem)
    }
    // Initializes the required elements
    required.foreach(set.requires)
    set
  }
  
  /** Creates a new CP Var Set that can contain a set of possible values */
  def apply(possible: Set[Int])(implicit store: CPStore): CPSetVar = apply(possible, Set())(store)
  
  @deprecated("use apply(required: Set[Int], possibleNotRequired: Set[Int])(implicit store: CPStore) instead", "1.0")
  def apply(s: CPStore, required: Set[Int] = Set(), possibleNotRequired: Set[Int]): CPSetVar = {
    if (!required.intersect(possibleNotRequired).isEmpty) {
      throw new RuntimeException("Possible values should not be required")
    }
    val allPossibles = required ++ possibleNotRequired
    val x = new CPSetVar(s, allPossibles.min, allPossibles.max)
    for (v <- allPossibles.min to allPossibles.max if !allPossibles.contains(v)) {
      x.excludes(v)
    }
    for (v <- required) {
      x.requires(v)
    }
    x
  }
}
  
