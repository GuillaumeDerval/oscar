package oscar.modeling.vars.cp

import oscar.algo.reversible.TrailEntry
import oscar.algo.search.IntConstrainableContext
import oscar.algo.vars.IntVarLike
import oscar.modeling.models.ModelDeclaration
import oscar.modeling.vars.IntVar

/**
  * Proxies request made to the "model context" to the effective CP context
  */
class ContextProxy(store: IntConstrainableContext) extends IntConstrainableContext {
  /**
    * Helper to get the ModelDeclaration and the IntVar from an IntVarLike
    */
  @inline private[this] final def c(v: IntVarLike): (ModelDeclaration, IntVar) = {
    (v.asInstanceOf[IntVar].model_decl, v.asInstanceOf[IntVar])
  }

  /**
    * Post x == v
    */
  @inline final override def assign(x: IntVarLike, v: Int): Unit = {
    val (modelDeclaration, iv) = c(x)
    // this will call cpstore.assign
    modelDeclaration.post(iv === v)
  }

  /**
    * Post x != v
    */
  @inline final override def remove(x: IntVarLike, v: Int): Unit = {
    val (modelDeclaration, iv) = c(x)
    // this will call cpstore.remove
    modelDeclaration.post(iv !== v)
  }

  /**
    * Post x <= v
    */
  @inline final override def smallerEq(x: IntVarLike, v: Int): Unit = {
    val (modelDeclaration, iv) = c(x)
    // this will call cpstore.smallerEq
    modelDeclaration.post(iv <= v)
  }

  /**
    * Post x >= v
    */
  @inline final override def largerEq(x: IntVarLike, v: Int): Unit = {
    val (modelDeclaration, iv) = c(x)
    // this will call cpstore.largerEq
    modelDeclaration.post(iv >= v)
  }

  /**
    * Post x != v for all v in vs
    */
  @inline final override def remove(x: IntVarLike, vs: Array[Int]): Unit = {
    val (modelDeclaration, iv) = c(x)
    // this will call cpstore.remove
    vs.foreach(v => modelDeclaration.post(iv !== v))
  }

  /**
    * @return true if this context is in fail state
    */
  @inline final override def isFailed: Boolean = store.isFailed

  /** Returns the magic number of the context */
  @inline final override def magic: Long = store.magic

  /** Stores the current state of the node on a stack */
  @inline final override def pushState(): Unit = store.pushState()

  /** Restores state on top of the stack of states and remove it from the stack */
  @inline final override def pop(): Unit = store.pop()

  /** Trail the entry such that its restore method is called on corresponding pop */
  @inline final override def trail(entry: TrailEntry): Unit = store.trail(entry)

  /** Trail the closure such that it is called on corresponding pop */
  @inline final override def trail[@specialized T](closure: => T): Unit = store.trail[T](closure)
}
