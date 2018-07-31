package oscar.cbls.core.draft.computation

import oscar.cbls.core.draft.propagation.PropagationElement


trait InvariantTrait extends PropagationElement{

  def defineOutputVariable(v:Variable): Unit ={
    require(!v.hasDefiningInvariant, "variable " + v.name + "already has a defining invariant")
    this.registerStaticallyListeningElement(v)
    this.registerPermanentDynamicDependency(v,Int.MinValue)
  }

  /** this is the propagation method that should be overridden by propagation elements.
    * notice that it is only called in a propagation wave if:
    * 1: it has been registered for propagation since the last time it was propagated
    * 2: it is included in the propagation wave: partial propagation wave do not propagate all propagation elements;
    * it only propagates the ones that come in the predecessors of the targeted propagation element
    * overriding this method is optional, so an empty body is provided by default */
  override def performPropagation(){performInvariantPropagation()}

  def performInvariantPropagation(){}
}

class Invariant(store:Store)
  extends InvariantTrait{
  store.registerInvariant(this)

}


object Invariant{
  implicit val Ord:Ordering[Invariant] = new Ordering[Invariant]{
    def compare(o1: Invariant, o2: Invariant) = o1.compare(o2)
  }
}

object InvariantTrait{
  implicit val Ord:Ordering[InvariantTrait] = new Ordering[InvariantTrait]{
    def compare(o1: InvariantTrait, o2: InvariantTrait) = o1.compare(o2)
  }
}
