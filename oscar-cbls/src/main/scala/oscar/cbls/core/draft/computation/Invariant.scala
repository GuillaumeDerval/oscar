package oscar.cbls.core.draft.computation

import oscar.cbls.algo.distributedStorage.DistributedStorageUtility
import oscar.cbls.core.draft.propagation.PropagationElement

object Invariant{
  implicit val Ord:Ordering[Invariant] = new Ordering[Invariant]{
    def compare(o1: Invariant, o2: Invariant) = o1.compare(o2)
  }
}



/**This is be base class for all invariants.
  * Invariants also register to the model, but they identify the model they are associated to by querying the variables they are monitoring.
  */
trait Invariant extends PropagationElement{

  def model:Store = schedulingHandler.asInstanceOf[Store]

  def hasModel:Boolean = schedulingHandler != null

  final def preFinishInitialization(model:Store = null):Store = {
    if (schedulingHandler == null){
      if (model == null){
        schedulingHandler = InvariantHelper.findModel(getStaticallyListenedElements)
        this.model
      }else{
        schedulingHandler = model //assert(model == InvariantHelper.FindModel(getStaticallyListenedElements()))
        model
      }
    }else this.model
  }

  /**Must be called by all invariant after they complete their initialization
    * that is: before they get their output variable.
    * This performs some registration to the model, which is discovered by exploring the variables that are statically registered to the model
    * no more variable can be registered statically after this method has been called unless you provide the model in the parameter.
    * @param model; if specified, it only checks that the model is coherent, and registers to it for the ordering
    */
  final def finishInitialization(model:Store = null){
    val m:Store = preFinishInitialization(model)
    assert(uniqueID == -1)
    if (m != null){
      uniqueID = m.registerInvariant(this)
    }else{
      uniqueID = -1
    }
  }

  /**Call this from within the invariant to notify that you will statically listen to this variable.
    * You CANNOT register a variable twice. It is undetected, but will lead to unexpected behavior.
    * @param v the variable that you want to listen to (and be notified about change)
    */
  def registerStaticDependency(v:Value){
    registerStaticallyListenedElement(v)
  }

  def registerStaticDependencies(v:Value*){
    for (vv <- v)registerStaticDependency(vv)
  }

  /**this method is an alternative to the two call to the registration methods
    * it performs the registration to the static and dynamic graphs at once.
    * just to spare one call
    * @param v the variable that we want to register to
    * @param i the integer value that will be passed to the invariant to notify some changes in the value of this variable
    */
  def registerStaticAndDynamicDependency(v:Value,i:Int = -1){
    registerStaticDependency(v)
    registerDynamicDependency(v,i)
  }

  def registerStaticAndDynamicDependencies(v:((Value,Int))*){
    for (varint <- v){
      registerStaticDependency(varint._1)
      registerDynamicDependency(varint._1,varint._2)
    }
  }

  /**
    * registers static and dynamic dependency to all items in the array
    * let be i, a position in the array, the index used for dynamic dependency registration is i+offset
    * @param v the variable that are registered
    * @param offset and offset applied to the position in the array when registering the dynamic dependency
    * @return null
    */
  def registerStaticAndDynamicDependencyArrayIndex[T <: Value](v:Array[T],offset:Int = 0):Array[KeyForElementRemoval] =  {
    for (i <- 0 until v.size) {
      registerStaticDependency(v(i))
      registerDynamicDependency(v(i), i + offset)
    }
    null
  }


  def registerStaticAndDynamicDependenciesNoID(v:Value*){
    for (varint <- v){
      registerStaticDependency(varint)
      registerDynamicDependency(varint)
    }
  }

  def registerStaticAndDynamicDependencyAllNoID(v:Iterable[Value]){
    for (varint <- v){
      registerStaticDependency(varint)
      registerDynamicDependency(varint)
    }
  }

  /**Call this from within the invariant to notify that you will listen to this variable.
    * The variable must be registered in the static propagation graph.
    * You CANNOT register a variable twice. It is undetected, but will lead to unexpected behavior.
    * @param v the variable that you want to listen to (and be notified about change)
    * @param i: an integer value that will be passed when updates on this variable are notified to the invariant
    * @return null
    */
  def registerDynamicDependency(v:Value,i:Int = -1):KeyForElementRemoval = {
    registerDynamicallyListenedElement(v,i)
    null
  }



  /**To override whenever possible to spot errors in invariants.
    * this will be called for each invariant after propagation is performed.
    * It requires that the Model is instantiated with the variable debug set to true.
    */
  override def checkInternals():Unit = ???

  /** this is the propagation method that should be overridden by propagation elements.
    * notice that it is only called in a propagation wave if:
    * 1: it has been registered for propagation since the last time it was propagated
    * 2: it is included in the propagation wave: partial propagation wave do not propagate all propagation elements;
    * it only propagates the ones that come in the predecessors of the targeted propagation element
    * overriding this method is optional, so an empty body is provided by default */
  override def performPropagation(){performInvariantPropagation()}

  def performInvariantPropagation(){}
}

object InvariantHelper{
  /**this is useful to find the model out of a set of propagation elements.
    *
    * @param i some propagation elements, typically, variables listened by some invariants
    * @return the model that the invariant belongs to
    */
  def findModel(i:Iterable[BasicPropagationElement]):Store={
    val it = i.toIterator
    while(it.hasNext){
      it.next() match{
        case pe:AbstractVariable if pe.model != null => return pe.model
        case _ => ;
      }
    }
    null
  }

  def findModel(i:BasicPropagationElement*):Store={
    val it = i.toIterator
    while(it.hasNext){
      it.next() match {
        case v: Variable =>
          val m = v.model
          if (m != null) return m
        case _ => ;
      }}
    null
  }

  def getMinMaxBounds(variables:Iterable[IntValue]):(Int,Int) = {
    var MyMax = Int.MinValue
    var MyMin = Int.MaxValue
    for (v <- variables) {
      if (MyMax < v.max) MyMax = v.max
      if (MyMin > v.min) MyMin = v.min
    }
    (MyMin, MyMax)
  }

  def getMinMaxRange(variables:Iterable[IntValue]):Range = {
    val (min,max) = getMinMaxBounds(variables)
    min to max
  }

  def getMinMaxBoundsInt(variables:Iterable[Int]):(Int,Int) = {
    var MyMax = Int.MinValue
    var MyMin = Int.MaxValue
    for (v <- variables) {
      if (MyMax < v) MyMax = v
      if (MyMin > v) MyMin = v
    }
    (MyMin, MyMax)
  }

  def getMinMaxRangeInt(variables:Iterable[Int]):Range = {
    val (min,max) = getMinMaxBoundsInt(variables)
    min to max
  }

  def getMinMaxBoundsSet(variables:Iterable[SetValue]):(Int,Int) = {
    var MyMax = Int.MinValue
    var MyMin = Int.MaxValue
    for (v <- variables) {
      if (MyMax < v.max) MyMax = v.max
      if (MyMin > v.min) MyMin = v.min
    }
    (MyMin, MyMax)
  }

  def getMinMaxRangeSet(variables:Iterable[SetValue]):Range = {
    val (min,max) = getMinMaxBoundsSet(variables)
    min to max
  }

  def arrayToString[T](a:Array[T]):String =
    "[" + a.toList.mkString(",")+"]"
}


trait VaryingDependencies extends Invariant with VaryingDependenciesPE{

  /**register to determining element. It must be in the static dependency graph*/
  def registerDeterminingDependency(v:Value,i:Int = -1){
    registerDeterminingElement(v,i)
  }

  /**Call this from within the invariant to notify that you will listen to this variable.
    * The variable must be registered in the static propagation graph.
    * You CANNOT register a variable twice. It is undetected, but will lead to unexpected behavior.
    * @param v the variable that you want to listen to (and be notified about change)
    * @param i: an integer value that will be passed when updates on this variable are notified to the invariant
    * @return a handle that is required to remove the listened var from the dynamically listened ones
    */
  override def registerDynamicDependency(v:Value,i:Int = -1):KeyForElementRemoval = {
    registerDynamicallyListenedElement(v,i)
  }

  /**
    * registers static and dynamic dependency to all items in the array
    * let be i, a position in the array, the index used for dynamic dependency registration is i+offset
    * @param v the variable that are registered
    * @param offset and offset applied to the position in the array when registering the dynamic dependency
    */
  override def registerStaticAndDynamicDependencyArrayIndex[T <: Value](v:Array[T],offset:Int = 0):Array[KeyForElementRemoval] = {
    Array.tabulate(v.size)((i:Int) => {
      registerStaticDependency(v(i))
      registerDynamicDependency(v(i),i + offset)
    })
  }
}


