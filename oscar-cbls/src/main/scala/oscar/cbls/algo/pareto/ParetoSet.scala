package oscar.cbls.algo.pareto

class ParetoMap[T] extends Iterable[T]{
  def add(key:Array[Long],value:T)
  def getAllDominating(key:Array[Long]):ParetoMap[T]
  def smallest(keyID:Int):ParetoMap[T]

}
