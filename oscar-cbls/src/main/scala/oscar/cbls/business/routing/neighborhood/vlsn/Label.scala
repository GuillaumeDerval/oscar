package oscar.cbls.business.routing.neighborhood.vlsn

import oscar.cbls.algo.magicArray.MagicBoolArray

case class LabelSystem(labelsToNbLabels:Array[Int], vehicleToLabels:Int => Array[Int], v:Int) {

  def areInterferingSlow(vehicle1:Int,vehicle2:Int):Boolean = {
    val ar1 = vehicleToLabels(vehicle1)
    val ar2 = vehicleToLabels(vehicle2)

    var l = labelsToNbLabels.length
    while(l != 0){
      l = l-1
      if(ar1(l) == ar2(l)) return true
    }
    false
  }

  val interferenceArray:Array[Array[Boolean]] = Array.tabulate(v)(vehicle1 => Array.tabulate(v)(vehicle2 => areInterferingSlow(vehicle1,vehicle2)))

  def areInterferingFast(vehicle1:Int,vehicle2:Int):Boolean = {
    interferenceArray(vehicle1)(vehicle2)
  }

  val interferingVehicles:Array[List[Int]] =
    Array.tabulate(v)(vehicle1 => (0 until v).toList.filter(vehicle2 => areInterferingFast(vehicle1,vehicle2)))

  val nbLabelDimensions = labelsToNbLabels.length
  val labelDimensionToOffset = Array.fill(nbLabelDimensions)(0)
  for(i <- 1 until labelDimensionToOffset.length){
    labelDimensionToOffset(i) = labelDimensionToOffset(i-1) + labelsToNbLabels(i-1)
  }

  val totalLabels = labelsToNbLabels.sum

  def makeMagicArray:MagicBooleanLabelArray = MagicBooleanLabelArray(this)
}

case class MagicBooleanLabelArray(ls:LabelSystem){

  var nbLabelDimensions = ls.nbLabelDimensions
  val labelDimensionToOffset = ls.labelDimensionToOffset
  val totalLabels = ls.totalLabels

  private val isOffsetLabelMarked = MagicBoolArray(totalLabels,false)

  def setAllLabelsOf(vehicle:Int): Unit ={
    val idToValue = ls.vehicleToLabels(vehicle)
    var l = nbLabelDimensions
    while(l != 0){
      l = l-1
      isOffsetLabelMarked(idToValue(l) + labelDimensionToOffset(l)) = true
    }
  }
  def hasAnyMarkedLabel(vehicle:Int):Boolean = {
    val idToValue = ls.vehicleToLabels(vehicle)
    var l = nbLabelDimensions
    while(l != 0){
      l = l-1
      if(isOffsetLabelMarked(idToValue(l) + labelDimensionToOffset(l))) return true
    }
    false
  }

  def clearAll(): Unit ={
    isOffsetLabelMarked.all = false
  }
}
