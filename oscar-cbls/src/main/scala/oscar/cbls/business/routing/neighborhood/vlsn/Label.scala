package oscar.cbls.business.routing.neighborhood.vlsn

import oscar.cbls.algo.magicArray.MagicBoolArray

//negative labels are ignored, so you can set -1 for non-labelled stuff
case class LabelSystem(labelsToNbLabels:Array[Int], vehicleToLabels:Int => Array[Int], v:Int) {

  def areInterferingSlow(vehicle1:Int,vehicle2:Int):Boolean = {
    val ar1 = vehicleToLabels(vehicle1)
    val ar2 = vehicleToLabels(vehicle2)

    var l = labelsToNbLabels.length
    while(l != 0){
      l = l-1
      if(ar1(l) >= 0 && ar1(l) == ar2(l)) return true
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

  val nbLabelDimensions:Int = ls.nbLabelDimensions
  private val labelDimensionToOffset:Array[Int] = ls.labelDimensionToOffset
  val totalLabels:Int = ls.totalLabels

  private val isOffsetLabelMarked = MagicBoolArray(totalLabels,false)
  private val vehicleHasAnyMarkedLabelCache = MagicBoolArray(ls.v,false)

  def setAllLabelsOf(vehicle:Int): Unit ={
    val idToValue = ls.vehicleToLabels(vehicle)
    var l = nbLabelDimensions
    while(l != 0){
      l = l-1
      val labelForDimension = idToValue(l)
      if(labelForDimension >= 0) {
        isOffsetLabelMarked(labelForDimension+labelDimensionToOffset(l)) = true
      }
    }
  }

  def hasAnyMarkedLabel(vehicle:Int):Boolean = {
    if(vehicleHasAnyMarkedLabelCache(vehicle)) return true
    val idToValue = ls.vehicleToLabels(vehicle)
    var l = nbLabelDimensions
    while(l != 0){
      l = l-1
      val labelForDimension = idToValue(l)
      if(labelForDimension >= 0) {
        if (isOffsetLabelMarked(idToValue(l) + labelDimensionToOffset(l))){
          vehicleHasAnyMarkedLabelCache(vehicle) = true
          return true
        }
      }
    }
    false
  }

  def clearAll(): Unit ={
    vehicleHasAnyMarkedLabelCache.all = false
    isOffsetLabelMarked.all = false
  }
}


