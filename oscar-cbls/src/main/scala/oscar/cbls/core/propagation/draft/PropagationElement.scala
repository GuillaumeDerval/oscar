package oscar.cbls.core.propagation.draft

import PropagationImpactCharacteristics._
import oscar.cbls.algo.quick.QList


trait DynamicDependency extends PropagationElement{
  override def finishInitialization(){
    super.finishInitialization()
    //create the VSH structure here
  }
}

abstract class PropagationElement(val notificationBehavior:PropagationImpactCharacteristics){

  val couldBePropagated:Boolean = {
    notificationBehavior match {
      case NotificationOnPropagateNoNotificationReceived | NotificationOnNotifyAndPropagate | NotificationOnPropagateReceivesNotification => true
      case NotificationOnNotifyNoPropagate | BulkElement | NoPropagationNotificationReceivedNoNotificationEmitted => false
    }
  }

  var layer:Int
  var threadID:Int
  var isScheduled:Boolean = false
  var schedulingHandler:SchedulingHandler
  var model:PropagationStructure = null

  var staticallyListeningElements:QList[PropagationElement] = null

  def finishInitialization(): Unit ={
    schedulingHandler = model
  }

  def scheduleMyselfForPropagation(): Unit ={
    assert(!couldBePropagated)
    if(!isScheduled){
      schedulingHandler.schedulePEForPropagation(this)
      isScheduled = true
    }
  }

  def reScheduleIfScheduled(): Unit ={
    if(isScheduled){
      schedulingHandler.schedulePEForPropagation(this)
    }
  }

  def triggerPropagation(){
    model.triggerPropagation(this)
  }

  final def performPropagation(){
    require(!couldBePropagated)
    if(isScheduled) {
      isScheduled = false
      propagate()
    }
  }

  protected def propagate() = ???
}
