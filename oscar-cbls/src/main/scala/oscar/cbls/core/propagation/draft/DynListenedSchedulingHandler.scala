package oscar.cbls.core.propagation.draft

import oscar.cbls.algo.quick.QList


class RootedSchedulingHandlerDynListened(val root:PropagationElement) extends RootedSchedulingHandlerDynListenedTrait {
  //among all scheduled element with this scheduling handler,
  //only the root can be listened by some PE not in the SH
}

trait RootedSchedulingHandlerDynListenedTrait extends SimpleSchedulingHandler {
  val root:PropagationElement

  root.dynamicallyListeningElements.notifyInserts(notifyRootListenedByNewPE)

  class ListeningSHAndValidityTest(val sh:SimpleSchedulingHandler,val isStillValid:()=>Boolean)
  var dynamicallyListeningSHAndValidityTest:QList[ListeningSHAndValidityTest] = null

  def notifyRootListenedByNewPE(newListeningPE:(PropagationElement,Int),isStillValid:()=> Boolean) {
    if (isStillValid == null || isStillValid()) {
      dynamicallyListeningSHAndValidityTest = QList(new ListeningSHAndValidityTest(newListeningPE._1.schedulingHandler, isStillValid), dynamicallyListeningSHAndValidityTest)
      if (isScheduled) {
        newListeningPE._1.schedulingHandler.scheduleSHForPropagation(this, isStillValid)
      }
    }
  }

  override protected def scheduleMyselfForPropagation(){
    if(!isScheduled){
      isScheduled = true
      var listeningSchedulingHandlersAcc = dynamicallyListeningSHAndValidityTest
      dynamicallyListeningSHAndValidityTest = null
      while(listeningSchedulingHandlersAcc != null){
        val prop = listeningSchedulingHandlersAcc.head
        val isSTillValidFn = prop.isStillValid
        if(isSTillValidFn()) {
          prop.sh.scheduleSHForPropagation(this, prop.isStillValid)
          dynamicallyListeningSHAndValidityTest = QList(prop,dynamicallyListeningSHAndValidityTest)
        }
        listeningSchedulingHandlersAcc = listeningSchedulingHandlersAcc.tail
      }
    }
  }
}

//the one for dynamic dependencies
//basically, the PE has a set of dynamic dependencies, and a varying dependency.
//this scheduling handler is the SH of: the varying PE, the determining element.
//all other dependencies are considered dynamic, and have one scheduling handler each. As such, the
class DynListeningSchedulingHandler(val p:PropagationElement, s:PropagationStructure) extends SimpleSchedulingHandler{
  s.registerSchedulingHandler(this)
  require(p.varyingDependencies, "expecting a PE with varying dependencies")

  var isDeterminingElementScheduled:Boolean = false


  override def scheduleSHForPropagation(sh:SimpleSchedulingHandler,isStillValid:()=>Boolean): Unit ={

  }



  //when this one is registered for run, it schedules the trigger for propagation
  //when it is enqueued for run, it enqueues the SHof the determining element
  //when the trigger is propagated, it calls this class to

  override def schedulePEForPropagation(pe:PropagationElement): Unit ={
    if(isRunning){
      //we are actually propagating
      runner.enqueuePE(pe)
    }else {
      if(pe == p.determiningElement) isDeterminingElementScheduled = true
      scheduledElements = QList(pe, scheduledElements)
      scheduleMyselfForPropagation()
    }
  }

  def runNeededDynamicDependencies(): Unit ={
    for(dynamicallyListenedPE <- p.dynamicallyListenedElements if dynamicallyListenedPE != p.determiningElement){
      if(dynamicallyListenedPE.schedulingHandler.isScheduled){
        dynamicallyListenedPE.schedulingHandler.enqueueForRun()
      }
    }
  }

  def runDeterminingElementIfNeeded():Boolean={
    if(p.determiningElement == null){
      false
    }else{

    }
  }
}


