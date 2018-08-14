package oscar.visualfx.trackopt

import javafx.animation.Animation
import javafx.beans.value.{ChangeListener, ObservableValue}
import javafx.collections.ListChangeListener
import scalafx.animation.{ParallelTransition, PauseTransition, SequentialTransition}
import scalafx.application.JFXApp
import scalafx.util.Duration


class Scheduler(visualPlan: VisualPlan, nbLadle: Int = 1) {

  var _speed: Double = 1000
  val millisToMin: Double = 60 * 1000
  val ladleArray: Array[Ladle] = new Array[Ladle](nbLadle)
  val startPos: Pos = new Pos(-500,189,"starting position", new Duration(0) / _speed)
  val eafPos: Pos = new Pos(-139,106,"EAF", new Duration(47 * millisToMin) / _speed)
  val eafHeatingPos: Pos = new Pos(-67,106,"EAF Heating", new Duration(10 * millisToMin) / _speed)
  val lfLoadUnload: Pos = new Pos(10,106,"LF Load/Unload", new Duration(10 * millisToMin) / _speed)
  val lfWaitingPos: Pos = new Pos(10,59,"LF Waiting", new Duration(5 * millisToMin) / _speed)
  val lfWorkingPos: Pos = new Pos(10,24,"LF Working", new Duration(40 * millisToMin) / _speed)
  val mechMaintenancePos: Pos = new Pos(98,230,"Mechanical Maintenance", new Duration(50 * millisToMin) / _speed)
  val ladleHeating4Pos: Pos = new Pos(154,127,"Heating 4", new Duration(10 * millisToMin) / _speed)
  val ccWorkingPos: Pos = new Pos(191,34,"CC Working", new Duration(5 * millisToMin) / _speed)
  val ccWaitingPos: Pos = new Pos(216,59,"CC Waiting", new Duration(10 * millisToMin) / _speed)
  val ladleHeating2Pos: Pos = new Pos(327,78,"Heating 2", new Duration(10 * millisToMin) / _speed)
  val ladleHeating3Pos: Pos = new Pos(327,133,"Heating 3", new Duration(10 * millisToMin) / _speed)
  val refrMaintenancePos: Pos = new Pos(331,216,"Refractory Maintenance", new Duration(15 * millisToMin) / _speed)
  val ladleHeating5Pos: Pos = new Pos(318,279,"Heating 5", new Duration(10 * millisToMin) / _speed)
  val processTransition: ParallelTransition = new ParallelTransition()

  Range(0,nbLadle,1).foreach(i => {
    this.ladleArray(i) = new Ladle(this.startPos,"ladle " + (i + 1).toString + " info",i + 1,30) {setStyleSheet("Ladle.css")}
    this.processTransition.children.add(this.process(this.ladleArray(i)))
    this.visualPlan.stackPane.children.add(this.ladleArray(i))
    this.visualPlan.addLadleState(this.ladleArray(i))
  })

  def process(ladle: Ladle): SequentialTransition = {
    val SeqTransition = new SequentialTransition(ladle)
    SeqTransition.children.addAll(
      new PauseTransition(new Duration(1000)),
      ladle.Pos = eafPos,
      new PauseTransition(new Duration(1000)),
      ladle.Pos = mechMaintenancePos,
      ladle.Pos = refrMaintenancePos
    )
    SeqTransition
  }

  def speed: Double = this._speed
  def speed_=(s: Double): Unit = {
    this._speed = s
    this.getClass.getDeclaredFields.filter(_.getName.endsWith("Pos")).foreach(field => {
      field.setAccessible(true)
      field.get(this).asInstanceOf[Pos].duration = field.get(this).asInstanceOf[Pos].duration / s
    })
  }

  visualPlan.speedSlider.valueProperty().addListener(new ChangeListener[Number] {
    override def changed(observable: ObservableValue[_ <: Number], oldValue: Number, newValue: Number): Unit = {
      Scheduler.this.speed = 1 / oldValue.doubleValue()
      Scheduler.this.speed = newValue.doubleValue()
      visualPlan.speedLabel.text = "x%d".format(Scheduler.this.speed.toInt)
    }
  })

  visualPlan.startButton.onAction = event => {
    this.processTransition.play()
  }

  visualPlan.pauseButton.onAction = event => {
    this.processTransition.pause()
  }

  visualPlan.stopButton.onAction = event => {
    this.processTransition.stop()
  }

}

object SchedulerExamples extends JFXApp {
  val visualPlan = new VisualPlan("TracOpt")
  val scheduler = new Scheduler(visualPlan, 2)
  visualPlan.visualFrame.showStage()
  val ladle = new Ladle(scheduler.startPos,"ladle info",3)
  visualPlan.addLadleState(ladle)
  visualPlan.stackPane.children.add(ladle)
  visualPlan.visualFrame.stage.sizeToScene()
  visualPlan.visualFrame.stage.centerOnScreen()
  ladle.toFront()
  scheduler.process(ladle)
}