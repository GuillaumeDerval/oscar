package scampi.des.engine

import scala.util.continuations._
import scala.react._

class Frequency(m: Model, state: State[_]){

  var duration:Double = 0
  var last:Double = 0
//  
//  Reactor.loop{self=>
//    val last = m.time(self next state.atEntry)
//    duration += m.time(self next state.atLeaving) - last
//  }
  
  def apply() = duration/m.clock()
  
}