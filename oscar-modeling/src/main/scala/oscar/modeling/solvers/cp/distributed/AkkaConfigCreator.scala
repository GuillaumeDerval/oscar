package oscar.modeling.solvers.cp.distributed

import com.typesafe.config.{Config, ConfigFactory}

/**
  * Stores the default Akka config
  */
object AkkaConfigCreator {
  def remote(hostname: String, port: Int): Config = {
    ConfigFactory.parseString(s"""
       akka {
         #loglevel = "DEBUG"
         actor {
           provider = "akka.remote.RemoteActorRefProvider"
           serializers {
             kryo = "com.twitter.chill.akka.AkkaSerializer"
             sp = "oscar.modeling.solvers.cp.distributed.DoSubproblemSerializer"
           }
           serialization-bindings {
             "oscar.modeling.solvers.cp.distributed.SolvingMessage" = kryo
             "oscar.modeling.solvers.cp.distributed.HelloMessage" = kryo
             "oscar.modeling.solvers.cp.distributed.StartMessage" = kryo
             "oscar.modeling.solvers.cp.distributed.AwaitingSPMessage" = kryo
             "oscar.modeling.solvers.cp.distributed.DoneMessage" = kryo
             "oscar.modeling.solvers.cp.distributed.BoundUpdateMessage" = kryo
             "oscar.modeling.solvers.cp.distributed.AskForSolutionRecap" = kryo
             "oscar.modeling.solvers.cp.distributed.SolutionRecapMessage" = kryo
             "oscar.modeling.solvers.cp.distributed.AllDoneMessage" = kryo
             "oscar.modeling.solvers.cp.distributed.SolutionMessage" = kryo
             "oscar.modeling.solvers.cp.distributed.ConfigMessage" = kryo
             "oscar.modeling.models.Model" = kryo
             "oscar.modeling.models.ModelDeclaration" = kryo
             "oscar.modeling.solvers.cp.distributed.DoSubproblemMessage" = sp
             "scala.spores.NullarySpore" = kryo
             "oscar.modeling.solvers.cp.distributed.ActorParameters" = kryo
           }
         }
         remote {
           #log-received-messages = on
           #log-frame-size-exceeding = 10b
           enabled-transports = ["akka.remote.netty.tcp"]
           netty.tcp {
             hostname = "$hostname"
             port = $port
           }
         }
       }
     """)
  }

  def local(): Config = {
    ConfigFactory.load()
  }
}