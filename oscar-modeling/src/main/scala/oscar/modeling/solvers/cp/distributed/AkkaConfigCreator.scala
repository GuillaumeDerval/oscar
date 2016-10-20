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
             sp = "solvers.cp.DoSubproblemSerializer"
           }
           serialization-bindings {
             "solvers.cp.SolvingMessage" = kryo
             "solvers.cp.HelloMessage" = kryo
             "solvers.cp.StartMessage" = kryo
             "solvers.cp.AwaitingSPMessage" = kryo
             "solvers.cp.DoneMessage" = kryo
             "solvers.cp.BoundUpdateMessage" = kryo
             "solvers.cp.AskForSolutionRecap" = kryo
             "solvers.cp.SolutionRecapMessage" = kryo
             "solvers.cp.AllDoneMessage" = kryo
             "solvers.cp.SolutionMessage" = kryo
             "models.Model" = kryo
             "models.ModelDeclaration" = kryo
             "solvers.cp.DoSubproblemMessage" = sp
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