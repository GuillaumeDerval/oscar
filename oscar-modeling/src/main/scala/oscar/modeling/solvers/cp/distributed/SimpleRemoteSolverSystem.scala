package oscar.modeling.solvers.cp.distributed

import java.nio.charset.Charset
import java.nio.file.{Files, Paths}

import akka.actor.{ActorSystem, ExtendedActorSystem}

import scala.concurrent.Await
import scala.concurrent.duration.Duration

/**
  * Created by dervalguillaume on 20/10/16.
  */
class SimpleRemoteSolverSystem(hostname: String, port: Int, registerDir: Option[String]) {
  val system = ActorSystem("solving", AkkaConfigCreator.remote(hostname, port))
  if(registerDir.isDefined) {
    val addr = system.asInstanceOf[ExtendedActorSystem].provider.getDefaultAddress
    val filename = addr.host.get+":"+addr.port.get
    val filepath = Paths.get(registerDir.get, filename)
    Files.write(filepath, scala.collection.JavaConverters.asJavaIterableConverter(List[CharSequence](addr.toString)).asJava, Charset.forName("UTF-8"))
  }
  Await.result(system.whenTerminated, Duration.Inf)
}