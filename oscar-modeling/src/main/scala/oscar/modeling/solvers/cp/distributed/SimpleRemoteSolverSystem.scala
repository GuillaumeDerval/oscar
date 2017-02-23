/*******************************************************************************
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License  for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
 ******************************************************************************/

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