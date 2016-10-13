package oscar.modeling.misc.scallop

import java.io.File

/**
  * Specific converter for Scallop and hostname:port pairs
  */
object HostnameParser {
  val hostnameRegex = """^(([a-zA-Z0-9]|[a-zA-Z0-9][a-zA-Z0-9\-]*[a-zA-Z0-9])\.)*([A-Za-z0-9]|[A-Za-z0-9][A-Za-z0-9\-]*[A-Za-z0-9])$""".r
  val ipRegex = """^(([0-9]|[1-9][0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-5])\.){3}([0-9]|[1-9][0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-5])$""".r

  def parse(value: String): (String, Int) = {
    value.split(":").toList match {
      case host :: port :: Nil => parsePair(host, port)
      case host :: Nil => parsePair(host, 2001)
    }
  }

  def parseFromFile(filePath: String): List[(String, Int)] = {
    val file = new File(filePath)
    if(!file.exists())
      throw new Exception("File "+filePath+" does not exists")
    io.Source.fromFile(file).getLines().filter(_.trim() != "").map(s => parse(s)).toList
  }

  def parsePair(host: String, port: Int): (String, Int) = {
    if(hostnameRegex.findFirstIn(host).isEmpty && ipRegex.findFirstIn(host).isEmpty)
      throw new Exception("Invalid host/ip")
    if(port > 65535)
      throw new Exception("Invalid port")

    (host, port)
  }

  def parsePair(host: String, port: String):(String, Int) = {
    parsePair(host, Integer.parseInt(port))
  }
}
