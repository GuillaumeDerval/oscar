package oscar.linprog.enums

import java.io.FileOutputStream

/**
 * Describes a potential output for the log
 */
sealed abstract class LogOutput(val stream: java.io.OutputStream) {
  /**
   * Cleanly closes this logging output
   */
  def close(): Unit
}

/**
 * A [[java.io.OutputStream]] writing to nowhere
 */
class NullOutputStream extends java.io.OutputStream {
  def write(b: Int) = ()
}

/**
 * Basically wraps a [[NullOutputStream]]
 * so that the log is not written.
 */
case object DisabledLogOutput extends LogOutput(new NullOutputStream) {
  def close() = stream.close()

  override def toString = "disabled"
}

/**
 * The "standard" output, that is the console.
 */
case object StandardLogOutput extends LogOutput(System.out) {
  def close() = () // Do not close the "standard" output stream

  override def toString = "standard"
}

/**
 * Basically wraps the [[java.io.File]] pointed by the given [[java.nio.file.Path]]
 * so that the log is written to this file.
 */
case class FileLogOutput(path: java.nio.file.Path) extends LogOutput(new FileOutputStream(path.toFile)) {
  val file = path.toFile
  def close() = stream.close()

  override def toString = path.toString
}

object LogOutput {
  def disabled = DisabledLogOutput

  /**
   * Returns the "standard" log output, that is the console.
   */
  def standard = StandardLogOutput

  def file(path: java.nio.file.Path) = FileLogOutput(path)
}