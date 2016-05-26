package oscar

package object linprog {
  implicit class RichPath(path: java.nio.file.Path) {
    val extension: String = {
      val name = path.getFileName.toString
      name.substring(name.lastIndexOf('.')+1, name.length)
    }
  }
}
