package oscar.anytime.lns.utils

import java.io.{File, FileWriter}

object IOUtils {
  def getFolderContent(dir: String):List[File] = {
    val d = new File(dir)
    if (d.exists && d.isDirectory) {
      d.listFiles.toList.sortBy(_.getName)
    } else {
      List[File]()
    }
  }

  def getFileName(path: String): String = {
    val file = new File(path)
    file.getName
  }

  def getParentName(path: String): String = {
    val file = new File(path)
    file.getParentFile.getName
  }

  def getInstanceFiles(dir: String, extension: String): List[(String, String, String)] = getFolderContent(dir)
    .filter(f => f.isDirectory || f.getName.endsWith(extension))
    .flatMap(f => {
      if(f.isDirectory) getInstanceFiles(dir + "/" + f.getName, extension)
      else List[(String, String, String)]((f.getName, f.getParentFile.getName, f.getAbsolutePath))
    })

  def saveToFile(filePath:String, s:String): Unit = {
    val output = new File(filePath)
    if(!output.exists()){
      output.getParentFile.mkdirs()
      output.createNewFile()
    }

    val writer = new FileWriter(output, true)

    writer.write(s)

    writer.close()
  }
}
