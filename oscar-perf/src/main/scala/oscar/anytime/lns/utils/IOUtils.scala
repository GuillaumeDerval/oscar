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

  def getFileName(path: String, keepExtension: Boolean = true): String = {
    val file = new File(path)
    if(keepExtension) file.getName
    else{
      val splitName = file.getName.split("\\.")
      if(splitName.nonEmpty) splitName(0)
      else file.getName
    }
  }

  def getParentName(path: String): String = {
    val file = new File(path)
    file.getParentFile.getName
  }

  def getFiles(dir: String, extension: String): List[File] = getFolderContent(dir)
    .filter(f => f.isDirectory || f.getName.endsWith(extension))
    .flatMap(f => {
      if(f.isDirectory) getFiles(dir + "/" + f.getName, extension)
      else List[File](f)
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
