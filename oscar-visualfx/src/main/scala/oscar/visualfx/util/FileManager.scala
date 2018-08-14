package oscar.visualfx.util

import java.io.File

import scalafx.stage.FileChooser.ExtensionFilter
import scalafx.stage.{FileChooser, Stage}

class FileManager {

  def getFile(parentStage: Stage, open: Boolean, extension: String) :File = {

    val fileChooser = new FileChooser()
    fileChooser.initialFileName = "snapshot"
    fileChooser.initialDirectory = new File("C:/Users/" + System.getProperty("user.name") + "/Pictures")
    fileChooser.getExtensionFilters.add(new ExtensionFilter("%s image".format(extension.toUpperCase), "*.%s".format(extension.toLowerCase)))

    val file = if (open) fileChooser.showOpenDialog(parentStage) else fileChooser.showSaveDialog(parentStage)
    file
  }

}
