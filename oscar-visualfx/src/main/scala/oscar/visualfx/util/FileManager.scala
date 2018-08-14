package oscar.visualfx.util

/**
  * @author Rémi Barralis remi.barralis@yahoo.fr
  */

import java.io.File
import scalafx.stage.FileChooser.ExtensionFilter
import scalafx.stage.{FileChooser, Stage}

/**
  * Simple singleton that open a dialog window to open or save a file
  */
object FileManager {

  /**
    * Return the file the user opened or saved
    *
    * @param parentStage the window owning the dialog window
    * @param extension the string representing the extension of the file
    * @param open defines wether the dialog window is an open dialog or a save dialog
    * @return the file the user opened or saved
    */
  def getFile(parentStage: Stage, extension: String, open: Boolean = false) :File = {

    val fileChooser = new FileChooser()
    fileChooser.initialFileName = "snapshot"
    fileChooser.initialDirectory = new File("C:/Users/" + System.getProperty("user.name") + "/Pictures")
    fileChooser.getExtensionFilters.add(new ExtensionFilter("%s image".format(extension.toUpperCase), "*.%s".format(extension.toLowerCase)))

    val file = if (open) fileChooser.showOpenDialog(parentStage) else fileChooser.showSaveDialog(parentStage)
    file
  }

}
