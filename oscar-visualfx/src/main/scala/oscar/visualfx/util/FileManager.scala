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
