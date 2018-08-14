package oscar.visualfx.util

import java.awt.RenderingHints
import java.awt.image.BufferedImage
import java.io.File

import javax.imageio.{IIOImage, ImageIO, ImageTypeSpecifier}
import javax.imageio.metadata.{IIOMetadata, IIOMetadataNode}

import util.control.Breaks._

object ImageWriter {

  val formatName: String = "png"
  val DPI: Int = 250

  def saveImage(file: File, image: BufferedImage): Unit = {
    val iwIterator = ImageIO.getImageWritersByFormatName(formatName)
    while (iwIterator.hasNext) {
      val writer = iwIterator.next()
      val writeParam = writer.getDefaultWriteParam
      val typeSpecifier = ImageTypeSpecifier.createFromBufferedImageType(BufferedImage.TYPE_INT_RGB)
      val metadata = writer.getDefaultImageMetadata(typeSpecifier,writeParam)

      if (!(metadata.isReadOnly || !metadata.isStandardMetadataFormatSupported)) {
        setDPI(metadata)

        val stream = ImageIO.createImageOutputStream(file)
        writer.setOutput(stream)
        writer.write(metadata,new IIOImage(image,null,metadata), writeParam)
        stream.close()
      }
    }

  }

  def setDPI(metadata: IIOMetadata): Unit = {
    val dpi:Double = 1.0 * DPI / 25.4

    val horizontal: IIOMetadataNode = new IIOMetadataNode("HorizontalPixelSize")
    horizontal.setAttribute("value", dpi.toString)

    val vertical: IIOMetadataNode = new IIOMetadataNode("VerticalPixelSize")
    vertical.setAttribute("value", dpi.toString)

    val dimension: IIOMetadataNode = new IIOMetadataNode("Dimension")
    dimension.appendChild(horizontal)
    dimension.appendChild(vertical)

    val root: IIOMetadataNode = new IIOMetadataNode("javax_imageio_1.0")
    root.appendChild(dimension)

    metadata.mergeTree("javax_imageio_1.0", root)
  }

}
