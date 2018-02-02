package Utilities

import java.awt.Color
import java.io.File
import javax.imageio.ImageIO

object Image {
  def fileToRgb(filePath: String): Vector[Vector[List[Double]]] = readRgbValues(openFile(filePath))

  private def openFile(filePath: String): java.awt.image.BufferedImage = ImageIO.read(new File(filePath))

  private def readRgbValues(image: java.awt.image.BufferedImage): Vector[Vector[List[Double]]] = {
    def rgbValues(c: Color): List[Double] = List(c.getRed, c.getGreen, c.getBlue) map (_.toDouble)

    val w = image.getWidth
    val h = image.getHeight

    for (i <- (0 until h).toVector) yield
      for (j <- (0 until w).toVector) yield
        rgbValues(new Color(image.getRGB(j, i)))
  }

  def rgbToColor(rgb: List[Double]): Int = new Color(rgb.head.toInt, rgb(1).toInt, rgb(2).toInt).getRGB
}
