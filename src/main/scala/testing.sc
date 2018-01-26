import java.awt.Color
import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO

import Utilities.Image

import scala.annotation.tailrec
import scala.util.Random.nextInt

val fileName = "/home/eric/Pictures/gus_01.jpg"

Image.fileToRgb(fileName)


val inputList: List[List[Double]] = List(
  List(1,2,3),
  List(4,5,6),
  List(7,8,9)
)

val flattenedRgb = for (a <- Image.fileToRgb(fileName); b <- a) yield b

val (assignments, clusters) = kMeans(flattenedRgb, 7)

val mappedData = assignments map (clusters(_))

val rawImage = ImageIO.read(new File(fileName))
val w = rawImage.getWidth
val h = rawImage.getHeight
val newImage = new BufferedImage(w, h, BufferedImage.TYPE_INT_RGB)

val indices = for (j <- 0 until h; i <- 0 until w) yield (j,i)

def rgbToColor(rgb: List[Double]): Int = (rgb(0) * 65536 + rgb(1) * 256 + rgb(2)).toInt

for (((i,j),k) <- indices.zipWithIndex) {
  newImage.setRGB(j, i, rgbToColor(mappedData(k)))
}

ImageIO.write(newImage, "jpg", new File("/home/eric/Pictures/test.jpg"))

//
