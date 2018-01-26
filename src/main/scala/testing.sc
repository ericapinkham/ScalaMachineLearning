import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO
import Algorithms.kMeans
import Utilities.Image

// Open the file and flatten
val fileName = "/home/eric/Pictures/gus_01.jpg"
val flattenedRgb = for (a <- Image.fileToRgb(fileName); b <- a) yield b

// Setup the kmeans class
val mykMeans = new kMeans(7)
val (assignments, centers) = mykMeans.cluster(flattenedRgb)

// Apply the clustering to the data
val mappedData = assignments map (centers(_))
val rawImage = ImageIO.read(new File(fileName))
val w = rawImage.getWidth
val h = rawImage.getHeight
val newImage = new BufferedImage(w, h, BufferedImage.TYPE_INT_RGB)

// Write the new jpg
val indices = for (j <- 0 until h; i <- 0 until w) yield (j,i)

def rgbToColor(rgb: List[Double]): Int = (rgb.head * 65536 + rgb(1) * 256 + rgb(2)).toInt

for (((i,j),k) <- indices.zipWithIndex) {
  newImage.setRGB(j, i, rgbToColor(mappedData(k)))
}

ImageIO.write(newImage, "jpg", new File("/home/eric/Pictures/test.jpg"))
