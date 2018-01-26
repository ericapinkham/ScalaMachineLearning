package Algorithms

import scala.annotation.tailrec
import scala.util.Random.nextInt

class kMeans {
  def kMeans(data: Vector[List[Double]], numClusters: Int) = {

    def listAdd(xs: List[Double], ys: List[Double]): List[Double] = (xs zip ys) map {case (x,y) => x + y}
    def listSub(xs: List[Double], ys: List[Double]): List[Double] = listAdd(xs, ys.map(y => -y))

    def l2Dist(xs: List[Double], ys: List[Double]): Double = {
      //require(xs.lengthCompare(ys.length) == 0)
      Math.sqrt(listSub(xs,ys).map(Math.pow(_, 2)).sum)
    }

    def assign(centers: Vector[List[Double]], dataPoint: List[Double]): Int =
      (centers map (l2Dist(_, dataPoint))).zipWithIndex.min._2

    def findCenter(i: Int, assignments: Vector[Int]): List[Double] = {
      //println((for ((j, c) <- assignments.zipWithIndex.toList if j == i) yield data(c)))
      (for ((j, c) <- assignments.zipWithIndex.toList if j == i) yield data(c)) match {
        case Nil => Nil
        case points => points.foldLeft(points.head.map(_ => 0.toDouble)){(acc, l) => listAdd(acc, l)}.map(_ / points.length.toDouble)
      }
    }

    @tailrec
    def iterate(centers: Vector[List[Double]]): (Vector[Int], Vector[List[Double]]) = {
      // define the stopping condition
      def distanceMoved(c1: Vector[List[Double]], c2: Vector[List[Double]]): Double = {
        (c1 zip c2).map(x => l2Dist(x._1,x._2)).sum
      }
      // Get assignment
      val assignments = data map(assign(centers, _))

      // Update centers
      val newCenters: Vector[List[Double]] = for (i <- centers.indices.toVector) yield findCenter(i, assignments)

      val moved: Double = distanceMoved(centers, newCenters)
      //println(moved)
      if (moved <= 0.01)
        (assignments, newCenters)
      else iterate(newCenters)
    }

    val initialClusters =
      for (i <- (0 until numClusters).toVector) yield data(nextInt(data.length - 1))

    //println(initialClusters)
    iterate(initialClusters)
  }
}
