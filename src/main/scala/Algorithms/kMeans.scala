package Algorithms

import scala.annotation.tailrec
import scala.util.Random.nextInt
import Utilities.Vector._

class kMeans(numClusters: Int) {

  // The main function to call
  def cluster(data: Vector[List[Double]]): (Vector[Int], Vector[List[Double]]) = {

    // Some utility functions
    def distanceMoved(c1: Vector[List[Double]], c2: Vector[List[Double]]): Double = {
      (c1 zip c2).map(x => l2Dist(x._1,x._2)).sum
    }

    def assign(centers: Vector[List[Double]], dataPoint: List[Double]): Int =
      (centers map (l2Dist(_, dataPoint))).zipWithIndex.min._2

    def findCenter(i: Int, assignments: Vector[Int]): List[Double] = {
      (for ((j, c) <- assignments.zipWithIndex.toList if j == i) yield data(c)) match {
        case Nil => Nil
        case points => points.foldLeft(points.head.map(_ => 0.toDouble)){(acc, l) => listAdd(acc, l)}.map(_ / points.length.toDouble)
      }
    }

    @tailrec
    def iterate(centers: Vector[List[Double]]): (Vector[Int], Vector[List[Double]]) = {
      // Get assignment
      val assignments = data map(assign(centers, _))

      // Update centers
      val newCenters: Vector[List[Double]] = for (i <- centers.indices.toVector) yield findCenter(i, assignments)

      // Stopping condition
      val moved: Double = distanceMoved(centers, newCenters)
      if (moved <= 0.01)
        (assignments, newCenters)
      else iterate(newCenters)
    }

    // Randomly assign initial clusters
    val initialClusters =
      for (i <- (0 until numClusters).toVector) yield data(nextInt(data.length - 1))

    // Execute the main iteration
    iterate(initialClusters)
  }
}
