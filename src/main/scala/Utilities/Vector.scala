package Utilities
object Vector {

  // Vector addition and subtraction
  def listAdd(xs: List[Double], ys: List[Double]): List[Double] = (xs zip ys) map {case (x,y) => x + y}
  def listSub(xs: List[Double], ys: List[Double]): List[Double] = listAdd(xs, ys.map(y => -y))

  // the lp distances
  def lpDist(p: Int)(x: List[Double], y: List[Double]): Double = {
    require(p >= 1)
    lpNorm(p)(listSub(x, y))
  }

  // Usually we'll use this
  def l2Dist: (List[Double], List[Double]) => Double = lpDist(2)

  // Define the lp norms
  def lpNorm(p: Double)(x: List[Double]): Double = {
    require(p >= 1)
    Math.pow(x.map(Math.pow(_, p)).sum, 1 / p)
  }

  // Most commonly we'll use this
  def l2Norm: List[Double] => Double = lpNorm(2)

}
