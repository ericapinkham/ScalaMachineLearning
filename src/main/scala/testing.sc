


val x = List[Double](1, 2, 3, 4)
val y = List[Double](5, -1, 5, 1)


Math.pow(x.map(Math.pow(_, 2)).sum, 1 / 2)