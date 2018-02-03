package LinearAlgebra

class vectorN(seq: Double*) extends Traversable[Double] with Iterable[Double] {
  override def foreach[U](f: Double => U): Unit = seq.foreach(f)

  override def iterator: Iterator[Double] = seq.toIterator

  override def toString(): String = "(" + this.mkString(", ") + ")"

  def apply(n: Int): Double = seq(n)

  def +(other: vectorN): vectorN = {
    new vectorN ((this zip other).map({case (x,y) => x + y}).toSeq:_*)
  }

  def -(other: vectorN): vectorN = this + new vectorN(other.map(x => -x).toSeq:_*)

  def *(alpha: Double): vectorN = new vectorN(this.map(x => alpha * x).toSeq:_*)

  def dot(other: vectorN): Double = {
    this zip other map {case (x,y) => x * y} sum
  }

  lazy val norm: Double = math.sqrt(this dot this)
}
