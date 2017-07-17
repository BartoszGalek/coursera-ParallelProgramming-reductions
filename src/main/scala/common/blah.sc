import scala.collection.parallel.Task
val threshold: Int = 10

trait Splitter[T] {
  def split: Seq[Splitter[T]]
  def remaining: Int
}

trait Iterator[T] {
  def hasNext: Boolean
  def next(): T
  def foldLeft[S](z: S)(f: (S, T) => S): S = {
    var result = z
    while (hasNext) result = f(result,next)

    result
  }

}

trait ItterableSplitter[T] extends  with Iterator[T] with Splitter[T] {
  def fold(z: T)(f: (T, T) => T): T = {
    if (remaining < threshold) foldLeft(z)(f)
    else {
      val children: Seq[Task[T]] = ???
      children.map(_.join()).foldLeft(z)(f)
    }
  }
}

