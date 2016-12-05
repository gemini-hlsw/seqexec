package gem

import scalaz._, Scalaz._
import scalaz.Ordering.{EQ, GT, LT}

/** A sortable value used to indicate relative positions of a set of associated
  * elements.  `Location`s may be thought of as lists of arbitrary integers
  * which are be compared element by element such that the first pair of values
  * that differ determine the ordering of the `Location`s as a whole.  If one
  * `Location` is a proper prefex of another, then it sorts ahead of the other
  * `Location`.
  */
sealed abstract case class Location(toIList: IList[Int]) {

  // Use the sketchy sealed abstract case class technique to control the
  // NonEmptyList values that are passed to the constructor.  We want to
  // ensure that trailing Int.MinValue is always trimmed so that EQ and ==
  // agree.

  def toList: List[Int] =
    toIList.toList

  override def toString: String =
    toList.mkString("{", ",", "}")
}

object Location {
  private def trim(is: Seq[Int]): IList[Int] =
    trim(IList(is: _*))

  // Remove trailing Int.MinValue so that == agrees with EQ.
  private def trim(is: IList[Int]): IList[Int] =
    is.dropRightWhile(_ == Int.MinValue)

  def apply(is: Int*): Location =
    new Location(trim(is)) {}

  def fromList(is: List[Int]): Location =
    new Location(trim(is)) {}

  def fromIList(is: IList[Int]): Location =
    new Location(trim(is)) {}

  def fromNel(n: NonEmptyList[Int]): Location =
    new Location(trim(n.list)) {}

  implicit val OrderLocation: Order[Location] = Order.order { (l0, l1) =>
    l0.toList
      .zipAll(l1.toList, Int.MinValue, Int.MinValue)
      .find { case (a, b) => a =/= b }
      .fold[Ordering](EQ) { case (a, b) => (a < b) ?[Ordering] LT | GT }
  }

  implicit val ShowLocation: Show[Location] = Show.shows(_.toString)
}
