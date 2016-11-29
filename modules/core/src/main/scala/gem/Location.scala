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
final case class Location(toNel: NonEmptyList[Int]) {
  def toIList: IList[Int] =
    toNel.list

  def toList: List[Int] =
    toIList.toList

  override def toString: String =
    toList.mkString("{", ",", "}")
}

object Location {
  def apply(h: Int, t: Int*): Location =
    Location(NonEmptyList(h, t: _*))

  def fromList(is: List[Int]): Option[Location] =
    is match {
      case Nil    => None
      case h :: t => Some(Location(NonEmptyList.nel(h, IList.fromList(t))))
    }

  def unsafeFromList(is: List[Int]): Location =
    fromList(is).get

  private val LocationRegex = """^\{(\s*[-+]?[0-9]+\s*(?:,\s*[-+]?[0-9]+\s*)*)\}$""".r

  def parse(s: String): Option[Location] =
    s match {
      case LocationRegex(ds) => Location.fromList(ds.split(',').toList.map(_.trim.toInt))
      case _                 => None
    }

  def unsafeParse(s: String): Location =
    parse(s).getOrElse(sys.error(s"Could not parse location '$s'"))

  implicit val OrderLocation: Order[Location] = Order.order { (l0, l1) =>
    l0.toList
      .zipAll(l1.toList, Int.MinValue, Int.MinValue)
      .find { case (a, b) => a =/= b }
      .fold[Ordering](EQ) { case (a, b) => (a < b) ?[Ordering] LT | GT }
  }

  implicit val ShowLocation: Show[Location] = Show.shows(_.toString)

}
