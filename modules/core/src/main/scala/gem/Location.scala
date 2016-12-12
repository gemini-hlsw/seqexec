package gem

import scala.annotation.tailrec
import scalaz._, Scalaz._
import scalaz.Ordering.{EQ, GT, LT}

/** A sortable value used to indicate relative positions of a set of associated
  * elements.  `Location`s may be thought of as lists of arbitrary integers
  * which are be compared element by element such that the first pair of values
  * that differ determine the ordering of the `Location`s as a whole.  If one
  * `Location` is a proper prefex of another, then it sorts ahead of the other
  * `Location`.
  */
sealed trait Location {

  // These functions aren't of any use to clients.  Instead they are involved
  // in the cacluation of Locations that fall between two other locations.

  /** Infinite Stream of position elements corresponding to this Location. */
  protected def positions: Stream[Int]

  /** Minimum size required to obtain all the fixed elements in this Location,
    * if any.  Beginning and End Locations have no fixed elements.
    */
  protected def minPrefixLength: Int
}

object Location {

  /** A marker for the first Location.  No other Location comes before the
    * Beginning.  Beginning should not be used to order elements unless you
    * are sure no elements will ever need to be inserted before Beginning.  Use
    * Beginning to calculate Locations that fall before the first existing
    * Location (if any).
    */
  case object Beginning extends Location {
    protected def positions: Stream[Int] =
      Stream.continually(Int.MinValue)

    protected def minPrefixLength: Int =
      0

    override def toString: String =
      "{α}"
  }

  /** A Location that falls somewhere between the Beginning and End.
    */
  sealed abstract case class Middle(posList: OneAnd[Vector, Int]) extends Location {
    def positions: Stream[Int] =
      posList.toStream #::: Stream.continually(Int.MinValue)

    def toIList: IList[Int] =
      IList.fromFoldable(posList.head +: posList.tail)

    def toList: List[Int] =
      posList.head +: posList.tail.toList

    protected def minPrefixLength: Int =
      1 + posList.tail.length

    override def toString =
      toList.mkString("{", ",", "}")
  }

  /** A marker for the last Location.  No other Location comes after the End.
    * End should not be used to order elements unless you are sure no elements
    * will ever need to be inserted after End.  Use End to calculate new
    * Locations that fall after the last existing Location (if any).
    */
  case object End extends Location {
    def positions: Stream[Int] =
      Stream.continually(Int.MaxValue)

    protected def minPrefixLength: Int =
      0

    override def toString: String =
      "{ω}"
  }

  // Constructors

  def apply(is: Int*): Location =
    fromTrimmedFoldable(is)(new Foldable[Seq] with Foldable.FromFoldr[Seq] {
      def foldRight[A, B](fa: Seq[A], z: => B)(f: (A, => B) => B): B =
        fa.foldRight(z) { (a, b) => f(a, b) }
    })

  def fromFoldable[F[_]: Foldable](fi: F[Int]): Location =
    fromTrimmedFoldable(fi)

  def fromTrimmedFoldable[F[_]: Foldable](fi: F[Int]): Location = {
    fi.foldRight(Vector.empty[Int]) { (i, v) =>
      (v.isEmpty && i === Int.MinValue) ? v | i +: v
    } match {
      case h +: t => new Middle(OneAnd(h, t)) {}
      case _      => Beginning
    }
  }

  /** Assuming not all provided Ints are `Int.MinValue`, produces a `Middle`
    * `Location`.
    */
  def unsafeMiddle(i: Int, is: Int*): Middle =
    unsafeMiddle(i :: is.toList)

  /** Assuming not all provided Ints are `Int.MinValue`, produces a `Middle`
    * `Location`.
    */
  def unsafeMiddle[F[_]: Foldable](fi: F[Int]): Middle =
    fromFoldable(fi) match {
      case m: Middle => m
      case _         => sys.error("Location arguments do not form a Middle position: " + fi.toList.mkString(", "))
    }


  // Utility

  /** Finds `count` `Location`s that fall evenly distributed between `l0` and
    * `l1`, assuming these locations are not the same.
    *
    * @param count how many locations to find
    * @param start starting location (exclusive)
    * @param end ending Location (exclusive)
    *
    * @return sorted list of `Location` where every element is GT l0 and LT l1
    *         (or vice versa if l0 is GT l1)
    */
  def find(count: Int, start: Location, end: Location): IList[Middle] = {
    val Zero  = BigInt(0)
    val One   = BigInt(1)
    val Max   = BigInt(Int.MaxValue)
    val Min   = BigInt(Int.MinValue)
    val Radix = Max + Min.abs + One

    def toBase10(loc: Location, len: Int): BigInt =
      loc.positions.take(len).foldRight((Zero, One)) { case (i, (acc, pow)) =>
        (acc + (BigInt(i) + Min.abs) * pow, pow * Radix)
      }._1

    def fromBase10(bi: BigInt): Middle = {
      @tailrec
      def go(rem: BigInt, tail: Vector[Int]): Middle = {
        val (a, b) = rem /% Radix
        val head   = (b + Min).intValue
        if (a === Zero) new Middle(OneAnd(head, tail)) {} else go(a, head +: tail)
      }
      go(bi, Vector.empty)
    }

    @tailrec
    def go(len: Int): IList[Middle] = {
      val start10 = toBase10(start, len)
      val end10   = toBase10(end, len)
      val avail   = end10 - start10 - One

      if (avail < count)
        go(len + 1)
      else {
        val incr = (BigDecimal.exact(avail) / (count + 1)).setScale(0, BigDecimal.RoundingMode.CEILING).toBigInt
        IList.fromList((1 to count).toList.map(i => fromBase10(start10 + (incr * i))))
      }
    }

    if ((count <= 0) || (start >= end)) INil[Middle]
    else go(start.minPrefixLength max end.minPrefixLength)
  }

  // Type Classes

  implicit val OrderLocation: Order[Location] = Order.order(Function.untupled {
    case (Beginning,  Beginning )                => EQ
    case (End,        End       )                => EQ
    case (Middle(m0), Middle(m1)) if (m0 === m1) => EQ
    case (l0,         l1        )                =>
      l0.positions.zip(l1.positions)
        .find { case (a, b) => a =/= b }
        .fold[Ordering](EQ) { case (a, b) => (a < b) ?[Ordering] LT | GT }
  })

  implicit val ShowLocation: Show[Location] = Show.shows(_.toString)
}
