// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.util

import cats._, cats.data._, cats.implicits._
import scala.BigDecimal.RoundingMode.FLOOR
import scala.annotation.tailrec
import scala.collection.breakOut

/** A sortable value used to indicate relative positions of a set of associated
  * elements.  `Location`s may be thought of as lists of arbitrary integers
  * which are be compared element by element such that the first pair of values
  * that differ determine the ordering of the `Location`s as a whole.  If one
  * `Location` is a proper prefex of another, then it sorts ahead of the other
  * `Location`.
  * @group Sequence Model
  */
sealed trait Location extends Product with Serializable {

  // These functions aren't of any use to clients.  Instead they are involved
  // in the cacluation of Locations that fall between two other locations.

  /** Infinite Stream of position elements corresponding to this Location. */
  protected def positions: Stream[Int]

  /** Minimum size required to obtain all the fixed elements in this Location,
    * if any.  Beginning and End Locations have no fixed elements.
    */
  protected def minPrefixLength: Int

  final override def toString: String =
    this match {
      case Location.Beginning  => "{α}"
      case Location.Middle(ps) => "{" + ps.map(_.toString).intercalate(",") + "}"
      case Location.End        => "{ω}"
    }

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

  }

  /** A Location that falls somewhere between the Beginning and End.
    */
  sealed abstract case class Middle(posList: NonEmptyList[Int]) extends Location {
    def positions: Stream[Int] =
      posList.toList.toStream #::: Stream.continually(Int.MinValue)

    def toList: List[Int] =
      posList.toList

    protected def minPrefixLength: Int =
      posList.size

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

  }

  val beginning: Location = Beginning
  val end: Location       = End

  // Constructors

  def apply(is: Int*): Location =
    fromFoldable(is.toList)

  def fromFoldable[F[_]: Foldable](fi: F[Int]): Location =
    fi.foldRight(Eval.now(List.empty[Int])) { (i, elst) =>
      elst.map { lst =>
        if (lst.isEmpty && i === Int.MinValue) lst else (i :: lst)
      }
    }.value match {
      case h :: t => new Middle(NonEmptyList(h, t)) {}
      case _      => Beginning
    }

  /** Assuming not all provided Ints are `Int.MinValue`, produces a `Middle`
    * `Location`.
    */
  def unsafeMiddle(is: Int*): Middle =
    unsafeMiddleFromFoldable(is.toList)

  /** Assuming not all provided Ints are `Int.MinValue`, produces a `Middle`
    * `Location`.
    */
  def unsafeMiddleFromFoldable[F[_]: Foldable](fi: F[Int]): Middle =
    fromFoldable(fi) match {
      case m: Middle => m
      case _         => sys.error("Location arguments do not form a Middle position: " + fi.toList.mkString(", "))
    }


  // Utility

  private object Base10 {
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
      def go(rem: BigInt, tail: List[Int]): Middle = {
        val (a, b) = rem /% Radix
        val head   = (b + Min).intValue
        if (a === Zero) new Middle(NonEmptyList(head, tail)) {} else go(a, head +: tail)
      }
      go(bi, List.empty)
    }
  }

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
  def find(count: Int, start: Location, end: Location): List[Middle] = {
    import Base10._

    @tailrec
    def go(len: Int): List[Middle] = {
      val start10 = toBase10(start, len)
      val end10   = toBase10(end, len)

      // (end10 - start10) is the space we have to fill.  We need count + 1
      // more or less evenly spaced gaps between numbers in that space.  So
      // total space / gaps needed is the decimal gap size.  It has to be at
      // least 1.0 or else we don't have enough space to accommodate `count`
      // new elements and we have to recurse.
      val gapSize = BigDecimal.exact(end10 - start10) / (count + 1)

      if (gapSize < BigDecimal.exact(1)) go(len + 1)
      else {
        // This is the existing start position as a BigDecimal.
        val startBd = BigDecimal(start10, 0)

        // Calculate count digits separated one from the other by gapSized gaps,
        // but rounding down to make them integral. Since gapSize is at least
        // 1.0, this will always advance and never produce duplicates.
        (1 to count)
          .scanLeft(startBd) { (sum, _) => sum + gapSize }
          .drop(1)
          .map { bd => fromBase10(bd.setScale(0, FLOOR).toBigInt) }(breakOut)
      }
    }

    if ((count <= 0) || (start >= end)) Nil
    else go(start.minPrefixLength max end.minPrefixLength)
  }

  // Type Classes

  implicit val OrderLocation: Order[Location] = Order.from {
    case (Beginning,  Beginning )                => 0
    case (End,        End       )                => 0
    case (Middle(m0), Middle(m1)) if (m0 === m1) => 0
    case (l0,         l1        )                =>
      l0.positions.zip(l1.positions)
        .find { case (a, b) => a =!= b }
        .foldMap { case (a, b) => a compare b }
  }

  implicit val OrderMiddle: Order[Location.Middle] =
    OrderLocation.contramap[Location.Middle](lm => lm: Location)

  implicit val ShowLocation: Show[Location] = Show.fromToString
}
