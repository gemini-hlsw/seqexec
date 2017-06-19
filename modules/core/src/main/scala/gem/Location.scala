/*
 * Copyright (c) 2017, Association of Universities for Research in Astronomy, Inc. (AURA)
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this
 *    list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 * 3. Neither the name of the copyright holder nor the names of its contributors
 *    may be used to endorse or promote products derived from this software without
 *    specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

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
sealed trait Location extends Product with Serializable {

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
  sealed abstract case class Middle(posList: NonEmptyList[Int]) extends Location {
    def positions: Stream[Int] =
      posList.toStream #::: Stream.continually(Int.MinValue)

    def toIList: IList[Int] =
      posList.list

    def toList: List[Int] =
      toIList.toList

    protected def minPrefixLength: Int =
      posList.size

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

  val beginning: Location = Beginning
  val end: Location       = End

  // Constructors

  private implicit object FoldableSeq extends Foldable[Seq] with Foldable.FromFoldr[Seq] {
    def foldRight[A, B](fa: Seq[A], z: => B)(f: (A, => B) => B): B =
      fa.foldRight(z) { (a, b) => f(a, b) }
  }

  def apply(is: Int*): Location =
    fromFoldable(is)

  def fromFoldable[F[_]: Foldable](fi: F[Int]): Location =
    fi.foldRight(IList.empty[Int]) { (i, lst) =>
      (lst.isEmpty && i === Int.MinValue) ? lst | i +: lst
    } match {
      case ICons(h, t) => new Middle(NonEmptyList.nel(h, t)) {}
      case _           => Beginning
    }

  /** Assuming not all provided Ints are `Int.MinValue`, produces a `Middle`
    * `Location`.
    */
  def unsafeMiddle(is: Int*): Middle =
    unsafeMiddle(is)

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
      def go(rem: BigInt, tail: IList[Int]): Middle = {
        val (a, b) = rem /% Radix
        val head   = (b + Min).intValue
        if (a === Zero) new Middle(NonEmptyList.nel(head, tail)) {} else go(a, head +: tail)
      }
      go(bi, IList.empty)
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
        .fold[Ordering](EQ) { case (a, b) => if (a < b) LT else GT }
  })

  implicit val OrderMiddle: Order[Location.Middle] =
    OrderLocation.contramap[Location.Middle](lm => lm: Location)

  implicit val ShowLocation: Show[Location] = Show.shows(_.toString)
}
