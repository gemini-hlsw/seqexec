// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package enum

import cats.syntax.eq._
import cats.instances.string._
import gem.util.Enumerated

/**
 * Enumerated type for Flamingos2 window cover state.
 * @group Enumerations (Generated)
 */
sealed abstract class F2WindowCover(
  val tag: String,
  val shortName: String,
  val longName: String
) extends Product with Serializable

object F2WindowCover {

  /** @group Constructors */ case object Open extends F2WindowCover("Open", "Open", "Open")
  /** @group Constructors */ case object Close extends F2WindowCover("Close", "Close", "Close")

  /** All members of F2WindowCover, in canonical order. */
  val all: List[F2WindowCover] =
    List(Open, Close)

  /** Select the member of F2WindowCover with the given tag, if any. */
  def fromTag(s: String): Option[F2WindowCover] =
    all.find(_.tag === s)

  /** Select the member of F2WindowCover with the given tag, throwing if absent. */
  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def unsafeFromTag(s: String): F2WindowCover =
    fromTag(s).getOrElse(throw new NoSuchElementException(s))

  /** @group Typeclass Instances */
  implicit val F2WindowCoverEnumerated: Enumerated[F2WindowCover] =
    new Enumerated[F2WindowCover] {
      def all = F2WindowCover.all
      def tag(a: F2WindowCover) = a.tag
    }

}