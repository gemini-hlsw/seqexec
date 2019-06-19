// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package enum

import cats.instances.string._
import cats.syntax.eq._
import gem.util.Enumerated

/**
 * Enumerated type for track types.
 * @group Enumerations (Generated)
 */
sealed abstract class TrackType(
  val tag: String
) extends Product with Serializable

object TrackType {

  /** @group Constructors */ case object Sidereal extends TrackType("Sidereal")
  /** @group Constructors */ case object Nonsidereal extends TrackType("Nonsidereal")

  /** All members of TrackType, in canonical order. */
  val all: List[TrackType] =
    List(Sidereal, Nonsidereal)

  /** Select the member of TrackType with the given tag, if any. */
  def fromTag(s: String): Option[TrackType] =
    all.find(_.tag === s)

  /** Select the member of TrackType with the given tag, throwing if absent. */
  def unsafeFromTag(s: String): TrackType =
    fromTag(s).getOrElse(throw new NoSuchElementException(s"TrackType: Invalid tag: '$s'"))

  /** @group Typeclass Instances */
  implicit val TrackTypeEnumerated: Enumerated[TrackType] =
    new Enumerated[TrackType] {
      def all = TrackType.all
      def tag(a: TrackType) = a.tag
      override def unsafeFromTag(s: String): TrackType =
        TrackType.unsafeFromTag(s)
    }

}