// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package enum

import cats.instances.string._
import cats.syntax.eq._
import gem.util.Enumerated

/**
 * Enumerated type for guider.
 * @group Enumerations (Generated)
 */
sealed abstract class Guider(
  val tag: String,
  val instrument: Option[Instrument],
  val shortName: String,
  val longName: String
) extends Product with Serializable

object Guider {

  /** @group Constructors */ case object F2OI extends Guider("F2OI", Some(Instrument.Flamingos2), "F2 OI", "Flamingos2 OIWFS")
  /** @group Constructors */ case object GmosNOI extends Guider("GmosNOI", Some(Instrument.GmosN), "GMOS-N OI", "GMOS North OIWFS")
  /** @group Constructors */ case object GmosSOI extends Guider("GmosSOI", Some(Instrument.GmosS), "GMOS-S OI", "GMOS South OIWFS")
  /** @group Constructors */ case object P1GN extends Guider("P1GN", None, "P1 GN", "PWFS1 North")
  /** @group Constructors */ case object P2GN extends Guider("P2GN", None, "P2 GN", "PWFS2 North")
  /** @group Constructors */ case object P1GS extends Guider("P1GS", None, "P1 GS", "PWFS1 South")
  /** @group Constructors */ case object P2GS extends Guider("P2GS", None, "P2 GS", "PWFS2 South")

  /** All members of Guider, in canonical order. */
  val all: List[Guider] =
    List(F2OI, GmosNOI, GmosSOI, P1GN, P2GN, P1GS, P2GS)

  /** Select the member of Guider with the given tag, if any. */
  def fromTag(s: String): Option[Guider] =
    all.find(_.tag === s)

  /** Select the member of Guider with the given tag, throwing if absent. */
  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def unsafeFromTag(s: String): Guider =
    fromTag(s).getOrElse(throw new NoSuchElementException(s))

  /** @group Typeclass Instances */
  implicit val GuiderEnumerated: Enumerated[Guider] =
    new Enumerated[Guider] {
      def all = Guider.all
      def tag(a: Guider) = a.tag
    }

}