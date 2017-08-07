// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package enum

import scalaz.syntax.equal._
import scalaz.std.string._

/**
 * Enumerated type for Flamingos2 dispersers.
 * @group Enumerations (Generated)
 */
sealed abstract class F2Disperser(
  val tag: String,
  val shortName: String,
  val longName: String,
  val wavelength: Option[Double]
)

object F2Disperser {

  /** @group Constructors */ case object R1200JH extends F2Disperser("R1200JH", "R1200JH", "R=1200 (J + H) grism", Some(1.39))
  /** @group Constructors */ case object R1200HK extends F2Disperser("R1200HK", "R1200HK", "R=1200 (H + K) grism", Some(1.871))
  /** @group Constructors */ case object R3000 extends F2Disperser("R3000", "R3000", "R=3000 (J or H or K) grism", Some(1.65))
  /** @group Constructors */ case object NoDisperser extends F2Disperser("NoDisperser", "None", "None", None)

  /** All members of F2Disperser, in canonical order. */
  val all: List[F2Disperser] =
    List(R1200JH, R1200HK, R3000, NoDisperser)

  /** Select the member of F2Disperser with the given tag, if any. */
  def fromTag(s: String): Option[F2Disperser] =
    all.find(_.tag === s)

  /** Select the member of F2Disperser with the given tag, throwing if absent. */
  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def unsafeFromTag(s: String): F2Disperser =
    fromTag(s).getOrElse(throw new NoSuchElementException(s))

  /** @group Typeclass Instances */
  implicit val F2DisperserEnumerated: Enumerated[F2Disperser] =
    new Enumerated[F2Disperser] {
      def all = F2Disperser.all
      def tag(a: F2Disperser) = a.tag
    }

}