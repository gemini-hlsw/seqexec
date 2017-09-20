// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package enum

import cats.syntax.eq._
import cats.instances.string._
import gem.util.Enumerated

/**
 * Enumerated type for calibration unit continuum lamps.
 * @group Enumerations (Generated)
 */
sealed abstract class GcalContinuum(
  val tag: String,
  val shortName: String,
  val longName: String,
  val obsolete: Boolean
) extends Product with Serializable

object GcalContinuum {

  /** @group Constructors */ case object IrGreyBodyLow extends GcalContinuum("IrGreyBodyLow", "IR grey body - low", "IR grey body - low", false)
  /** @group Constructors */ case object IrGreyBodyHigh extends GcalContinuum("IrGreyBodyHigh", "IR grey body - high", "IR grey body - high", false)
  /** @group Constructors */ case object QuartzHalogen extends GcalContinuum("QuartzHalogen", "Quartz Halogen", "Quartz Halogen", false)

  /** All members of GcalContinuum, in canonical order. */
  val all: List[GcalContinuum] =
    List(IrGreyBodyLow, IrGreyBodyHigh, QuartzHalogen)

  /** Select the member of GcalContinuum with the given tag, if any. */
  def fromTag(s: String): Option[GcalContinuum] =
    all.find(_.tag === s)

  /** Select the member of GcalContinuum with the given tag, throwing if absent. */
  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def unsafeFromTag(s: String): GcalContinuum =
    fromTag(s).getOrElse(throw new NoSuchElementException(s))

  /** @group Typeclass Instances */
  implicit val GcalContinuumEnumerated: Enumerated[GcalContinuum] =
    new Enumerated[GcalContinuum] {
      def all = GcalContinuum.all
      def tag(a: GcalContinuum) = a.tag
    }

}