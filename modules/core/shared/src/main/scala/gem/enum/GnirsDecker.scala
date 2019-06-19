// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package enum

import cats.instances.string._
import cats.syntax.eq._
import gem.util.Enumerated

/**
 * Enumerated type for GNRIS Decker.
 * @group Enumerations (Generated)
 */
sealed abstract class GnirsDecker(
  val tag: String,
  val shortName: String,
  val longName: String,
  val obsolete: Boolean
) extends Product with Serializable

object GnirsDecker {

  /** @group Constructors */ case object Acquisition extends GnirsDecker("Acquisition", "Acquisition", "Acquisition", false)
  /** @group Constructors */ case object PupilViewer extends GnirsDecker("PupilViewer", "Pupil", "Pupil viewer", false)
  /** @group Constructors */ case object ShortCamCrossDispersed extends GnirsDecker("ShortCamCrossDispersed", "Short camera XD", "Short camera cross dispersed", false)
  /** @group Constructors */ case object Ifu extends GnirsDecker("Ifu", "IFU", "Integral field unit", true)
  /** @group Constructors */ case object LongCamLongSlit extends GnirsDecker("LongCamLongSlit", "Long camera slit", "Long camera long slit", false)
  /** @group Constructors */ case object Wollaston extends GnirsDecker("Wollaston", "Wollaston", "Wollaston", true)
  /** @group Constructors */ case object ShortCamLongSlit extends GnirsDecker("ShortCamLongSlit", "Short camera slit", "Short camera long slit", false)
  /** @group Constructors */ case object LongCamCrossDispersed extends GnirsDecker("LongCamCrossDispersed", "Long camera XD", "Long camera cross dispersed", false)

  /** All members of GnirsDecker, in canonical order. */
  val all: List[GnirsDecker] =
    List(Acquisition, PupilViewer, ShortCamCrossDispersed, Ifu, LongCamLongSlit, Wollaston, ShortCamLongSlit, LongCamCrossDispersed)

  /** Select the member of GnirsDecker with the given tag, if any. */
  def fromTag(s: String): Option[GnirsDecker] =
    all.find(_.tag === s)

  /** Select the member of GnirsDecker with the given tag, throwing if absent. */
  def unsafeFromTag(s: String): GnirsDecker =
    fromTag(s).getOrElse(throw new NoSuchElementException(s"GnirsDecker: Invalid tag: '$s'"))

  /** @group Typeclass Instances */
  implicit val GnirsDeckerEnumerated: Enumerated[GnirsDecker] =
    new Enumerated[GnirsDecker] {
      def all = GnirsDecker.all
      def tag(a: GnirsDecker) = a.tag
      override def unsafeFromTag(s: String): GnirsDecker =
        GnirsDecker.unsafeFromTag(s)
    }

}