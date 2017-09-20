// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package enum

import cats.syntax.eq._
import cats.instances.string._
import gem.util.Enumerated

/**
 * Enumerated type for GMOS ROI (region of interest).
 * @group Enumerations (Generated)
 */
sealed abstract class GmosRoi(
  val tag: String,
  val shortName: String,
  val longName: String,
  val obsolete: Boolean
) extends Product with Serializable

object GmosRoi {

  /** @group Constructors */ case object FullFrame extends GmosRoi("FullFrame", "full", "Full Frame Readout", false)
  /** @group Constructors */ case object Ccd2 extends GmosRoi("Ccd2", "ccd2", "CCD 2", false)
  /** @group Constructors */ case object CentralSpectrum extends GmosRoi("CentralSpectrum", "cspec", "Central Spectrum", false)
  /** @group Constructors */ case object CentralStamp extends GmosRoi("CentralStamp", "stamp", "Central Stamp", false)
  /** @group Constructors */ case object TopSpectrum extends GmosRoi("TopSpectrum", "tspec", "Top Spectrum", true)
  /** @group Constructors */ case object BottomSpectrum extends GmosRoi("BottomSpectrum", "bspec", "Bottom Spectrum", true)
  /** @group Constructors */ case object Custom extends GmosRoi("Custom", "custom", "Custom ROI", false)

  /** All members of GmosRoi, in canonical order. */
  val all: List[GmosRoi] =
    List(FullFrame, Ccd2, CentralSpectrum, CentralStamp, TopSpectrum, BottomSpectrum, Custom)

  /** Select the member of GmosRoi with the given tag, if any. */
  def fromTag(s: String): Option[GmosRoi] =
    all.find(_.tag === s)

  /** Select the member of GmosRoi with the given tag, throwing if absent. */
  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def unsafeFromTag(s: String): GmosRoi =
    fromTag(s).getOrElse(throw new NoSuchElementException(s))

  /** @group Typeclass Instances */
  implicit val GmosRoiEnumerated: Enumerated[GmosRoi] =
    new Enumerated[GmosRoi] {
      def all = GmosRoi.all
      def tag(a: GmosRoi) = a.tag
    }

}