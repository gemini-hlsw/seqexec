// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package enum

import scalaz.syntax.equal._
import scalaz.std.string._

/**
 * Enumerated type for GMOS built-in ROI (region of interest).
 * @group Enumerations (Generated)
 */
sealed abstract class GmosBuiltinRoi(
  val tag: String,
  val shortName: String,
  val longName: String,
  val xStart: Int,
  val yStart: Int,
  val xSize: Int,
  val ySize: Int,
  val obsolete: Boolean
)

object GmosBuiltinRoi {

  /** @group Constructors */ case object FullFrame extends GmosBuiltinRoi("FullFrame", "full", "Full Frame Readout", 1, 1, 6144, 4608, false)
  /** @group Constructors */ case object Ccd2 extends GmosBuiltinRoi("Ccd2", "ccd2", "CCD 2", 2049, 1, 2048, 4608, false)
  /** @group Constructors */ case object CentralSpectrum extends GmosBuiltinRoi("CentralSpectrum", "cspec", "Central Spectrum", 1, 1792, 6144, 1024, false)
  /** @group Constructors */ case object CentralStamp extends GmosBuiltinRoi("CentralStamp", "stamp", "Central Stamp", 2922, 2154, 300, 300, false)
  /** @group Constructors */ case object TopSpectrum extends GmosBuiltinRoi("TopSpectrum", "tspec", "Top Spectrum", 1, 3328, 6144, 1024, true)
  /** @group Constructors */ case object BottomSpectrum extends GmosBuiltinRoi("BottomSpectrum", "bspec", "Bottom Spectrum", 1, 256, 6144, 1024, true)

  /** All members of GmosBuiltinRoi, in canonical order. */
  val all: List[GmosBuiltinRoi] =
    List(FullFrame, Ccd2, CentralSpectrum, CentralStamp, TopSpectrum, BottomSpectrum)

  /** Select the member of GmosBuiltinRoi with the given tag, if any. */
  def fromTag(s: String): Option[GmosBuiltinRoi] =
    all.find(_.tag === s)

  /** Select the member of GmosBuiltinRoi with the given tag, throwing if absent. */
  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def unsafeFromTag(s: String): GmosBuiltinRoi =
    fromTag(s).getOrElse(throw new NoSuchElementException(s))

  /** @group Typeclass Instances */
  implicit val GmosBuiltinRoiEnumerated: Enumerated[GmosBuiltinRoi] =
    new Enumerated[GmosBuiltinRoi] {
      def all = GmosBuiltinRoi.all
      def tag(a: GmosBuiltinRoi) = a.tag
    }

}