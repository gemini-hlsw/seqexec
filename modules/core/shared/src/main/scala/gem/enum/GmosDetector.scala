// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package enum

import cats.syntax.eq._
import cats.instances.string._
import gem.util.Enumerated

/**
 * Enumerated type for GMOS detector.
 * @group Enumerations (Generated)
 */
sealed abstract class GmosDetector(
  val tag: String,
  val shortName: String,
  val longName: String,
  val northPixelSize: gem.math.Angle,
  val southPixelSize: gem.math.Angle,
  val shuffleOffset: Int,
  val xSize: Int,
  val ySize: Int,
  val maxRois: Int
) extends Product with Serializable

object GmosDetector {

  /** @group Constructors */ case object E2V extends GmosDetector("E2V", "E2V", "E2V", gem.math.Angle.fromDoubleArcseconds(0.0727), gem.math.Angle.fromDoubleArcseconds(0.073), 1536, 6144, 4608, 4)
  /** @group Constructors */ case object HAMAMATSU extends GmosDetector("HAMAMATSU", "Hamamatsu", "Hamamatsu", gem.math.Angle.fromDoubleArcseconds(0.0809), gem.math.Angle.fromDoubleArcseconds(0.0809), 1392, 6144, 4224, 5)

  /** All members of GmosDetector, in canonical order. */
  val all: List[GmosDetector] =
    List(E2V, HAMAMATSU)

  /** Select the member of GmosDetector with the given tag, if any. */
  def fromTag(s: String): Option[GmosDetector] =
    all.find(_.tag === s)

  /** Select the member of GmosDetector with the given tag, throwing if absent. */
  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def unsafeFromTag(s: String): GmosDetector =
    fromTag(s).getOrElse(throw new NoSuchElementException(s))

  /** @group Typeclass Instances */
  implicit val GmosDetectorEnumerated: Enumerated[GmosDetector] =
    new Enumerated[GmosDetector] {
      def all = GmosDetector.all
      def tag(a: GmosDetector) = a.tag
    }

}