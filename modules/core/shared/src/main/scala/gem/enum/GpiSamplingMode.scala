// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package enum

import cats.instances.string._
import cats.syntax.eq._
import gem.util.Enumerated

/**
 * Enumerated type for GPI Sampling Mode.
 * @group Enumerations (Generated)
 */
sealed abstract class GpiSamplingMode(
  val tag: String,
  val shortName: String,
  val longName: String,
  val obsolete: Boolean
) extends Product with Serializable

object GpiSamplingMode {

  /** @group Constructors */ case object FAST extends GpiSamplingMode("FAST", "Fast", "Fast", false)
  /** @group Constructors */ case object SINGLE_CDS extends GpiSamplingMode("SINGLE_CDS", "Single CDS", "Single CDS", false)
  /** @group Constructors */ case object MULTIPLE_CDS extends GpiSamplingMode("MULTIPLE_CDS", "Multiple CDS", "Multiple CDS", false)
  /** @group Constructors */ case object UTR extends GpiSamplingMode("UTR", "UTR", "UTR", false)

  /** All members of GpiSamplingMode, in canonical order. */
  val all: List[GpiSamplingMode] =
    List(FAST, SINGLE_CDS, MULTIPLE_CDS, UTR)

  /** Select the member of GpiSamplingMode with the given tag, if any. */
  def fromTag(s: String): Option[GpiSamplingMode] =
    all.find(_.tag === s)

  /** Select the member of GpiSamplingMode with the given tag, throwing if absent. */
  def unsafeFromTag(s: String): GpiSamplingMode =
    fromTag(s).getOrElse(throw new NoSuchElementException(s"GpiSamplingMode: Invalid tag: '$s'"))

  /** @group Typeclass Instances */
  implicit val GpiSamplingModeEnumerated: Enumerated[GpiSamplingMode] =
    new Enumerated[GpiSamplingMode] {
      def all = GpiSamplingMode.all
      def tag(a: GpiSamplingMode) = a.tag
      override def unsafeFromTag(s: String): GpiSamplingMode =
        GpiSamplingMode.unsafeFromTag(s)
    }

}