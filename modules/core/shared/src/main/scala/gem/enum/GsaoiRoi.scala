// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package enum

import cats.instances.string._
import cats.syntax.eq._
import gem.util.Enumerated

/**
 * Enumerated type for Gsaoi Region of Interest.
 * @group Enumerations (Generated)
 */
sealed abstract class GsaoiRoi(
  val tag: String,
  val shortName: String,
  val longName: String
) extends Product with Serializable

object GsaoiRoi {

  /** @group Constructors */ case object FullArray extends GsaoiRoi("FullArray", "Full Array", "Det2kx2k")
  /** @group Constructors */ case object Array64 extends GsaoiRoi("Array64", "Array 64", "Det2kx2k")
  /** @group Constructors */ case object Array128 extends GsaoiRoi("Array128", "Array 128", "Det2kx2k")
  /** @group Constructors */ case object Array256 extends GsaoiRoi("Array256", "Array 256", "Det2kx2k")
  /** @group Constructors */ case object Array512 extends GsaoiRoi("Array512", "Array 512", "Det2kx2k")
  /** @group Constructors */ case object Array1k extends GsaoiRoi("Array1k", "Array 1K", "Det2kx2k")
  /** @group Constructors */ case object Central64 extends GsaoiRoi("Central64", "Central 64", "Det2kx2k")
  /** @group Constructors */ case object Central128 extends GsaoiRoi("Central128", "Central 128", "Det2kx2k")
  /** @group Constructors */ case object Central256 extends GsaoiRoi("Central256", "Central 256", "Det2kx2k")
  /** @group Constructors */ case object Central512 extends GsaoiRoi("Central512", "Central 512", "Det2kx2k")
  /** @group Constructors */ case object Central1k extends GsaoiRoi("Central1k", "Central 1K", "Det2kx2k")
  /** @group Constructors */ case object Central2k extends GsaoiRoi("Central2k", "Central 2K", "Det2kx2k")

  /** All members of GsaoiRoi, in canonical order. */
  val all: List[GsaoiRoi] =
    List(FullArray, Array64, Array128, Array256, Array512, Array1k, Central64, Central128, Central256, Central512, Central1k, Central2k)

  /** Select the member of GsaoiRoi with the given tag, if any. */
  def fromTag(s: String): Option[GsaoiRoi] =
    all.find(_.tag === s)

  /** Select the member of GsaoiRoi with the given tag, throwing if absent. */
  def unsafeFromTag(s: String): GsaoiRoi =
    fromTag(s).getOrElse(throw new NoSuchElementException(s"GsaoiRoi: Invalid tag: '$s'"))

  /** @group Typeclass Instances */
  implicit val GsaoiRoiEnumerated: Enumerated[GsaoiRoi] =
    new Enumerated[GsaoiRoi] {
      def all = GsaoiRoi.all
      def tag(a: GsaoiRoi) = a.tag
      override def unsafeFromTag(s: String): GsaoiRoi =
        GsaoiRoi.unsafeFromTag(s)
    }

}