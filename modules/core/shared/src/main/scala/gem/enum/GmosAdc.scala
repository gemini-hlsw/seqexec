// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package enum

import cats.syntax.eq._
import cats.instances.string._
import gem.util.Enumerated

/**
 * Enumerated type for GMOS ADC.
 * @group Enumerations (Generated)
 */
sealed abstract class GmosAdc(
  val tag: String,
  val shortName: String,
  val longName: String
) extends Product with Serializable

object GmosAdc {

  /** @group Constructors */ case object BestStatic extends GmosAdc("BestStatic", "Best Static", "Best Static Correction")
  /** @group Constructors */ case object Follow extends GmosAdc("Follow", "Follow", "Follow During Exposure")

  /** All members of GmosAdc, in canonical order. */
  val all: List[GmosAdc] =
    List(BestStatic, Follow)

  /** Select the member of GmosAdc with the given tag, if any. */
  def fromTag(s: String): Option[GmosAdc] =
    all.find(_.tag === s)

  /** Select the member of GmosAdc with the given tag, throwing if absent. */
  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def unsafeFromTag(s: String): GmosAdc =
    fromTag(s).getOrElse(throw new NoSuchElementException(s))

  /** @group Typeclass Instances */
  implicit val GmosAdcEnumerated: Enumerated[GmosAdc] =
    new Enumerated[GmosAdc] {
      def all = GmosAdc.all
      def tag(a: GmosAdc) = a.tag
    }

}