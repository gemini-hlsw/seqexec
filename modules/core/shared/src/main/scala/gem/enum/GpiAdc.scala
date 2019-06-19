// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package enum

import cats.instances.string._
import cats.syntax.eq._
import gem.util.Enumerated

/**
 * Enumerated type for GPI ADC.
 * @group Enumerations (Generated)
 */
sealed abstract class GpiAdc(
  val tag: String,
  val shortName: String,
  val longName: String,
  val value: Boolean
) extends Product with Serializable

object GpiAdc {

  /** @group Constructors */ case object In extends GpiAdc("In", "In", "In", true)
  /** @group Constructors */ case object Out extends GpiAdc("Out", "Out", "Out", false)

  /** All members of GpiAdc, in canonical order. */
  val all: List[GpiAdc] =
    List(In, Out)

  /** Select the member of GpiAdc with the given tag, if any. */
  def fromTag(s: String): Option[GpiAdc] =
    all.find(_.tag === s)

  /** Select the member of GpiAdc with the given tag, throwing if absent. */
  def unsafeFromTag(s: String): GpiAdc =
    fromTag(s).getOrElse(throw new NoSuchElementException(s"GpiAdc: Invalid tag: '$s'"))

  /** @group Typeclass Instances */
  implicit val GpiAdcEnumerated: Enumerated[GpiAdc] =
    new Enumerated[GpiAdc] {
      def all = GpiAdc.all
      def tag(a: GpiAdc) = a.tag
      override def unsafeFromTag(s: String): GpiAdc =
        GpiAdc.unsafeFromTag(s)
    }

}