// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package enum

import cats.instances.string._
import cats.syntax.eq._
import gem.util.Enumerated

/**
 * Enumerated type for GPI Apodizer.
 * @group Enumerations (Generated)
 */
sealed abstract class GpiApodizer(
  val tag: String,
  val shortName: String,
  val longName: String,
  val obsolete: Boolean
) extends Product with Serializable

object GpiApodizer {

  /** @group Constructors */ case object CLEAR extends GpiApodizer("CLEAR", "Clear", "Clear", false)
  /** @group Constructors */ case object CLEARGP extends GpiApodizer("CLEARGP", "CLEAR GP", "CLEAR GP", false)
  /** @group Constructors */ case object APOD_Y extends GpiApodizer("APOD_Y", "APOD_Y_56", "APOD_Y_56", false)
  /** @group Constructors */ case object APOD_J extends GpiApodizer("APOD_J", "APOD_J_56", "APOD_J_56", false)
  /** @group Constructors */ case object APOD_H extends GpiApodizer("APOD_H", "APOD_H_56", "APOD_H_56", false)
  /** @group Constructors */ case object APOD_K1 extends GpiApodizer("APOD_K1", "APOD_K1_56", "APOD_K1_56", false)
  /** @group Constructors */ case object APOD_K2 extends GpiApodizer("APOD_K2", "APOD_K2_56", "APOD_K2_56", false)
  /** @group Constructors */ case object NRM extends GpiApodizer("NRM", "NRM", "NRM", false)
  /** @group Constructors */ case object APOD_HL extends GpiApodizer("APOD_HL", "APOD_HL", "APOD_HL", false)
  /** @group Constructors */ case object APOD_STAR extends GpiApodizer("APOD_STAR", "APOD_star", "APOD_star", true)
  /** @group Constructors */ case object ND3 extends GpiApodizer("ND3", "ND3", "ND3", false)

  /** All members of GpiApodizer, in canonical order. */
  val all: List[GpiApodizer] =
    List(CLEAR, CLEARGP, APOD_Y, APOD_J, APOD_H, APOD_K1, APOD_K2, NRM, APOD_HL, APOD_STAR, ND3)

  /** Select the member of GpiApodizer with the given tag, if any. */
  def fromTag(s: String): Option[GpiApodizer] =
    all.find(_.tag === s)

  /** Select the member of GpiApodizer with the given tag, throwing if absent. */
  def unsafeFromTag(s: String): GpiApodizer =
    fromTag(s).getOrElse(throw new NoSuchElementException(s"GpiApodizer: Invalid tag: '$s'"))

  /** @group Typeclass Instances */
  implicit val GpiApodizerEnumerated: Enumerated[GpiApodizer] =
    new Enumerated[GpiApodizer] {
      def all = GpiApodizer.all
      def tag(a: GpiApodizer) = a.tag
      override def unsafeFromTag(s: String): GpiApodizer =
        GpiApodizer.unsafeFromTag(s)
    }

}