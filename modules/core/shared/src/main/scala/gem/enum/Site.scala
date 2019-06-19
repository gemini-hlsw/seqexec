// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package enum

import cats.instances.string._
import cats.syntax.eq._
import gem.util.Enumerated
import gsp.math.Angle
import java.time.ZoneId

/**
 * Enumerated type for Gemini observing sites.
 * @group Enumerations (Generated)
 */
sealed abstract class Site(
  val tag: String,
  val shortName: String,
  val longName: String,
  val mountain: String,
  val latitude: Angle,
  val longitude: Angle,
  val altitude: Int,
  val timezone: ZoneId
) extends Product with Serializable

object Site {

  /** @group Constructors */ case object GN extends Site("GN", "GN", "Gemini North", "Mauna Kea", Angle.fromDoubleDegrees(19.8238068), Angle.fromDoubleDegrees(-155.4690550), 4213, ZoneId.of("Pacific/Honolulu"))
  /** @group Constructors */ case object GS extends Site("GS", "GS", "Gemini South", "Cerro Pachon", Angle.fromDoubleDegrees(-30.2407494), Angle.fromDoubleDegrees(-70.7366867), 2722, ZoneId.of("America/Santiago"))

  /** All members of Site, in canonical order. */
  val all: List[Site] =
    List(GN, GS)

  /** Select the member of Site with the given tag, if any. */
  def fromTag(s: String): Option[Site] =
    all.find(_.tag === s)

  /** Select the member of Site with the given tag, throwing if absent. */
  def unsafeFromTag(s: String): Site =
    fromTag(s).getOrElse(throw new NoSuchElementException(s"Site: Invalid tag: '$s'"))

  /** @group Typeclass Instances */
  implicit val SiteEnumerated: Enumerated[Site] =
    new Enumerated[Site] {
      def all = Site.all
      def tag(a: Site) = a.tag
      override def unsafeFromTag(s: String): Site =
        Site.unsafeFromTag(s)
    }

}