// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package enum

import cats.instances.string._
import cats.syntax.eq._
import gem.util.Enumerated

/**
 * Enumerated type for GPI Lyot.
 * @group Enumerations (Generated)
 */
sealed abstract class GpiLyot(
  val tag: String,
  val shortName: String,
  val longName: String,
  val obsolete: Boolean
) extends Product with Serializable

object GpiLyot {

  /** @group Constructors */ case object BLANK extends GpiLyot("BLANK", "Blank", "Blank", false)
  /** @group Constructors */ case object LYOT_080m12_03 extends GpiLyot("LYOT_080m12_03", "080m12_03", "080m12_03", false)
  /** @group Constructors */ case object LYOT_080m12_04 extends GpiLyot("LYOT_080m12_04", "080m12_04", "080m12_04", false)
  /** @group Constructors */ case object LYOT_080_04 extends GpiLyot("LYOT_080_04", "080_04", "080_04", false)
  /** @group Constructors */ case object LYOT_080m12_06 extends GpiLyot("LYOT_080m12_06", "080m12_06", "080m12_06", false)
  /** @group Constructors */ case object LYOT_080m12_04_c extends GpiLyot("LYOT_080m12_04_c", "080m12_04_c", "080m12_04_c", false)
  /** @group Constructors */ case object LYOT_080m12_06_03 extends GpiLyot("LYOT_080m12_06_03", "080m12_06_03", "080m12_06_03", false)
  /** @group Constructors */ case object LYOT_080m12_07 extends GpiLyot("LYOT_080m12_07", "080m12_07", "080m12_07", false)
  /** @group Constructors */ case object LYOT_080m12_10 extends GpiLyot("LYOT_080m12_10", "080m12_10", "080m12_10", false)
  /** @group Constructors */ case object OPEN extends GpiLyot("OPEN", "Open", "Open", false)

  /** All members of GpiLyot, in canonical order. */
  val all: List[GpiLyot] =
    List(BLANK, LYOT_080m12_03, LYOT_080m12_04, LYOT_080_04, LYOT_080m12_06, LYOT_080m12_04_c, LYOT_080m12_06_03, LYOT_080m12_07, LYOT_080m12_10, OPEN)

  /** Select the member of GpiLyot with the given tag, if any. */
  def fromTag(s: String): Option[GpiLyot] =
    all.find(_.tag === s)

  /** Select the member of GpiLyot with the given tag, throwing if absent. */
  def unsafeFromTag(s: String): GpiLyot =
    fromTag(s).getOrElse(throw new NoSuchElementException(s"GpiLyot: Invalid tag: '$s'"))

  /** @group Typeclass Instances */
  implicit val GpiLyotEnumerated: Enumerated[GpiLyot] =
    new Enumerated[GpiLyot] {
      def all = GpiLyot.all
      def tag(a: GpiLyot) = a.tag
      override def unsafeFromTag(s: String): GpiLyot =
        GpiLyot.unsafeFromTag(s)
    }

}