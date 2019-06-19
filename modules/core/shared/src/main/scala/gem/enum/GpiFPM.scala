// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package enum

import cats.instances.string._
import cats.syntax.eq._
import gem.util.Enumerated

/**
 * Enumerated type for GPI FPM.
 * @group Enumerations (Generated)
 */
sealed abstract class GpiFPM(
  val tag: String,
  val shortName: String,
  val longName: String,
  val obsolete: Boolean
) extends Product with Serializable

object GpiFPM {

  /** @group Constructors */ case object OPEN extends GpiFPM("OPEN", "Open", "Open", false)
  /** @group Constructors */ case object F50umPIN extends GpiFPM("F50umPIN", "50umPIN", "50umPIN", false)
  /** @group Constructors */ case object WITH_DOT extends GpiFPM("WITH_DOT", "WITH_DOT", "WITH_DOT", false)
  /** @group Constructors */ case object SCIENCE extends GpiFPM("SCIENCE", "SCIENCE", "SCIENCE", false)
  /** @group Constructors */ case object FPM_K1 extends GpiFPM("FPM_K1", "FPM_K1", "FPM_K1", false)
  /** @group Constructors */ case object FPM_H extends GpiFPM("FPM_H", "FPM_H", "FPM_H", false)
  /** @group Constructors */ case object FPM_J extends GpiFPM("FPM_J", "FPM_J", "FPM_J", false)
  /** @group Constructors */ case object FPM_Y extends GpiFPM("FPM_Y", "FPM_Y", "FPM_Y", false)

  /** All members of GpiFPM, in canonical order. */
  val all: List[GpiFPM] =
    List(OPEN, F50umPIN, WITH_DOT, SCIENCE, FPM_K1, FPM_H, FPM_J, FPM_Y)

  /** Select the member of GpiFPM with the given tag, if any. */
  def fromTag(s: String): Option[GpiFPM] =
    all.find(_.tag === s)

  /** Select the member of GpiFPM with the given tag, throwing if absent. */
  def unsafeFromTag(s: String): GpiFPM =
    fromTag(s).getOrElse(throw new NoSuchElementException(s"GpiFPM: Invalid tag: '$s'"))

  /** @group Typeclass Instances */
  implicit val GpiFPMEnumerated: Enumerated[GpiFPM] =
    new Enumerated[GpiFPM] {
      def all = GpiFPM.all
      def tag(a: GpiFPM) = a.tag
      override def unsafeFromTag(s: String): GpiFPM =
        GpiFPM.unsafeFromTag(s)
    }

}