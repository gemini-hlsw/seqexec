// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package enum

import cats.syntax.eq._
import cats.instances.string._
import gem.util.Enumerated

/**
 * Enumerated type for calibration unit diffusers.
 * @group Enumerations (Generated)
 */
sealed abstract class GcalDiffuser(
  val tag: String,
  val shortName: String,
  val longName: String,
  val obsolete: Boolean
) extends Product with Serializable

object GcalDiffuser {

  /** @group Constructors */ case object Ir extends GcalDiffuser("Ir", "IR", "IR", false)
  /** @group Constructors */ case object Visible extends GcalDiffuser("Visible", "Visible", "Visible", false)

  /** All members of GcalDiffuser, in canonical order. */
  val all: List[GcalDiffuser] =
    List(Ir, Visible)

  /** Select the member of GcalDiffuser with the given tag, if any. */
  def fromTag(s: String): Option[GcalDiffuser] =
    all.find(_.tag === s)

  /** Select the member of GcalDiffuser with the given tag, throwing if absent. */
  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def unsafeFromTag(s: String): GcalDiffuser =
    fromTag(s).getOrElse(throw new NoSuchElementException(s))

  /** @group Typeclass Instances */
  implicit val GcalDiffuserEnumerated: Enumerated[GcalDiffuser] =
    new Enumerated[GcalDiffuser] {
      def all = GcalDiffuser.all
      def tag(a: GcalDiffuser) = a.tag
    }

}