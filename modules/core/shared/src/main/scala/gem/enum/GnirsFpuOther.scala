// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package enum

import cats.instances.string._
import cats.syntax.eq._
import gem.util.Enumerated

/**
 * Enumerated type for GNRIS FPU Other.
 * @group Enumerations (Generated)
 */
sealed abstract class GnirsFpuOther(
  val tag: String,
  val shortName: String,
  val longName: String,
  val obsolete: Boolean
) extends Product with Serializable {
  type Self = this.type
}

object GnirsFpuOther {

  type Aux[A] = GnirsFpuOther { type Self = A }

  /** @group Constructors */ case object Ifu extends GnirsFpuOther("Ifu", "IFU", "Integral Field Unit", true)
  /** @group Constructors */ case object Acquisition extends GnirsFpuOther("Acquisition", "Acquisition", "Acquisition", false)
  /** @group Constructors */ case object PupilViewer extends GnirsFpuOther("PupilViewer", "Pupil", "Pupil viewer", false)
  /** @group Constructors */ case object Pinhole1 extends GnirsFpuOther("Pinhole1", "Small pin", "pinhole 0.1", false)
  /** @group Constructors */ case object Pinhole2 extends GnirsFpuOther("Pinhole2", "Large pin", "pinhole 0.3", false)

  /** All members of GnirsFpuOther, in canonical order. */
  val all: List[GnirsFpuOther] =
    List(Ifu, Acquisition, PupilViewer, Pinhole1, Pinhole2)

  /** Select the member of GnirsFpuOther with the given tag, if any. */
  def fromTag(s: String): Option[GnirsFpuOther] =
    all.find(_.tag === s)

  /** Select the member of GnirsFpuOther with the given tag, throwing if absent. */
  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def unsafeFromTag(s: String): GnirsFpuOther =
    fromTag(s).getOrElse(throw new NoSuchElementException(s))

  /** @group Typeclass Instances */
  implicit val GnirsFpuOtherEnumerated: Enumerated[GnirsFpuOther] =
    new Enumerated[GnirsFpuOther] {
      def all = GnirsFpuOther.all
      def tag(a: GnirsFpuOther) = a.tag
    }

}