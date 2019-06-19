// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package enum

import cats.instances.string._
import cats.syntax.eq._
import gem.util.Enumerated

/**
 * Enumerated type for GPI Pupil Camera.
 * @group Enumerations (Generated)
 */
sealed abstract class GpiPupilCamera(
  val tag: String,
  val shortName: String,
  val longName: String,
  val value: Boolean
) extends Product with Serializable

object GpiPupilCamera {

  /** @group Constructors */ case object In extends GpiPupilCamera("In", "In", "In", true)
  /** @group Constructors */ case object Out extends GpiPupilCamera("Out", "Out", "Out", false)

  /** All members of GpiPupilCamera, in canonical order. */
  val all: List[GpiPupilCamera] =
    List(In, Out)

  /** Select the member of GpiPupilCamera with the given tag, if any. */
  def fromTag(s: String): Option[GpiPupilCamera] =
    all.find(_.tag === s)

  /** Select the member of GpiPupilCamera with the given tag, throwing if absent. */
  def unsafeFromTag(s: String): GpiPupilCamera =
    fromTag(s).getOrElse(throw new NoSuchElementException(s"GpiPupilCamera: Invalid tag: '$s'"))

  /** @group Typeclass Instances */
  implicit val GpiPupilCameraEnumerated: Enumerated[GpiPupilCamera] =
    new Enumerated[GpiPupilCamera] {
      def all = GpiPupilCamera.all
      def tag(a: GpiPupilCamera) = a.tag
      override def unsafeFromTag(s: String): GpiPupilCamera =
        GpiPupilCamera.unsafeFromTag(s)
    }

}