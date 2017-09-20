// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package enum

import cats.syntax.eq._
import cats.instances.string._
import gem.util.Enumerated

/**
 * Enumerated type for GMOS North dispersers.
 * @group Enumerations (Generated)
 */
sealed abstract class GmosNorthDisperser(
  val tag: String,
  val shortName: String,
  val longName: String,
  val rulingDensity: Int,
  val obsolete: Boolean
) extends Product with Serializable

object GmosNorthDisperser {

  /** @group Constructors */ case object B1200_G5301 extends GmosNorthDisperser("B1200_G5301", "B1200", "B1200_G5301", 1200, false)
  /** @group Constructors */ case object R831_G5302 extends GmosNorthDisperser("R831_G5302", "R831", "R831_G5302", 831, false)
  /** @group Constructors */ case object B600_G5303 extends GmosNorthDisperser("B600_G5303", "B600", "B600_G5303", 600, true)
  /** @group Constructors */ case object B600_G5307 extends GmosNorthDisperser("B600_G5307", "B600", "B600_G5307", 600, false)
  /** @group Constructors */ case object R600_G5304 extends GmosNorthDisperser("R600_G5304", "R600", "R600_G5304", 600, false)
  /** @group Constructors */ case object R400_G5305 extends GmosNorthDisperser("R400_G5305", "R400", "R400_G5305", 400, false)
  /** @group Constructors */ case object R150_G5306 extends GmosNorthDisperser("R150_G5306", "R150", "R150_G5306", 150, false)
  /** @group Constructors */ case object R150_G5308 extends GmosNorthDisperser("R150_G5308", "R150", "R150_G5308", 150, false)

  /** All members of GmosNorthDisperser, in canonical order. */
  val all: List[GmosNorthDisperser] =
    List(B1200_G5301, R831_G5302, B600_G5303, B600_G5307, R600_G5304, R400_G5305, R150_G5306, R150_G5308)

  /** Select the member of GmosNorthDisperser with the given tag, if any. */
  def fromTag(s: String): Option[GmosNorthDisperser] =
    all.find(_.tag === s)

  /** Select the member of GmosNorthDisperser with the given tag, throwing if absent. */
  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def unsafeFromTag(s: String): GmosNorthDisperser =
    fromTag(s).getOrElse(throw new NoSuchElementException(s))

  /** @group Typeclass Instances */
  implicit val GmosNorthDisperserEnumerated: Enumerated[GmosNorthDisperser] =
    new Enumerated[GmosNorthDisperser] {
      def all = GmosNorthDisperser.all
      def tag(a: GmosNorthDisperser) = a.tag
    }

}