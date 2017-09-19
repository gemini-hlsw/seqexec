// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package enum

import cats.syntax.eq._
import cats.instances.string._
import gem.util.Enumerated

/**
 * Enumerated type for GMOS South dispersers.
 * @group Enumerations (Generated)
 */
sealed abstract class GmosSouthDisperser(
  val tag: String,
  val shortName: String,
  val longName: String,
  val rulingDensity: Int,
  val obsolete: Boolean
) extends Product with Serializable

object GmosSouthDisperser {

  /** @group Constructors */ case object B1200_G5321 extends GmosSouthDisperser("B1200_G5321", "B1200", "B1200_G5321", 1200, false)
  /** @group Constructors */ case object R831_G5322 extends GmosSouthDisperser("R831_G5322", "R831", "R831_G5322", 831, false)
  /** @group Constructors */ case object B600_G5323 extends GmosSouthDisperser("B600_G5323", "B600", "B600_G5323", 600, false)
  /** @group Constructors */ case object R600_G5324 extends GmosSouthDisperser("R600_G5324", "R600", "R600_G5324", 600, false)
  /** @group Constructors */ case object R400_G5325 extends GmosSouthDisperser("R400_G5325", "R400", "R400_G5325", 400, false)
  /** @group Constructors */ case object R150_G5326 extends GmosSouthDisperser("R150_G5326", "R150", "R150_G5326", 150, false)

  /** All members of GmosSouthDisperser, in canonical order. */
  val all: List[GmosSouthDisperser] =
    List(B1200_G5321, R831_G5322, B600_G5323, R600_G5324, R400_G5325, R150_G5326)

  /** Select the member of GmosSouthDisperser with the given tag, if any. */
  def fromTag(s: String): Option[GmosSouthDisperser] =
    all.find(_.tag === s)

  /** Select the member of GmosSouthDisperser with the given tag, throwing if absent. */
  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def unsafeFromTag(s: String): GmosSouthDisperser =
    fromTag(s).getOrElse(throw new NoSuchElementException(s))

  /** @group Typeclass Instances */
  implicit val GmosSouthDisperserEnumerated: Enumerated[GmosSouthDisperser] =
    new Enumerated[GmosSouthDisperser] {
      def all = GmosSouthDisperser.all
      def tag(a: GmosSouthDisperser) = a.tag
    }

}