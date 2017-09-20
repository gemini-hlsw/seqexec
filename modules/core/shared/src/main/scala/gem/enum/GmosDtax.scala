// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package enum

import cats.syntax.eq._
import cats.instances.string._
import gem.util.Enumerated

/**
 * Enumerated type for GMOS detector translation X offset.
 * @group Enumerations (Generated)
 */
sealed abstract class GmosDtax(
  val tag: String,
  val shortName: String,
  val longName: String,
  val dtax: Int
) extends Product with Serializable

object GmosDtax {

  /** @group Constructors */ case object MinusSix extends GmosDtax("MinusSix", "-6", "-6", -6)
  /** @group Constructors */ case object MinusFive extends GmosDtax("MinusFive", "-5", "-5", -5)
  /** @group Constructors */ case object MinusFour extends GmosDtax("MinusFour", "-4", "-4", -4)
  /** @group Constructors */ case object MinusThree extends GmosDtax("MinusThree", "-3", "-3", -3)
  /** @group Constructors */ case object MinusTwo extends GmosDtax("MinusTwo", "-2", "-2", -2)
  /** @group Constructors */ case object MinusOne extends GmosDtax("MinusOne", "-1", "-1", -1)
  /** @group Constructors */ case object Zero extends GmosDtax("Zero", "0", "0", 0)
  /** @group Constructors */ case object One extends GmosDtax("One", "1", "1", 1)
  /** @group Constructors */ case object Two extends GmosDtax("Two", "2", "2", 2)
  /** @group Constructors */ case object Three extends GmosDtax("Three", "3", "3", 3)
  /** @group Constructors */ case object Four extends GmosDtax("Four", "4", "4", 4)
  /** @group Constructors */ case object Five extends GmosDtax("Five", "5", "5", 5)
  /** @group Constructors */ case object Six extends GmosDtax("Six", "6", "6", 6)

  /** All members of GmosDtax, in canonical order. */
  val all: List[GmosDtax] =
    List(MinusSix, MinusFive, MinusFour, MinusThree, MinusTwo, MinusOne, Zero, One, Two, Three, Four, Five, Six)

  /** Select the member of GmosDtax with the given tag, if any. */
  def fromTag(s: String): Option[GmosDtax] =
    all.find(_.tag === s)

  /** Select the member of GmosDtax with the given tag, throwing if absent. */
  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def unsafeFromTag(s: String): GmosDtax =
    fromTag(s).getOrElse(throw new NoSuchElementException(s))

  /** @group Typeclass Instances */
  implicit val GmosDtaxEnumerated: Enumerated[GmosDtax] =
    new Enumerated[GmosDtax] {
      def all = GmosDtax.all
      def tag(a: GmosDtax) = a.tag
    }

}