// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package giapi.enums

import cats.syntax.eq._
import lucuma.core.util.Enumerated

/**
 * Enumerated type for giapi status types.
 * @group Enumerations (Generated)
 */
sealed abstract class GiapiType(
  val tag: String
) extends Product
    with Serializable

object GiapiType {

  /** @group Constructors */
  case object String extends GiapiType("String")

  /** @group Constructors */
  case object Int extends GiapiType("Int")

  /** @group Constructors */
  case object Float extends GiapiType("Float")

  /** @group Constructors */
  case object Double extends GiapiType("Double")

  /** All members of GiapiType, in canonical order. */
  val all: List[GiapiType] =
    List(String, Int, Float, Double)

  /** Select the member of GiapiType with the given tag, if any. */
  def fromTag(s: String): Option[GiapiType] =
    all.find(_.tag === s)

  /** Select the member of GiapiType with the given tag, throwing if absent. */
  def unsafeFromTag(s: String): GiapiType =
    fromTag(s).getOrElse(throw new NoSuchElementException(s"GiapiType: Invalid tag: '$s'"))

  /** @group Typeclass Instances */
  implicit val GiapiTypeEnumerated: Enumerated[GiapiType] =
    new Enumerated[GiapiType] {
      def all = GiapiType.all
      def tag(a: GiapiType) = a.tag
      override def unsafeFromTag(s: String): GiapiType =
        GiapiType.unsafeFromTag(s)
    }

}
