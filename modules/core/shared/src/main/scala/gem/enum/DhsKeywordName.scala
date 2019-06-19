// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package enum

import cats.instances.string._
import cats.syntax.eq._
import gem.util.Enumerated

/**
 * Enumerated type for DHS Keyword names.
 * @group Enumerations (Generated)
 */
sealed abstract class DhsKeywordName(
  val tag: String,
  val keyword: KeywordName,
  val name: String
) extends Product with Serializable

object DhsKeywordName {

  /** @group Constructors */ case object INSTRUMENT extends DhsKeywordName("INSTRUMENT", KeywordName.INSTRUMENT, "instrument")
  /** @group Constructors */ case object OBSID extends DhsKeywordName("OBSID", KeywordName.OBSID, "obsid")
  /** @group Constructors */ case object TELESCOP extends DhsKeywordName("TELESCOP", KeywordName.TELESCOP, "telescope")
  /** @group Constructors */ case object FRMSPCYCL extends DhsKeywordName("FRMSPCYCL", KeywordName.FRMSPCYCL, "FRMSPCYCL")
  /** @group Constructors */ case object HDRTIMING extends DhsKeywordName("HDRTIMING", KeywordName.HDRTIMING, "HDRTIMING")
  /** @group Constructors */ case object A_TDETABS extends DhsKeywordName("A_TDETABS", KeywordName.A_TDETABS, "A_TDETABS")

  /** All members of DhsKeywordName, in canonical order. */
  val all: List[DhsKeywordName] =
    List(INSTRUMENT, OBSID, TELESCOP, FRMSPCYCL, HDRTIMING, A_TDETABS)

  /** Select the member of DhsKeywordName with the given tag, if any. */
  def fromTag(s: String): Option[DhsKeywordName] =
    all.find(_.tag === s)

  /** Select the member of DhsKeywordName with the given tag, throwing if absent. */
  def unsafeFromTag(s: String): DhsKeywordName =
    fromTag(s).getOrElse(throw new NoSuchElementException(s"DhsKeywordName: Invalid tag: '$s'"))

  /** @group Typeclass Instances */
  implicit val DhsKeywordNameEnumerated: Enumerated[DhsKeywordName] =
    new Enumerated[DhsKeywordName] {
      def all = DhsKeywordName.all
      def tag(a: DhsKeywordName) = a.tag
      override def unsafeFromTag(s: String): DhsKeywordName =
        DhsKeywordName.unsafeFromTag(s)
    }

}