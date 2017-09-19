// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package enum

import cats.syntax.eq._
import cats.instances.string._
import gem.util.Enumerated

/**
 * Enumerated type for user roles with respect to a given program.
 * @group Enumerations (Generated)
 */
sealed abstract class ProgramRole(
  val tag: String,
  val shortName: String,
  val longName: String
) extends Product with Serializable

object ProgramRole {

  /** @group Constructors */ case object PI extends ProgramRole("PI", "PI", "Principal Investigator")
  /** @group Constructors */ case object GEM extends ProgramRole("GEM", "GEM", "Gemini Contact")
  /** @group Constructors */ case object NGO extends ProgramRole("NGO", "NGO", "NGO Contact")

  /** All members of ProgramRole, in canonical order. */
  val all: List[ProgramRole] =
    List(PI, GEM, NGO)

  /** Select the member of ProgramRole with the given tag, if any. */
  def fromTag(s: String): Option[ProgramRole] =
    all.find(_.tag === s)

  /** Select the member of ProgramRole with the given tag, throwing if absent. */
  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def unsafeFromTag(s: String): ProgramRole =
    fromTag(s).getOrElse(throw new NoSuchElementException(s))

  /** @group Typeclass Instances */
  implicit val ProgramRoleEnumerated: Enumerated[ProgramRole] =
    new Enumerated[ProgramRole] {
      def all = ProgramRole.all
      def tag(a: ProgramRole) = a.tag
    }

}