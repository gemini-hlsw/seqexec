// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package enum

import cats.syntax.eq._
import cats.instances.string._
import gem.util.Enumerated

/**
 * Enumerated type for program types (see [[gem.ProgramId ProgramId]]).
 * @group Enumerations (Generated)
 */
sealed abstract class ProgramType(
  val tag: String,
  val shortName: String,
  val longName: String,
  val obsolete: Boolean
) extends Product with Serializable

object ProgramType {

  /** @group Constructors */ case object CAL extends ProgramType("CAL", "CAL", "Calibration", false)
  /** @group Constructors */ case object C extends ProgramType("C", "C", "Classical", false)
  /** @group Constructors */ case object DS extends ProgramType("DS", "DS", "Demo Science", false)
  /** @group Constructors */ case object DD extends ProgramType("DD", "DD", "Director's Time", false)
  /** @group Constructors */ case object ENG extends ProgramType("ENG", "ENG", "Engineering", false)
  /** @group Constructors */ case object FT extends ProgramType("FT", "FT", "Fast Turnaround", false)
  /** @group Constructors */ case object LP extends ProgramType("LP", "LP", "Large Program", false)
  /** @group Constructors */ case object Q extends ProgramType("Q", "Q", "Queue", false)
  /** @group Constructors */ case object SV extends ProgramType("SV", "SV", "System Verification", false)

  /** All members of ProgramType, in canonical order. */
  val all: List[ProgramType] =
    List(CAL, C, DS, DD, ENG, FT, LP, Q, SV)

  /** Select the member of ProgramType with the given tag, if any. */
  def fromTag(s: String): Option[ProgramType] =
    all.find(_.tag === s)

  /** Select the member of ProgramType with the given tag, throwing if absent. */
  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def unsafeFromTag(s: String): ProgramType =
    fromTag(s).getOrElse(throw new NoSuchElementException(s))

  /** @group Typeclass Instances */
  implicit val ProgramTypeEnumerated: Enumerated[ProgramType] =
    new Enumerated[ProgramType] {
      def all = ProgramType.all
      def tag(a: ProgramType) = a.tag
    }

}