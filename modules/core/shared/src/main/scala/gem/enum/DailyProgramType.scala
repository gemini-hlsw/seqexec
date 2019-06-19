// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package enum

import cats.syntax.eq._
import cats.instances.string._
import gem.util.Enumerated

/**
 * Enumerated type for the subset of [[ProgramType]] allowed for daily science programs.
 * @group Enumerations
 */
sealed abstract class DailyProgramType(
  val toProgramType: ProgramType
) {
  val tag:       String  = toProgramType.tag
  val shortName: String  = toProgramType.shortName
  val longName:  String  = toProgramType.longName
  val obsolete:  Boolean = toProgramType.obsolete
}

object DailyProgramType {

  /** @group Constructors */ case object CAL extends DailyProgramType(ProgramType.CAL)
  /** @group Constructors */ case object ENG extends DailyProgramType(ProgramType.ENG)

  val all: List[DailyProgramType] =
    List(CAL, ENG)

  def fromTag(s: String): Option[DailyProgramType] =
    all.find(_.tag === s)

  def unsafeFromTag(s: String): DailyProgramType =
    fromTag(s).getOrElse(throw new NoSuchElementException(s))

  /** @group Typeclass Instances */
  implicit val ProgramTypeEnumerated: Enumerated[DailyProgramType] =
    new Enumerated[DailyProgramType] {
      def all = DailyProgramType.all
      def tag(a: DailyProgramType) = a.tag
    }

}
