// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package enum

import scalaz.syntax.equal._
import scalaz.std.string._

/** The subset of ProgramType values usable for daily science programs. */
sealed abstract class DailyProgramType(
  val toProgramType: ProgramType
) {
  val tag:       String  = toProgramType.tag
  val shortName: String  = toProgramType.shortName
  val longName:  String  = toProgramType.longName
  val obsolete:  Boolean = toProgramType.obsolete
}

object DailyProgramType {

  case object CAL extends DailyProgramType(ProgramType.CAL)
  case object ENG extends DailyProgramType(ProgramType.ENG)

  val all: List[DailyProgramType] =
    List(CAL, ENG)

  def fromTag(s: String): Option[DailyProgramType] =
    all.find(_.tag === s)

  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def unsafeFromTag(s: String): DailyProgramType =
    fromTag(s).getOrElse(throw new NoSuchElementException(s))

  implicit val ProgramTypeEnumerated: Enumerated[DailyProgramType] =
    new Enumerated[DailyProgramType] {
      def all = DailyProgramType.all
      def tag(a: DailyProgramType) = a.tag
    }

}
