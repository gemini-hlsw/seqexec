// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package parser

import cats.implicits._
import atto._, Atto._

/** Parser for [[gem.Semester]]. */
trait SemesterParsers {
  import TimeParsers._
  import EnumParsers._

  /** Parser for a full-year `Semester` like `2015A`. */
  val semester: Parser[Semester] =
    (year4, half).mapN(Semester.apply) named "semester"

}
object SemesterParsers extends SemesterParsers
