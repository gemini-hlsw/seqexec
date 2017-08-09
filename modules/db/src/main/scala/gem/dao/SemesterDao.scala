// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package dao

import cats.syntax.functor._
import doobie.imports._

object SemesterDao {

  def canonicalize(s: Semester): ConnectionIO[Semester] =
    Statements.canonicalize(s).run.as(s)

  object Statements {

    def canonicalize(s: Semester): Update0 =
      sql"""
        INSERT INTO semester (semester_id, year, half)
        VALUES (${s.format}, ${s.year.getValue.toShort}, ${s.half}::half)
        ON CONFLICT DO NOTHING
      """.update

  }

}
