package gem
package dao

import edu.gemini.spModel.core._

import doobie.imports._

import scalaz._, Scalaz._

object SemesterDao {

  implicit val SemesterHalfMeta: Meta[Semester.Half] =
    Meta[String].xmap(Semester.Half.valueOf, _.name)

  implicit val SemesterMeta: Meta[Semester] =
    Meta[String].xmap(Semester.parse, _.toString)

  def canonicalize(s: Semester): ConnectionIO[Semester] =
    Statements.canonicalize(s).run.as(s)

  object Statements {

    def canonicalize(s: Semester): Update0 =
      sql"""
        INSERT INTO semester (semester_id, year, half)
        VALUES (${s.toString}, ${s.getYear}, ${s.getHalf})
        ON CONFLICT DO NOTHING
      """.update

  }

}
