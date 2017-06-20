// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package dao

import gem.config.{DynamicConfig, StaticConfig}

import edu.gemini.spModel.core._
import edu.gemini.spModel.core.ProgramId._

import doobie.imports._

import scalaz._, Scalaz._

object ProgramDao {

  /** Insert a program, disregarding its observations, if any. */
  def insert(p: Program[_]): ConnectionIO[Int] =
    insertProgramIdSlice(p.id) *>
    Statements.insert(p).run

  private def insertProgramIdSlice(pid: ProgramId): ConnectionIO[Int] =
    pid match {
      case id @ Science  (_, _, _, _) => insertScienceProgramIdSlice(id)
      case id @ Daily (_, _, _, _, _) => insertDailyProgramIdSlice(id)
      case id @ Arbitrary(_, _, _, _) => insertArbitraryProgramIdSlice(id)
    }

  private def insertScienceProgramIdSlice(pid: ProgramId.Science): ConnectionIO[Int] =
    SemesterDao.canonicalize(pid.semesterVal) *>
    Statements.insertScienceProgramIdSlice(pid).run

  private def insertDailyProgramIdSlice(pid: ProgramId.Daily): ConnectionIO[Int] =
    Statements.insertDailyProgramIdSlice(pid).run

  private def insertArbitraryProgramIdSlice(pid: ProgramId.Arbitrary): ConnectionIO[Int] =
    pid.semester.traverse(SemesterDao.canonicalize) *>
    Statements.insertArbitraryProgramIdSlice(pid).run

  def selectBySubstring(pat: String, max: Int): ConnectionIO[List[Program[Nothing]]] =
    Statements.selectBySubstring(pat, max).list

  /** Select a program by Id, without any Observation information. */
  def selectFlat(pid: Program.Id): ConnectionIO[Option[Program[Nothing]]] =
    Statements.selectFlat(pid).option

  /** Select a program by id, with full Observation information. */
  def selectFull(pid: Program.Id): ConnectionIO[Option[Program[Observation[StaticConfig, Step[DynamicConfig]]]]] =
    for {
      opn <- selectFlat(pid)
      os  <- ObservationDao.selectAll(pid)
    } yield opn.map(_.copy(observations = os))

  object Statements {

    // StepId has a DISTINCT type due to its check constraint so we need a fine-grained mapping
    // here to satisfy the query checker.
    private final case class Index(toInt: Int)
    private object Index {
      implicit val IndexMeta: Meta[Index] =
        Distinct.integer("id_index").xmap(Index(_), _.toInt)
    }

    def selectFlat(pid: Program.Id): Query0[Program[Nothing]] =
      sql"""
        SELECT title
          FROM program
         WHERE program_id = $pid
      """.query[String]
         .map(Program(pid, _, Nil))

    def selectBySubstring(pat: String, max: Int): Query0[Program[Nothing]] =
      sql"""
       SELECT program_id, title
         FROM program
        WHERE lower(program_id) like lower($pat) OR lower(title) like lower($pat)
      ORDER BY program_id, title
        LIMIT ${max.toLong}
      """.query[(Program.Id, String)]
        .map { case (pid, title) => Program(pid, title, Nil) }

    // N.B. assumes semester has been canonicalized
    def insertArbitraryProgramIdSlice(pid: ProgramId.Arbitrary): Update0 =
      sql"""
        INSERT INTO program (program_id,
                             site,
                             semester_id,
                             program_type)
              VALUES (${pid: Program.Id},
                      ${pid.site},
                      ${pid.semester.map(_.toString)},
                      ${pid.ptype})
      """.update

    def insertDailyProgramIdSlice(pid: ProgramId.Daily): Update0 =
      sql"""
        INSERT INTO program (program_id,
                            site,
                            program_type,
                            day)
              VALUES (${pid: Program.Id},
                      ${pid.siteVal},
                      ${pid.ptypeVal},
                      ${new java.util.Date(pid.start)})
      """.update

    // N.B. assumes semester has been canonicalized
    def insertScienceProgramIdSlice(pid: ProgramId.Science): Update0 =
      sql"""
         INSERT INTO program (program_id,
                             site,
                             semester_id,
                             program_type,
                             index)
             VALUES (${pid: Program.Id},
                     ${pid.siteVal},
                     ${pid.semesterVal.toString},
                     ${pid.ptypeVal},
                     ${Index(pid.index)})
      """.update

    // N.B. assumes program id slice has been inserted
    def insert(p: Program[_]): Update0 =
      sql"""
        UPDATE program
           SET title = ${p.title}
         WHERE program_id = ${p.id}
      """.update

  }

}
