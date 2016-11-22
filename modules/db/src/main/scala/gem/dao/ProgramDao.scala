package gem
package dao

import gem.enum._
import gem.config._

import edu.gemini.spModel.core._
import edu.gemini.spModel.core.ProgramId._

import doobie.imports._
import doobie.contrib.postgresql.syntax._

import scalaz._, Scalaz._

object ProgramDao {

  ///
  /// INSERT
  ///

  /** Insert a program, disregarding its observations, if any. */
  def insert(p: Program[_]): ConnectionIO[Int] =
    insertProgramIdSlice(p.id) *>
    sql"""
      UPDATE program
         SET title = ${p.title}
       WHERE program_id = ${p.id.toString}
    """.update.run

  private def insertProgramIdSlice(pid: ProgramId): ConnectionIO[Int] =
    pid match {
      case id @ Science  (_, _, _, _) => insertScienceProgramIdSlice(id)
      case id @ Daily (_, _, _, _, _) => insertDailyProgramIdSlice(id)
      case id @ Arbitrary(_, _, _, _) => insertArbitraryProgramIdSlice(id)
    }

  private def insertScienceProgramIdSlice(pid: ProgramId.Science): ConnectionIO[Int] =
    SemesterDao.canonicalize(pid.semesterVal) *>
    sql"""
       INSERT INTO program (program_id,
                           site,
                           semester_id,
                           program_type,
                           index)
           VALUES (${pid: Program.Id},
                   ${pid.siteVal.toString},
                   ${pid.semesterVal.toString},
                   ${pid.ptypeVal.toString},
                   ${pid.index})
    """.update.run

  private def insertDailyProgramIdSlice(pid: ProgramId.Daily): ConnectionIO[Int] =
    sql"""
      INSERT INTO program (program_id,
                          site,
                          program_type,
                          day)
            VALUES (${pid: Program.Id},
                    ${pid.siteVal.toString},
                    ${pid.ptypeVal.toString},
                    ${new java.util.Date(pid.year + "/" + pid.month + "/" + pid.day)}) -- TODO: not this
    """.update.run

  private def insertArbitraryProgramIdSlice(pid: ProgramId.Arbitrary): ConnectionIO[Int] =
    pid.semester.traverse(SemesterDao.canonicalize) *>
    sql"""
      INSERT INTO program (program_id,
                           site,
                           semester_id,
                           program_type)
            VALUES (${pid: Program.Id},
                    ${pid.site.map(_.toString)},
                    ${pid.semester.map(_.toString)},
                    ${pid.ptype.map(_.toString)})
    """.update.run

  ///
  /// SELECT
  ///

  def selectBySubstring(pat: String, max: Int): ConnectionIO[List[Program[Nothing]]] =
    sql"""
      SELECT program_id, title
        FROM program
       WHERE lower(program_id) like lower($pat) OR lower(title) like lower($pat)
    ORDER BY program_id, title
       LIMIT $max
    """.query[(Program.Id, String)]
       .map { case (pid, title) => Program(pid, title, Nil) }
       .list

  /** Select a program by Id, without any Observation information. */
  def selectFlat(pid: Program.Id): ConnectionIO[Option[Program[Nothing]]] =
    sql"""
      SELECT title
        FROM program
       WHERE program_id = $pid
    """.query[String]
       .map(Program(pid, _, Nil))
       .option

  /** Select a program by id, with full Observation information. */
  def selectFull(pid: Program.Id): ConnectionIO[Option[Program[Observation[Step[_]]]]] =
    for {
      opn <- selectFlat(pid)
      os  <- ObservationDao.selectAll(pid)
    } yield opn.map(_.copy(observations = os))
}
