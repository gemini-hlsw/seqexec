package gem
package dao

import edu.gemini.spModel.core._
import edu.gemini.spModel.core.ProgramId._

import doobie.imports._
import doobie.contrib.postgresql.syntax._

import scalaz._, Scalaz._

object ProgramDao {

  def insert(p: Program): ConnectionIO[Int] =
    insertProgramIdSlice(p.id) *> update(p)

  def update(p: Program): ConnectionIO[Int] =
    sql"""
      UPDATE program
         SET title = ${p.title}
       WHERE program_id = ${p.id.toString}
    """.update.run

  def insertProgramIdSlice(pid: ProgramId): ConnectionIO[Int] =
    pid match {
      case id @ Science  (_, _, _, _) => insertScienceProgramIdSlice(id)
      case id @ Daily (_, _, _, _, _) => insertDailyProgramIdSlice(id)
      case id @ Arbitrary(_, _, _, _) => insertArbitraryProgramIdSlice(id)
    }

  def insertScienceProgramIdSlice(pid: ProgramId.Science): ConnectionIO[Int] =
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

  def insertDailyProgramIdSlice(pid: ProgramId.Daily): ConnectionIO[Int] =
    sql"""
      INSERT INTO program (program_id, 
                          site, 
                          program_type, 
                          day)
            VALUES (${pid: Program.Id},
                    ${pid.siteVal.toString},
                    ${pid.ptypeVal.toString},
                    ${new java.util.Date(pid.year + "/" + pid.month + "/" + pid.day)})
    """.update.run

  def insertArbitraryProgramIdSlice(pid: ProgramId.Arbitrary): ConnectionIO[Int] =
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

}


