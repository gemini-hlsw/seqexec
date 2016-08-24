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

  def insert(p: Program[_]): ConnectionIO[Int] =
    insertProgramIdSlice(p.id) *> update(p)

  def update(p: Program[_]): ConnectionIO[Int] =
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
                    ${new java.util.Date(pid.year + "/" + pid.month + "/" + pid.day)}) -- TODO: not this
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

  def selectFlat(pid: Program.Id): ConnectionIO[Program[Nothing]] =
    sql"""
      SELECT title
        FROM program
       WHERE program_id = $pid
    """.query[String]
       .map(Program(pid, _, Nil))
       .unique

  // read-only mapping for use with join queries
  private implicit val OptionGCalConfigComposite: Composite[Option[GcalConfig]] =
    Composite[(Option[GCalLamp], Option[GCalShutter])].xmap({ 
      case (l, s) => (l |@| s)(GcalConfig(_, _))
    }, _ => sys.error("decode only"))

  // read-only mapping for use with join queries
  private implicit val OptionTelescopeConfigComposite: Composite[Option[TelescopeConfig]] =
    Composite[(Option[OffsetP], Option[OffsetQ])].xmap({
      case (p, q) => (p |@| q)(TelescopeConfig(_, _))
    }, _ => sys.error("decode only"))

  // read-only mapping for use with join queries
  private implicit val OptionStepComposite: Composite[Option[Step[_]]] =
    Composite[(Option[StepType], Option[Instrument], Option[GcalConfig], Option[TelescopeConfig])].xmap({
      case (None,                   None,    None,    None   ) => None
      case (Some(StepType.Bias),    Some(i), None,    None   ) => Some(BiasStep(i))
      case (Some(StepType.Dark),    Some(i), None,    None   ) => Some(DarkStep(i))
      case (Some(StepType.Gcal),    Some(i), Some(c), None   ) => Some(GcalStep(i, c))
      case (Some(StepType.Science), Some(i), None,    Some(c)) => Some(ScienceStep(i, c))
      case x => sys.error("Unexpected Option[Step] inputs: " + x)
    }, _ => sys.error("decode only"))

  // Option[(Observation, Option[Step]])]
  private implicit val OptionObservationOptionStep: Composite[Option[(Observation[Nothing], Option[Step[_]])]] =
    Composite[(Option[Observation.Id], Option[String], Option[Step[_]])].xmap({
      case (None,      None,        None) => None
      case (Some(oid), Some(title), step) => Some((Observation(oid, title, None, Nil), step))
      case x => sys.error("Unexpected Option[(Observation[Nothing], Option[Step[_]])] inputs: " + x)
    }, _ => sys.error("decode only"))

  def selectFull(pid: Program.Id): ConnectionIO[Option[Program[Observation[Step[_]]]]] =
    sql"""
      SELECT p.program_id,
             p.title,
             o.observation_id,
             o.title,
             s.step_type,
             s.instrument,
             sg.gcal_lamp,
             sg.shutter,
             sc.offset_p,
             sc.offset_q
        FROM program p
             LEFT OUTER JOIN observation o ON o.program_id = p.program_id
             LEFT OUTER JOIN step s ON s.observation_id = o.observation_id
             LEFT OUTER JOIN step_gcal sg
                ON sg.observation_id = s.observation_id AND sg.index = s.index
             LEFT OUTER JOIN step_science sc
                ON sc.observation_id = s.observation_id AND sc.index = s.index
       WHERE p.program_id = $pid
    ORDER BY o.observation_id, s.index;
    """.query[(Program.Id, String, Option[(Observation[Nothing], Option[Step[_]])])]
       .list.map { rows =>
         rows.headOption.map { case (pid, title, _) => 

           // Compute the list of observations by grouping by Obs[Nothing] and then
           // collecting the associated steps, if any, which will remain in order
           // through this transformation
           val obs: List[Observation[Step[_]]] = 
             rows.collect { case (_, _, Some(p)) => p }
                 .groupBy(_._1)
                 .toList
                 .map { case (obs, rows) =>
                   obs.copy(steps = rows.collect { case (_, Some(s)) => s } )
                 }

            // Done!
            Program(pid, title, obs)

         }
       }

}


