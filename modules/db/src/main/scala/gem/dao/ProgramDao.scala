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

  // The full program select is a 5-table join. Decoding requires some busywork that's made slightly
  // simpler by factoring out sub-encoders for different subsets of columns.
  private implicit val OptionGcalConfigComposite: Composite[Option[GcalConfig]] = {
    import GcalConfig.GcalArcs
    import GcalArc._
    Composite[(Option[GcalContinuum], Boolean, Boolean, Boolean, Boolean, Option[GcalShutter])].xmap({
      case (None, _, _, _, _, None) =>
        None

      case (None, ar, cuar, thar, xe, Some(shutter)) =>
        GcalConfig.mkLamp(None, ArArc -> ar, CuArArc -> cuar, ThArArc -> thar, XeArc -> xe).map {
          GcalConfig(_, shutter)
        }

      case (Some(continuum), _, _, _, _, Some(shutter)) =>
        Some(GcalConfig(continuum.left[GcalArcs], shutter))

      case x => sys.error("Unexecpted Option[GcalConfig] inputs: " + x)
    }, _ => sys.error("decode only"))
  }


  private implicit val OptionTelescopeConfigComposite = capply2(TelescopeConfig)

  private implicit val OptionStepComposite: Composite[Option[Step[_]]] =
    Composite[(Option[StepType], Option[Instrument], Option[GcalConfig], Option[TelescopeConfig])].xmap({
      case (None,                   None,    None,    None   ) => None
      case (Some(StepType.Bias),    Some(i), None,    None   ) => Some(BiasStep(i))
      case (Some(StepType.Dark),    Some(i), None,    None   ) => Some(DarkStep(i))
      case (Some(StepType.Gcal),    Some(i), Some(c), None   ) => Some(GcalStep(i, c))
      case (Some(StepType.Science), Some(i), None,    Some(c)) => Some(ScienceStep(i, c))
      case x => sys.error("Unexpected Option[Step] inputs: " + x)
    }, _ => sys.error("decode only"))

  private implicit val OptionObservationOptionStep: Composite[Option[(Observation[Nothing], Option[Step[_]])]] =
    Composite[(Option[Observation.Id], Option[String], Option[Step[_]])].xmap({
      case (None,      None,        None) => None
      case (Some(oid), Some(title), step) => Some((Observation(oid, title, None, Nil), step))
      case x => sys.error("Unexpected Option[(Observation[Nothing], Option[Step[_]])] inputs: " + x)
    }, _ => sys.error("decode only"))

  /** Select a program by Id, with fully-populated Observations and steps. */
  def selectFull(pid: Program.Id): ConnectionIO[Option[Program[Observation[Step[_]]]]] =
    sql"""
      SELECT p.program_id,
             p.title,
             o.observation_id,
             o.title,
             s.step_type,
             s.instrument,
             sg.continuum,
             sg.ar_arc,
             sg.cuar_arc,
             sg.thar_arc,
             sg.xe_arc,
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

           // Compute the list of observations by grouping by Obs[Nothing] and then collecting the
           // associated steps, if any, which will remain in order through this transformation.
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
