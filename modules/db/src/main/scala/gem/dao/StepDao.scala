package gem
package dao

import edu.gemini.spModel.core._
import gem.config._
import gem.enum._

import doobie.imports._

import java.time.Duration
import scalaz._, Scalaz._

object StepDao {

  def insert[I <: InstrumentConfig](oid: Observation.Id, loc: Location.Middle, s: Step[I]): ConnectionIO[Int] =
    for {
      id <- insertBaseSlice(oid, loc, s.instrument, StepType.forStep(s))
      _  <- s match {
              case BiasStep(_)         => insertBiasSlice(id)
              case DarkStep(_)         => insertDarkSlice(id)
              case ScienceStep(_, t)   => insertScienceSlice(id, t)
              case GcalStep(_, g)      => insertGcalSlice(id, g)
              case SmartGcalStep(_, t) => insertSmartGcalSlice(id, t)
            }
      _  <- insertConfigSlice(id, s.instrument)
    } yield id

  private def insertBaseSlice(oid: Observation.Id, loc: Location.Middle, i: InstrumentConfig, t: StepType): ConnectionIO[Int] =
    for {
      _  <- sql"""INSERT INTO step (observation_id, location, instrument, step_type)
                       VALUES ($oid, $loc, ${Instrument.forConfig(i).tag}, ${t.tag} :: step_type)
               """.update.run
      id <- sql"select lastval()".query[Int].unique
    } yield id

/*******************************************************************************
    TODO: I think this is the preferred way to do insertBaseSlice.

    sql"""
      INSERT INTO step (observation_id, location, instrument, step_type)
      VALUES ($oid, $loc, ${Instrument.forConfig(i).tag}, ${t.tag} :: step_type)
    """.update.withUniqueGeneratedKeys[Int]("step_id").run

    Unfortunately doesn't quite compile ....

    Cannot prove that scalaz.Free[doobie.free.connection.ConnectionOp,Int] =:= scalaz.Free.Trampoline[Int]

*******************************************************************************/


  private def insertBiasSlice(id: Int): ConnectionIO[Int] =
    sql"""
      INSERT INTO step_bias (step_bias_id)
      VALUES ($id)
    """.update.run

  private def insertDarkSlice(id: Int): ConnectionIO[Int] =
    sql"""
      INSERT INTO step_dark (step_dark_id)
      VALUES ($id)
    """.update.run

  private def insertGcalSlice(id: Int, gcal: GcalConfig): ConnectionIO[Int] = {
    def insertGcalStep(gcal_id: Int): ConnectionIO[Int] =
      sql"""
        INSERT into step_gcal (step_gcal_id, gcal_id)
        VALUES ($id, $gcal_id)
      """.update.run

    for {
      gcal_id <- GcalDao.insert(gcal)
      r       <- insertGcalStep(gcal_id)
    } yield r
  }

  private def insertSmartGcalSlice(id: Int, t: SmartGcalType): ConnectionIO[Int] =
    sql"""
      INSERT INTO step_smart_gcal (step_smart_gcal_id, type)
      VALUES ($id, $t)
    """.update.run

  private def insertScienceSlice(id: Int, t: TelescopeConfig): ConnectionIO[Int] =
    sql"""
      INSERT INTO step_science (step_science_id, offset_p, offset_q)
      VALUES ($id, ${t.p}, ${t.q})
    """.update.run

  private def insertConfigSlice(id: Int, i: InstrumentConfig): ConnectionIO[Int] =
    i match {

      case F2Config(fpu, mosPreimaging, exposureTime, filter, lyotWheel, disperser, windowCover) =>
        sql"""
          INSERT INTO step_f2 (step_f2_id, fpu, mos_preimaging, exposure_time, filter, lyot_wheel, disperser, window_cover)
          VALUES ($id, $fpu, $mosPreimaging, ${exposureTime.getSeconds}, $filter, $lyotWheel, $disperser, $windowCover)
        """.update.run

      case GenericConfig(i) => 0.point[ConnectionIO]

    }

  // The type we get when we select the fully joined step
  private case class StepKernel(
    i: Instrument,
    stepType: StepType, // todo: make an enum
    gcal: (Option[GcalContinuum], Option[Boolean], Option[Boolean], Option[Boolean], Option[Boolean], Option[GcalFilter], Option[GcalDiffuser], Option[GcalShutter], Option[Duration], Option[Int]),
    telescope: (Option[OffsetP],  Option[OffsetQ]),
    smartGcalType: Option[SmartGcalType])
  {
    def toStep: Step[Instrument] =
      stepType match {

        case StepType.Bias => BiasStep(i)
        case StepType.Dark => DarkStep(i)

        case StepType.Gcal =>
          import GcalArc._
          val (continuumOpt, arOpt, cuarOpt, tharOpt, xeOpt, filterOpt, diffuserOpt, shutterOpt, exposureOpt, coaddsOpt) = gcal
          (for {
            ar   <- arOpt
            cuar <- cuarOpt
            thar <- tharOpt
            xe   <- xeOpt
            l    <- GcalConfig.mkLamp(continuumOpt, ArArc -> ar, CuArArc -> cuar, ThArArc -> thar, XeArc -> xe)
            f    <- filterOpt
            d    <- diffuserOpt
            s    <- shutterOpt
            e    <- exposureOpt
            c    <- coaddsOpt
          } yield GcalStep(i, GcalConfig(l, f, d, s, e, c))).getOrElse(sys.error("missing gcal information: " + gcal))

        case StepType.SmartGcal =>
          smartGcalType.map(t => SmartGcalStep(i, t)).getOrElse(sys.error("missing smart gcal type"))

        case StepType.Science =>
          telescope.apply2(TelescopeConfig(_, _))
            .map(ScienceStep(i, _))
            .getOrElse(sys.error("missing telescope information: " + telescope))

      }
  }

  def selectAll(oid: Observation.Id): ConnectionIO[List[Step[Instrument]]] =
    sql"""
      SELECT s.instrument,
             s.step_type,
             gc.continuum,
             gc.ar_arc,
             gc.cuar_arc,
             gc.thar_arc,
             gc.xe_arc,
             gc.filter,
             gc.diffuser,
             gc.shutter,
             gc.exposure_time,
             gc.coadds,
             sc.offset_p,
             sc.offset_q,
             ss.type
        FROM step s
             LEFT OUTER JOIN step_gcal sg
                ON sg.step_gcal_id    = s.step_id
             LEFT OUTER JOING gcal gc
                ON gc.gcal_id         = sq.gcal_id
             LEFT OUTER JOIN step_science sc
                ON sc.step_science_id = s.step_id
             LEFT OUTER JOIN step_smart_gcal ss
                ON ss.step_smart_id   = s.step_id
       WHERE s.observation_id = $oid
    ORDER BY s.location
    """.query[StepKernel].map(_.toStep).list

}
