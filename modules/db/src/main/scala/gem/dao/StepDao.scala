package gem
package dao

import edu.gemini.spModel.core._
import gem.Location
import gem.config._
import gem.config.GcalConfig.GcalLamp
import gem.enum._
import doobie.imports._

import java.time.Duration

import scalaz._
import Scalaz._

object StepDao {

  type Loc = Location.Middle

  def insert[I <: InstrumentConfig](oid: Observation.Id, loc: Loc, s: Step[I]): ConnectionIO[Int] =
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

  private def insertBaseSlice(oid: Observation.Id, loc: Loc, i: InstrumentConfig, t: StepType): ConnectionIO[Int] =
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
      gcal_id <- GcalDao.insert(gcal, Some(id))
      r       <- insertGcalStep(gcal_id)
    } yield r
  }

  private def insertSmartGcalSlice(id: Int, t: SmartGcalType): ConnectionIO[Int] =
    sql"""
      INSERT INTO step_smart_gcal (step_smart_gcal_id, type)
      VALUES ($id, $t :: smart_gcal_type)
    """.update.run

  private def insertScienceSlice(id: Int, t: TelescopeConfig): ConnectionIO[Int] =
    sql"""
      INSERT INTO step_science (step_science_id, offset_p, offset_q)
      VALUES ($id, ${t.p}, ${t.q})
    """.update.run

  private def insertConfigSlice(id: Int, i: InstrumentConfig): ConnectionIO[Int] =
    i match {

      case F2Config(disperser, exposureTime, filter, fpu, lyotWheel, mosPreimaging, readMode, windowCover) =>
        sql"""
          INSERT INTO step_f2 (step_f2_id, disperser, exposure_time, filter, fpu, lyot_wheel, mos_preimaging, read_mode, window_cover)
          VALUES ($id, $disperser, $exposureTime, $filter, $fpu, $lyotWheel, $mosPreimaging, $readMode, $windowCover)
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
            l    <- GcalLamp.fromConfig(continuumOpt, ArArc -> ar, CuArArc -> cuar, ThArArc -> thar, XeArc -> xe)
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

  /** Selects a `Step` with an `Instrument` element at the given location in the
    * sequence but without any instrument configuration information.
    *
    * @param oid observation whose step should be selected
    * @param loc position of the step to select
    */
  def selectOneEmpty(oid: Observation.Id, loc: Loc): MaybeConnectionIO[Step[Instrument]] =
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
                ON sg.step_gcal_id       = s.step_id
             LEFT OUTER JOIN gcal gc
                ON gc.gcal_id            = sg.gcal_id
             LEFT OUTER JOIN step_science sc
                ON sc.step_science_id    = s.step_id
             LEFT OUTER JOIN step_smart_gcal ss
                ON ss.step_smart_gcal_id = s.step_id
       WHERE s.observation_id = $oid AND s.location = $loc
    """.query[StepKernel].map(_.toStep).maybe


  /** Selects `Step`s with an `Instrument` element but without any instrument
    * configuration information.
    *
    * @param oid observation whose steps should be selected
    */
  def selectAllEmpty(oid: Observation.Id): ConnectionIO[Loc ==>> Step[Instrument]] =
    sql"""
      SELECT s.location,
             s.instrument,
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
                ON sg.step_gcal_id       = s.step_id
             LEFT OUTER JOIN gcal gc
                ON gc.gcal_id            = sg.gcal_id
             LEFT OUTER JOIN step_science sc
                ON sc.step_science_id    = s.step_id
             LEFT OUTER JOIN step_smart_gcal ss
                ON ss.step_smart_gcal_id = s.step_id
       WHERE s.observation_id = $oid
    """.query[(Loc, StepKernel)].map(_.map(_.toStep)).list.map(==>>.fromList(_))


  private def oneF2Only(oid: Observation.Id, loc: Loc): MaybeConnectionIO[F2Config] =
    sql"""
      SELECT i.disperser,
             i.exposure_time,
             i.filter,
             i.fpu,
             i.lyot_wheel,
             i.mos_preimaging,
             i.read_mode,
             i.window_cover
        FROM step s
             LEFT OUTER JOIN step_f2 i
               ON i.step_f2_id = s.step_id
       WHERE s.observation_id = $oid AND s.location = $loc
    """.query[F2Config].maybe

  private def allF2Only(oid: Observation.Id): ConnectionIO[Loc ==>> F2Config] =
    sql"""
      SELECT s.location,
             i.disperser,
             i.exposure_time,
             i.filter,
             i.fpu,
             i.lyot_wheel,
             i.mos_preimaging,
             i.read_mode,
             i.window_cover
        FROM step s
             LEFT OUTER JOIN step_f2 i
               ON i.step_f2_id = s.step_id
       WHERE s.observation_id = $oid
    """.query[(Loc, F2Config)].list.map(==>>.fromList(_))

  private def oneGenericOnly(oid: Observation.Id, loc: Loc): MaybeConnectionIO[GenericConfig] =
    sql"""
      SELECT instrument
        FROM step
       WHERE observation_id = $oid AND location = $loc
    """.query[Instrument].map(GenericConfig).maybe

  private def allGenericOnly(oid: Observation.Id): ConnectionIO[Loc ==>> GenericConfig] =
    sql"""
      SELECT location,
             instrument
        FROM step
       WHERE observation_id = $oid
    """.query[(Loc, Instrument)].map(_.map(GenericConfig)).list.map(==>>.fromList(_))


  private def selectAll[I](oid: Observation.Id, f: Observation.Id => ConnectionIO[Loc ==>> I]): ConnectionIO[Loc ==>> Step[I]] =
    for {
      ss <- selectAllEmpty(oid)
      is <- f(oid)
    } yield ss.intersectionWith(is) { (s, i) => s.as(i) } // .map { case ((l, s), i) => (l, s.as(i)) }

  /** Selects all steps with their F2 instrument configuration data, assuming
    * the indicated observation is an F2 observation.  If not, fails with an
    * exception.
    *
    * @param oid F2 observation whose steps are sought
    */
  def selectAllF2(oid: Observation.Id): ConnectionIO[Loc ==>> Step[F2Config]] =
    selectAll(oid, allF2Only)

  /** Selects all steps with a generic instrument configuration data.
    *
    * @param oid observation whose steps are sought
    */
  def selectAllGeneric(oid: Observation.Id): ConnectionIO[Loc ==>> Step[GenericConfig]] =
    selectAll(oid, allGenericOnly)

  /** Selects the step at the indicated location in the sequence associated with
    * the indicated observation.
    *
    * @param oid observation whose step configuration is sought
    * @param loc location within the sequence to find
    */
  def selectOne(oid: Observation.Id, loc: Loc): MaybeConnectionIO[Step[InstrumentConfig]] = {
    def instrumentConfig(s: Step[Instrument]): MaybeConnectionIO[InstrumentConfig] =
      s.instrument match {
        case Instrument.Flamingos2 => oneF2Only(oid, loc)     .widen[InstrumentConfig]
        case _                     => oneGenericOnly(oid, loc).widen[InstrumentConfig]
      }

    for {
      s <- selectOneEmpty(oid, loc)
      i <- instrumentConfig(s)
    } yield s.as(i)
  }

  /** Selects all steps with their instrument configuration data.
    *
    * @param oid observation whose step configurations are sought
    */
  def selectAll(oid: Observation.Id): ConnectionIO[Loc ==>> Step[InstrumentConfig]] = {
    def instrumentConfig(ss: Loc ==>> Step[Instrument]): ConnectionIO[Loc ==>> InstrumentConfig] =
      ss.findMin.map(_._2.instrument).fold(==>>.empty[Loc, InstrumentConfig].point[ConnectionIO]) {
        case Instrument.Flamingos2 => allF2Only(oid)     .map(_.widen[InstrumentConfig])
        case _                     => allGenericOnly(oid).map(_.widen[InstrumentConfig])
      }

    for {
      ss <- selectAllEmpty(oid)
      is <- instrumentConfig(ss)
    } yield ss.intersectionWith(is) { (s, i) => s.as(i) }
  }

  /** Deletes the step at the indicated location, if any.
    *
    * @param oid observation whose step should be deleted
    * @param loc location of the step to delete
    */
  def delete(oid: Observation.Id, loc: Loc): ConnectionIO[Int] =
    sql"""
      DELETE FROM step
            WHERE observation_id = $oid
              AND location       = $loc
    """.update.run

  /** Deletes all steps for the given observation, if any.
    *
    * @param oid observation whose steps should be deleted
    */
  def delete(oid: Observation.Id): ConnectionIO[Int] =
    sql"""
      DELETE FROM step
            WHERE observation_id = $oid
    """.update.run
}
