// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package dao

import cats.implicits._
import doobie._, doobie.implicits._
import gem.util.Location
import gem.CoAdds
import gem.config._
import gem.config.GcalConfig.GcalLamp
import gem.dao.composite._
import gem.dao.meta._
import gem.enum._
import gsp.math.Offset
import gsp.math.syntax.treemap._
import java.time.Duration
import scala.collection.immutable.TreeMap

object StepDao {
  import CoAddsMeta._
  import EnumeratedMeta._
  import LocationMeta._
  import ProgramIdMeta._
  import IndexMeta._
  import OffsetMeta._
  import TimeMeta._
  import WavelengthMeta._

  type Loc = Location.Middle

  private implicit class ToMap[A](q: Query0[(Loc, A)]) {
    def toMap[B >: A]: ConnectionIO[TreeMap[Loc, B]] =
      q.to[List].map(ps => TreeMap.fromList(ps.widen[(Loc, B)]))
  }

  import Statements._


  def insert(oid: Observation.Id, loc: Loc, s: Step): ConnectionIO[Int] =
    for {
      id <- Statements.insertBaseSlice(oid, loc, Instrument.forStep(s), StepType.forStep(s)).withUniqueGeneratedKeys[Int]("step_id")
      _  <- s.base match {
              case Step.Base.Bias         => Statements.insertBiasSlice(id).run
              case Step.Base.Dark         => Statements.insertDarkSlice(id).run
              case Step.Base.Science(t)   => Statements.insertScienceSlice(id, t).run
              case Step.Base.SmartGcal(t) => Statements.insertSmartGcalSlice(id, t).run
              case Step.Base.Gcal(g)      => GcalDao.insertStepGcal(id, g)
            }
      _  <- insertConfigSlice(id, s.dynamicConfig)
    } yield id

  /** Selects a `Step` with an `Instrument` element at the given location in the
    * sequence but without any instrument configuration information.
    *
    * @param oid observation whose step should be selected
    * @param loc position of the step to select
    */
  def selectOneInstrumentAndBase(oid: Observation.Id, loc: Loc): MaybeConnectionIO[(Instrument, Step.Base)] =
    Statements.selectOneInstrumentAndBase(oid, loc).maybe

  /** Selects `Step`s with an `Instrument` element but without any instrument
    * configuration information.
    *
    * @param oid observation whose steps should be selected
    */
  def selectAllInstrumentAndBase(oid: Observation.Id): ConnectionIO[TreeMap[Loc, (Instrument, Step.Base)]] =
    Statements.selectAllInstrumentAndBase(oid).to[List].map(ps => TreeMap.fromList(ps))

  /** Selects the step at the indicated location in the sequence associated with
    * the indicated observation.
    *
    * @param oid observation whose step configuration is sought
    * @param loc location within the sequence to find
    */
  def selectOne(oid: Observation.Id, loc: Loc): MaybeConnectionIO[Step] =
    selectOneInstrumentAndBase(oid, loc).flatMap {
      case (Instrument.AcqCam, b)     => MaybeConnectionIO.some(DynamicConfig.AcqCam().toStep(b))
      case (Instrument.Bhros, b)      => MaybeConnectionIO.some(DynamicConfig.Bhros().toStep(b))
      case (Instrument.Flamingos2, b) => Flamingos2.selectOne(oid, loc).map(_.toStep(b): Step).maybe
      case (Instrument.Ghost, b)      => MaybeConnectionIO.some(DynamicConfig.Ghost().toStep(b))
      case (Instrument.GmosN, b)      => Gmos.selectOneNorth(oid, loc).map(_.toStep(b): Step).maybe
      case (Instrument.GmosS, b)      => Gmos.selectOneSouth(oid, loc).map(_.toStep(b): Step).maybe
      case (Instrument.Gnirs, b)      => Gnirs.selectOne(oid, loc)    .map(_.toStep(b): Step).maybe
      case (Instrument.Gpi, b)        => MaybeConnectionIO.some(DynamicConfig.Gpi().toStep(b))
      case (Instrument.Gsaoi, b)      => MaybeConnectionIO.some(DynamicConfig.Gsaoi().toStep(b))
      case (Instrument.Michelle, b)   => MaybeConnectionIO.some(DynamicConfig.Michelle().toStep(b))
      case (Instrument.Nici, b)       => MaybeConnectionIO.some(DynamicConfig.Nici().toStep(b))
      case (Instrument.Nifs, b)       => MaybeConnectionIO.some(DynamicConfig.Nifs().toStep(b))
      case (Instrument.Niri, b)       => MaybeConnectionIO.some(DynamicConfig.Niri().toStep(b))
      case (Instrument.Phoenix, b)    => MaybeConnectionIO.some(DynamicConfig.Phoenix().toStep(b))
      case (Instrument.Trecs, b)      => MaybeConnectionIO.some(DynamicConfig.Trecs().toStep(b))
      case (Instrument.Visitor, b)    => MaybeConnectionIO.some(DynamicConfig.Visitor().toStep(b))
    }

  /** Selects all steps with their instrument configuration data.
    *
    * @param oid observation whose step configurations are sought
    */
  def selectAll(oid: Observation.Id): ConnectionIO[TreeMap[Loc, Step]] = {

    def pure(dc: DynamicConfig): ConnectionIO[TreeMap[Loc, DynamicConfig]] =
      TreeMap(Location.unsafeMiddle(0) -> dc).pure[ConnectionIO]

    def instrumentConfig(ss: TreeMap[Loc, (Instrument, Step.Base)]): ConnectionIO[TreeMap[Loc, DynamicConfig]] =
      ss.headOption.map(_._2).fold(TreeMap.empty[Loc, DynamicConfig].pure[ConnectionIO]) {
        case (Instrument.AcqCam, _)     => pure(DynamicConfig.AcqCam())
        case (Instrument.Bhros, _)      => pure(DynamicConfig.Bhros())
        case (Instrument.Flamingos2, _) => Flamingos2.selectAll(oid)       .toMap[DynamicConfig] //.map(a => a: TreeMap[Loc, DynamicConfig])
        case (Instrument.Ghost, _)      => pure(DynamicConfig.Ghost())
        case (Instrument.GmosN, _)      => Gmos.selectAllNorth(oid).toMap[DynamicConfig] //.map(a => a: TreeMap[Loc, DynamicConfig])
        case (Instrument.GmosS, _)      => Gmos.selectAllSouth(oid).toMap[DynamicConfig] //.map(a => a: TreeMap[Loc, DynamicConfig])
        case (Instrument.Gnirs, _)      => Gnirs.selectAll(oid)    .toMap[DynamicConfig] //.map(a => a: TreeMap[Loc, DynamicConfig])
        case (Instrument.Gpi, _)        => pure(DynamicConfig.Gpi())
        case (Instrument.Gsaoi, _)      => pure(DynamicConfig.Gsaoi())
        case (Instrument.Michelle, _)   => pure(DynamicConfig.Michelle())
        case (Instrument.Nici, _)       => pure(DynamicConfig.Nici())
        case (Instrument.Nifs, _)       => pure(DynamicConfig.Nifs())
        case (Instrument.Niri, _)       => pure(DynamicConfig.Niri())
        case (Instrument.Phoenix, _)    => pure(DynamicConfig.Phoenix())
        case (Instrument.Trecs, _)      => pure(DynamicConfig.Trecs())
        case (Instrument.Visitor, _)    => pure(DynamicConfig.Visitor())
      }

    // n log n
    def intersect[K: Ordering, A, B, C](ma: TreeMap[K, A], mb: TreeMap[K, B])(f: (A, B) => C): TreeMap[K, C] =
      ma.toList.foldLeft(TreeMap.empty[K, C]) { case (kc, (k, a)) =>
        mb.get(k).fold(kc)(b => kc + (k -> f(a, b)))
      }

    for {
      ss <- selectAllInstrumentAndBase(oid)
      is <- instrumentConfig(ss)
    } yield intersect(ss, is) { case ((_, b), dc) => dc.toStep(b) }

  }

  /** Deletes the step at the indicated location, if any.
    *
    * @param oid observation whose step should be deleted
    * @param loc location of the step to delete
    */
  def deleteAtLocation(oid: Observation.Id, loc: Loc): ConnectionIO[Int] =
    Statements.deleteAtLocation(oid, loc).run

  /** Deletes all steps for the given observation, if any.
    *
    * @param oid observation whose steps should be deleted
    */
  def delete(oid: Observation.Id): ConnectionIO[Int] =
    Statements.delete(oid).run

  // HELPERS

  private def insertConfigSlice(id: Int, d: DynamicConfig): ConnectionIO[Unit] =
    d match {
      case _: DynamicConfig.AcqCam     => FC.unit
      case _: DynamicConfig.Bhros      => FC.unit
      case i: DynamicConfig.Flamingos2 => Flamingos2.insert(id, i).run.void
      case _: DynamicConfig.Ghost      => FC.unit
      case i: DynamicConfig.GmosN      => Gmos.insertCommon(id, i.common).run *> Gmos.insertNorth(id, i).run.void
      case i: DynamicConfig.GmosS      => Gmos.insertCommon(id, i.common).run *> Gmos.insertSouth(id, i).run.void
      case g: DynamicConfig.Gnirs      => Gnirs.insert(id, g).run.void
      case _: DynamicConfig.Gpi        => FC.unit
      case _: DynamicConfig.Gsaoi      => FC.unit
      case _: DynamicConfig.Michelle   => FC.unit
      case _: DynamicConfig.Nici       => FC.unit
      case _: DynamicConfig.Nifs       => FC.unit
      case _: DynamicConfig.Niri       => FC.unit
      case _: DynamicConfig.Phoenix    => FC.unit
      case _: DynamicConfig.Trecs      => FC.unit
      case _: DynamicConfig.Visitor    => FC.unit
    }

  // The type we get when we select the fully joined step
  private final case class StepKernel(
    i: Instrument,
    stepType: StepType, // todo: make an enum
    gcal: (Option[GcalContinuum], Option[Boolean], Option[Boolean], Option[Boolean], Option[Boolean], Option[GcalFilter], Option[GcalDiffuser], Option[GcalShutter], Option[Duration], Option[Short]),
    telescope: (Option[Offset.P],  Option[Offset.Q]),
    smartGcalType: Option[SmartGcalType])
  {
    def toInstrumentAndBase: (Instrument, Step.Base) =
      stepType match {

        case StepType.Bias => (i, Step.Base.Bias)
        case StepType.Dark => (i, Step.Base.Dark)

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
            c    <- coaddsOpt.flatMap(CoAdds.fromShort.getOption)
          } yield (i, Step.Base.Gcal(GcalConfig(l, f, d, s, e, c)))).getOrElse(sys.error(s"missing gcal information: $gcal"))

        case StepType.SmartGcal =>
          smartGcalType.map(t => (i, Step.Base.SmartGcal(t))).getOrElse(sys.error("missing smart gcal type"))

        case StepType.Science =>
          telescope.mapN(TelescopeConfig(_, _))
            .map(c => (i, Step.Base.Science(c)))
            .getOrElse(sys.error(s"missing telescope information: $telescope"))

      }
  }

  object Statements {

    def deleteAtLocation(oid: Observation.Id, loc: Loc): Update0 =
      sql"""
        DELETE FROM step
              WHERE program_id        = ${oid.pid}
                AND observation_index = ${oid.index}
                AND location          = $loc
      """.update

    def delete(oid: Observation.Id): Update0 =
      sql"""
        DELETE FROM step
              WHERE program_id        = ${oid.pid}
                AND observation_index = ${oid.index}
      """.update

    def selectAllInstrumentAndBase(oid: Observation.Id): Query0[(Loc, (Instrument, Step.Base))] =
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
               LEFT OUTER JOIN step_gcal gc
                  ON gc.step_gcal_id       = s.step_id
               LEFT OUTER JOIN step_science sc
                  ON sc.step_science_id    = s.step_id
               LEFT OUTER JOIN step_smart_gcal ss
                  ON ss.step_smart_gcal_id = s.step_id
         WHERE s.program_id        = ${oid.pid}
           AND s.observation_index = ${oid.index}
      """.query[(Loc, StepKernel)].map(_.map(_.toInstrumentAndBase))

    def selectOneInstrumentAndBase(oid: Observation.Id, loc: Loc): Query0[(Instrument, Step.Base)] =
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
               LEFT OUTER JOIN step_gcal gc
                  ON gc.step_gcal_id       = s.step_id
               LEFT OUTER JOIN step_science sc
                  ON sc.step_science_id    = s.step_id
               LEFT OUTER JOIN step_smart_gcal ss
                  ON ss.step_smart_gcal_id = s.step_id
         WHERE s.program_id        = ${oid.pid}
           AND s.observation_index = ${oid.index}
           AND s.location          = $loc
      """.query[StepKernel].map(_.toInstrumentAndBase)

    def insertScienceSlice(id: Int, t: TelescopeConfig): Update0 =
      sql"""
        INSERT INTO step_science (step_science_id, offset_p, offset_q)
        VALUES ($id, ${t.p}, ${t.q})
      """.update

    def insertSmartGcalSlice(id: Int, t: SmartGcalType): Update0 =
      sql"""
        INSERT INTO step_smart_gcal (step_smart_gcal_id, type)
        VALUES ($id, $t :: smart_gcal_type)
      """.update

    def insertDarkSlice(id: Int): Update0 =
      sql"""
        INSERT INTO step_dark (step_dark_id)
        VALUES ($id)
      """.update

    def insertBiasSlice(id: Int): Update0 =
      sql"""
        INSERT INTO step_bias (step_bias_id)
        VALUES ($id)
      """.update

    def insertBaseSlice(oid: Observation.Id, loc: Loc, i: Instrument, t: StepType): Update0 =
      sql"""
        INSERT INTO step (program_id, observation_index, location, instrument, step_type)
        VALUES (${oid.pid}, ${oid.index}, $loc, ${i: Instrument}, ${t} :: step_type)
      """.update

    object Flamingos2 {
      import F2Config.F2FpuChoice
      import F2Config.F2FpuChoice._

      final case class F2FpuBuilder(fpu: Option[F2Fpu], customMask: Boolean) {
        def toFpuChoice: Option[F2FpuChoice] =
          if (customMask) Some(Custom) else fpu.map(Builtin(_))
      }

      final case class F2Builder(
        disperser:     Option[F2Disperser],
        exposureTime:  Duration,
        filter:        F2Filter,
        fpuBuilder:    F2FpuBuilder,
        lyotWheel:     F2LyotWheel,
        readMode:      F2ReadMode,
        windowCover:   F2WindowCover
      ) {
        def toF2: DynamicConfig.Flamingos2 =
          DynamicConfig.Flamingos2(disperser, exposureTime, filter, fpuBuilder.toFpuChoice, lyotWheel, readMode, windowCover)
      }

      def selectAll(oid: Observation.Id): Query0[(Loc, DynamicConfig.Flamingos2)] =
        sql"""
          SELECT s.location,
                 i.disperser,
                 i.exposure_time,
                 i.filter,
                 i.fpu,
                 i.custom_mask,
                 i.lyot_wheel,
                 i.read_mode,
                 i.window_cover
            FROM step s
                 LEFT OUTER JOIN step_f2 i
                   ON i.step_f2_id = s.step_id
           WHERE s.program_id        = ${oid.pid}
             AND s.observation_index = ${oid.index}
        """.query[(Loc, F2Builder)].map(_.map(_.toF2))

      def selectOne(oid: Observation.Id, loc: Loc): Query0[DynamicConfig.Flamingos2] =
        sql"""
          SELECT i.disperser,
                 i.exposure_time,
                 i.filter,
                 i.fpu,
                 i.custom_mask,
                 i.lyot_wheel,
                 i.read_mode,
                 i.window_cover
            FROM step s
                 LEFT OUTER JOIN step_f2 i
                   ON i.step_f2_id = s.step_id
           WHERE s.program_id        = ${oid.pid}
             AND s.observation_index = ${oid.index}
             AND s.location          = $loc
        """.query[F2Builder].map(_.toF2)

      def insert(id: Int, f2: DynamicConfig.Flamingos2): Update0 =
        sql"""
          INSERT INTO step_f2 (
            step_f2_id,
            disperser, exposure_time, filter, fpu, custom_mask, lyot_wheel, read_mode, window_cover
          )
          VALUES (
            $id,
            ${f2.disperser},
            ${f2.exposureTime},
            ${f2.filter},
            ${f2.fpu.flatMap(_.toBuiltin)},
            ${f2.fpu.contains(Custom)},
            ${f2.lyotWheel},
            ${f2.readMode},
            ${f2.windowCover})
        """.update
    }

    object Gmos {

      implicit val WavelengthMeta = WavelengthMetaAsNanometers

      import gem.config.GmosConfig.{ GmosCommonDynamicConfig, GmosCustomMask, GmosGrating }
      import DynamicConfig.{ GmosN, GmosS }

      final case class GmosFpuBuilder[U](
                         mdfFileName: Option[String],
                         customSlitWidth: Option[GmosCustomSlitWidth],
                         builtin: Option[U]) {

        val toFpu: Option[Either[GmosCustomMask, U]] = {
          val customMask: Option[GmosCustomMask] =
            for {
              m <- mdfFileName
              w <- customSlitWidth
            } yield GmosCustomMask(m, w)

          customMask.map(Left(_)) orElse
            builtin.map(Right(_))
        }
      }

      final case class GmosNorthBuilder(
                         c: GmosCommonDynamicConfig,
                         g: Option[GmosGrating[GmosNorthDisperser]],
                         f: Option[GmosNorthFilter],
                         u: GmosFpuBuilder[GmosNorthFpu]) {

        val toDynamicConfig: GmosN =
          GmosN(c, g, f, u.toFpu)
      }

      final case class GmosSouthBuilder(
                         c: GmosCommonDynamicConfig,
                         g: Option[GmosGrating[GmosSouthDisperser]],
                         f: Option[GmosSouthFilter],
                         u: GmosFpuBuilder[GmosSouthFpu]) {

        val toDynamicConfig: GmosS =
          GmosS(c, g, f, u.toFpu)
      }

      private def selectFragment(withLocation: Boolean, table: String, oid: Observation.Id): Fragment =
        Fragment.const(
          s"""SELECT ${if (withLocation) "s.location," else ""}
                     c.x_binning,
                     c.y_binning,
                     c.amp_count,
                     c.amp_gain,
                     c.amp_read_mode,
                     c.dtax,
                     c.exposure_time,
                     c.roi,
                     i.disperser,
                     i.disperser_order,
                     i.wavelength,
                     i.filter,
                     i.mdf_file_name,
                     i.custom_slit_width,
                     i.fpu
                FROM step s
                     LEFT OUTER JOIN step_gmos_common c
                       ON c.step_id = s.step_id
                     LEFT OUTER JOIN $table i
                       ON i.step_id = s.step_id
            """) ++
          fr"""WHERE s.program_id = ${oid.pid} AND s.observation_index = ${oid.index}"""

      def selectAllNorth(oid: Observation.Id): Query0[(Loc, GmosN)] =
        selectFragment(withLocation = true, "step_gmos_north", oid)
          .query[(Loc, GmosNorthBuilder)].map(_.map(_.toDynamicConfig))

      def selectOneNorth(oid: Observation.Id, loc: Loc): Query0[GmosN] =
        (selectFragment(withLocation = false, "step_gmos_north", oid) ++
          fr"""AND s.location = $loc"""
          ).query[GmosNorthBuilder].map(_.toDynamicConfig)

      def selectAllSouth(oid: Observation.Id): Query0[(Loc, GmosS)] =
        selectFragment(withLocation = true, "step_gmos_south", oid)
          .query[(Loc, GmosSouthBuilder)].map(_.map(_.toDynamicConfig))

      def selectOneSouth(oid: Observation.Id, loc: Loc): Query0[GmosS] =
        (selectFragment(withLocation = false, "step_gmos_south", oid) ++
          fr"""AND s.location = $loc"""
          ).query[GmosSouthBuilder].map(_.toDynamicConfig)


      def insertCommon(id: Int, g: GmosCommonDynamicConfig): Update0 =
        sql"""
          INSERT INTO step_gmos_common (
            step_id,
            x_binning, y_binning, amp_count, amp_gain, amp_read_mode, dtax, exposure_time, roi
          )
          VALUES (
            $id,
            ${g.ccdReadout.xBinning},
            ${g.ccdReadout.yBinning},
            ${g.ccdReadout.ampCount},
            ${g.ccdReadout.ampGain},
            ${g.ccdReadout.ampReadMode},
            ${g.dtaxOffset},
            ${g.exposureTime},
            ${g.roi})
        """.update

      def insertNorth(id: Int, g: GmosN): Update0 =
        sql"""
          INSERT INTO step_gmos_north (
            step_id,
            disperser, disperser_order, wavelength, filter, mdf_file_name, custom_slit_width, fpu
          )
          VALUES (
            $id,
            ${g.grating.map(_.disperser)},
            ${g.grating.map(_.order)},
            ${g.grating.map(_.wavelength)},
            ${g.filter},
            ${g.fpu.flatMap(_.swap.toOption.map(_.maskDefinitionFilename))},
            ${g.fpu.flatMap(_.swap.toOption.map(_.slitWidth))},
            ${g.fpu.flatMap(_.toOption)})
        """.update

      def insertSouth(id: Int, g: GmosS): Update0 =
        sql"""
          INSERT INTO step_gmos_south (
            step_id,
            disperser, disperser_order, wavelength, filter, mdf_file_name, custom_slit_width, fpu
          )
          VALUES (
            $id,
            ${g.grating.map(_.disperser)},
            ${g.grating.map(_.order)},
            ${g.grating.map(_.wavelength)},
            ${g.filter},
            ${g.fpu.flatMap(_.swap.toOption.map(_.maskDefinitionFilename))},
            ${g.fpu.flatMap(_.swap.toOption.map(_.slitWidth))},
            ${g.fpu.flatMap(_.toOption)})
        """.update
    }

    object Gnirs {

      implicit val WavelengthMeta = WavelengthMetaAsMicrometers

      import EitherComposite._

      def selectAll(oid: Observation.Id): Query0[(Loc, DynamicConfig.Gnirs)] =
        sql"""
          SELECT s.location,
                 i.acquisition_mirror,
                 i.camera,
                 i.coadds,
                 i.decker,
                 i.disperser,
                 i.exposure_time,
                 i.filter,
                 i.fpu_other,
                 i.fpu_slit,
                 i.prism,
                 i.read_mode,
                 i.wavelength
            FROM step s
                 LEFT OUTER JOIN step_gnirs i
                   ON i.step_gnirs_id = s.step_id
           WHERE s.program_id        = ${oid.pid}
             AND s.observation_index = ${oid.index}
        """.query

      def selectOne(oid: Observation.Id, loc: Loc): Query0[DynamicConfig.Gnirs] =
        sql"""
          SELECT i.acquisition_mirror,
                 i.camera,
                 i.coadds,
                 i.decker,
                 i.disperser,
                 i.exposure_time,
                 i.filter,
                 i.fpu_other,
                 i.fpu_slit,
                 i.prism,
                 i.read_mode,
                 i.wavelength
            FROM step s
                 LEFT OUTER JOIN step_gnirs i
                   ON i.step_gnirs_id = s.step_id
           WHERE s.program_id        = ${oid.pid}
             AND s.observation_index = ${oid.index}
             AND s.location          = $loc
        """.query

      def insert(id: Int, gnirs: DynamicConfig.Gnirs): Update0 =
        sql"""
          INSERT INTO step_gnirs (
            step_gnirs_id,
            acquisition_mirror,
            camera,
            coadds,
            decker,
            disperser,
            exposure_time,
            filter,
            fpu_other,
            fpu_slit,
            prism,
            read_mode,
            wavelength
          )
          VALUES (
            $id,
            ${gnirs.acquisitionMirror},
            ${gnirs.camera},
            ${gnirs.coadds},
            ${gnirs.decker},
            ${gnirs.disperser},
            ${gnirs.exposureTime},
            ${gnirs.filter},
            ${gnirs.fpu.swap.toOption},
            ${gnirs.fpu.toOption},
            ${gnirs.prism},
            ${gnirs.readMode},
            ${gnirs.wavelength})
        """.update
    }

  }
}
