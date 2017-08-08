// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package dao

import gem.Location
import gem.config._
import gem.config.GcalConfig.GcalLamp
import gem.enum._
import gem.math.{ Offset, Wavelength }
import doobie.imports._

import java.time.Duration

import scalaz._
import Scalaz._

object StepDao {

  type Loc = Location.Middle

  private implicit class ToMap[C](q: Query0[(Loc, C)]) {
    def toMap: ConnectionIO[Loc ==>> C] =
      q.list.map(==>>.fromList(_))
  }

  import Statements._


  def insert[I <: DynamicConfig](oid: Observation.Id, loc: Loc, s: Step[I]): ConnectionIO[Int] =
    for {
      id <- Statements.insertBaseSlice(oid, loc, s.dynamicConfig, StepType.forStep(s)).withUniqueGeneratedKeys[Int]("step_id")
      _  <- s match {
              case Step.Bias(_)         => Statements.insertBiasSlice(id).run
              case Step.Dark(_)         => Statements.insertDarkSlice(id).run
              case Step.Science(_, t)   => Statements.insertScienceSlice(id, t).run
              case Step.SmartGcal(_, t) => Statements.insertSmartGcalSlice(id, t).run
              case Step.Gcal(_, g)      => GcalDao.insertStepGcal(id, g)
            }
      _  <- insertConfigSlice(id, s.dynamicConfig)
    } yield id

  /** Selects a `Step` with an `Instrument` element at the given location in the
    * sequence but without any instrument configuration information.
    *
    * @param oid observation whose step should be selected
    * @param loc position of the step to select
    */
  def selectOneEmpty(oid: Observation.Id, loc: Loc): MaybeConnectionIO[Step[Instrument]] =
    Statements.selectOneEmpty(oid, loc).maybe

  /** Selects `Step`s with an `Instrument` element but without any instrument
    * configuration information.
    *
    * @param oid observation whose steps should be selected
    */
  def selectAllEmpty(oid: Observation.Id): ConnectionIO[Loc ==>> Step[Instrument]] =
    Statements.selectAllEmpty(oid).list.map(==>>.fromList(_))

  /** Selects the step at the indicated location in the sequence associated with
    * the indicated observation.
    *
    * @param oid observation whose step configuration is sought
    * @param loc location within the sequence to find
    */
  def selectOne(oid: Observation.Id, loc: Loc): MaybeConnectionIO[Step[DynamicConfig]] = {
    def point(dc: DynamicConfig): MaybeConnectionIO[DynamicConfig] =
      dc.point[MaybeConnectionIO]

    def instrumentConfig(s: Step[Instrument]): MaybeConnectionIO[DynamicConfig] =
      s.dynamicConfig match {
        case Instrument.AcqCam     => point(DynamicConfig.AcqCam())
        case Instrument.Bhros      => point(DynamicConfig.Bhros())
        case Instrument.Flamingos2 => F2.selectOne(oid, loc)       .maybe.widen[DynamicConfig]
        case Instrument.GmosN      => Gmos.selectOneNorth(oid, loc).maybe.widen[DynamicConfig]
        case Instrument.GmosS      => Gmos.selectOneSouth(oid, loc).maybe.widen[DynamicConfig]
        case Instrument.Gnirs      => point(DynamicConfig.Gnirs())
        case Instrument.Gpi        => point(DynamicConfig.Gpi())
        case Instrument.Gsaoi      => point(DynamicConfig.Gsaoi())
        case Instrument.Michelle   => point(DynamicConfig.Michelle())
        case Instrument.Nici       => point(DynamicConfig.Nici())
        case Instrument.Nifs       => point(DynamicConfig.Nifs())
        case Instrument.Niri       => point(DynamicConfig.Niri())
        case Instrument.Phoenix    => point(DynamicConfig.Phoenix())
        case Instrument.Trecs      => point(DynamicConfig.Trecs())
        case Instrument.Visitor    => point(DynamicConfig.Visitor())
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
  def selectAll(oid: Observation.Id): ConnectionIO[Loc ==>> Step[DynamicConfig]] = {
    def point(dc: DynamicConfig): ConnectionIO[Loc ==>> DynamicConfig] =
      ==>>(Location.unsafeMiddle(0) -> dc).point[ConnectionIO]

    def instrumentConfig(ss: Loc ==>> Step[Instrument]): ConnectionIO[Loc ==>> DynamicConfig] =
      ss.findMin.map(_._2.dynamicConfig).fold(==>>.empty[Loc, DynamicConfig].point[ConnectionIO]) {
        case Instrument.AcqCam     => point(DynamicConfig.AcqCam())
        case Instrument.Bhros      => point(DynamicConfig.Bhros())
        case Instrument.Flamingos2 => F2.selectAll(oid)       .toMap.map(_.widen[DynamicConfig])
        case Instrument.GmosN      => Gmos.selectAllNorth(oid).toMap.map(_.widen[DynamicConfig])
        case Instrument.GmosS      => Gmos.selectAllSouth(oid).toMap.map(_.widen[DynamicConfig])
        case Instrument.Gnirs      => point(DynamicConfig.Gnirs())
        case Instrument.Gpi        => point(DynamicConfig.Gpi())
        case Instrument.Gsaoi      => point(DynamicConfig.Gsaoi())
        case Instrument.Michelle   => point(DynamicConfig.Michelle())
        case Instrument.Nici       => point(DynamicConfig.Nici())
        case Instrument.Nifs       => point(DynamicConfig.Nifs())
        case Instrument.Niri       => point(DynamicConfig.Niri())
        case Instrument.Phoenix    => point(DynamicConfig.Phoenix())
        case Instrument.Trecs      => point(DynamicConfig.Trecs())
        case Instrument.Visitor    => point(DynamicConfig.Visitor())
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
  def deleteAtLocation(oid: Observation.Id, loc: Loc): ConnectionIO[Int] =
    Statements.deleteAtLocation(oid, loc).run

  /** Deletes all steps for the given observation, if any.
    *
    * @param oid observation whose steps should be deleted
    */
  def delete(oid: Observation.Id): ConnectionIO[Int] =
    Statements.delete(oid).run

  // HELPERS

  private def insertConfigSlice(id: Int, i: DynamicConfig): ConnectionIO[Unit] =
    i match {
      case _: DynamicConfig.AcqCam    => ().point[ConnectionIO]
      case _: DynamicConfig.Bhros     => ().point[ConnectionIO]
      case f2: DynamicConfig.F2       => F2.insert(id, f2).run.void
      case g: DynamicConfig.GmosNorth => Gmos.insertCommon(id, g.common).run *>
                                           Gmos.insertNorth(id, g).run.void
      case g: DynamicConfig.GmosSouth => Gmos.insertCommon(id, g.common).run *>
                                           Gmos.insertSouth(id, g).run.void
      case _: DynamicConfig.Gnirs     => ().point[ConnectionIO]
      case _: DynamicConfig.Gpi       => ().point[ConnectionIO]
      case _: DynamicConfig.Gsaoi     => ().point[ConnectionIO]
      case _: DynamicConfig.Michelle  => ().point[ConnectionIO]
      case _: DynamicConfig.Nici      => ().point[ConnectionIO]
      case _: DynamicConfig.Nifs      => ().point[ConnectionIO]
      case _: DynamicConfig.Niri      => ().point[ConnectionIO]
      case _: DynamicConfig.Phoenix   => ().point[ConnectionIO]
      case _: DynamicConfig.Trecs     => ().point[ConnectionIO]
      case _: DynamicConfig.Visitor   => ().point[ConnectionIO]
    }

  // The type we get when we select the fully joined step
  private final case class StepKernel(
    i: Instrument,
    stepType: StepType, // todo: make an enum
    gcal: (Option[GcalContinuum], Option[Boolean], Option[Boolean], Option[Boolean], Option[Boolean], Option[GcalFilter], Option[GcalDiffuser], Option[GcalShutter], Option[Duration], Option[Short]),
    telescope: (Option[Offset.P],  Option[Offset.Q]),
    smartGcalType: Option[SmartGcalType])
  {
    def toStep: Step[Instrument] =
      stepType match {

        case StepType.Bias => Step.Bias(i)
        case StepType.Dark => Step.Dark(i)

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
          } yield Step.Gcal(i, GcalConfig(l, f, d, s, e, c))).getOrElse(sys.error(s"missing gcal information: $gcal"))

        case StepType.SmartGcal =>
          smartGcalType.map(t => Step.SmartGcal(i, t)).getOrElse(sys.error("missing smart gcal type"))

        case StepType.Science =>
          telescope.apply2(TelescopeConfig(_, _))
            .map(Step.Science(i, _))
            .getOrElse(sys.error(s"missing telescope information: $telescope"))

      }
  }

  object Statements {

    def deleteAtLocation(oid: Observation.Id, loc: Loc): Update0 =
      sql"""
        DELETE FROM step
              WHERE observation_id = $oid
                AND location       = $loc
      """.update

    def delete(oid: Observation.Id): Update0 =
      sql"""
        DELETE FROM step
              WHERE observation_id = $oid
      """.update

    def selectAllEmpty(oid: Observation.Id): Query0[(Loc, Step[Instrument])] =
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
         WHERE s.observation_id = $oid
      """.query[(Loc, StepKernel)].map(_.map(_.toStep))

    def selectOneEmpty(oid: Observation.Id, loc: Loc): Query0[Step[Instrument]] =
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
         WHERE s.observation_id = $oid AND s.location = $loc
      """.query[StepKernel].map(_.toStep)

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

    def insertBaseSlice(oid: Observation.Id, loc: Loc, i: DynamicConfig, t: StepType): Update0 =
      sql"""
        INSERT INTO step (observation_id, location, instrument, step_type)
        VALUES ($oid, $loc, ${i.instrument: Instrument}, ${t} :: step_type)
      """.update

    object F2 {
      import F2Config.F2FpuChoice
      import F2Config.F2FpuChoice._

      final case class F2FpuBuilder(fpu: Option[F2FpUnit], customMask: Boolean) {
        def toFpu: Option[F2FpuChoice] =
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
        def toF2: DynamicConfig.F2 =
          DynamicConfig.F2(disperser, exposureTime, filter, fpuBuilder.toFpu, lyotWheel, readMode, windowCover)
      }

      def selectAll(oid: Observation.Id): Query0[(Loc, DynamicConfig.F2)] =
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
           WHERE s.observation_id = $oid
        """.query[(Loc, F2Builder)].map(_.map(_.toF2))

      def selectOne(oid: Observation.Id, loc: Loc): Query0[DynamicConfig.F2] =
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
           WHERE s.observation_id = $oid AND s.location = $loc
        """.query[F2Builder].map(_.toF2)

      def insert(id: Int, f2: DynamicConfig.F2): Update0 =
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

      import gem.config.Gmos.{ GmosCommonDynamicConfig, GmosCustomMask, GmosGrating }
      import DynamicConfig.{ GmosNorth, GmosSouth }

      final case class GmosGratingBuilder[D](
                         disperser: Option[D],
                         disperserOrder: Option[GmosDisperserOrder],
                         wavelength: Option[Int]) {


        def toGrating: Option[GmosGrating[D]] =
          for {
            d <- disperser
            o <- disperserOrder
            w <- wavelength.flatMap(Wavelength.fromAngstroms)
          } yield GmosGrating(d, o, w)
      }

      final case class GmosFpuBuilder[U](
                         mdfFileName: Option[String],
                         customSlitWidth: Option[GmosCustomSlitWidth],
                         builtin: Option[U]) {

        val toFpu: Option[GmosCustomMask \/ U] = {
          val customMask: Option[GmosCustomMask] =
            for {
              m <- mdfFileName
              w <- customSlitWidth
            } yield GmosCustomMask(m, w)

          customMask.map(_.left[U]) orElse
            builtin.map(_.right[GmosCustomMask])
        }
      }

      final case class GmosNorthBuilder(
                         c: GmosCommonDynamicConfig,
                         g: GmosGratingBuilder[GmosNorthDisperser],
                         f: Option[GmosNorthFilter],
                         u: GmosFpuBuilder[GmosNorthFpu]) {

        val toDynamicConfig: GmosNorth =
          GmosNorth(c, g.toGrating, f, u.toFpu)
      }

      final case class GmosSouthBuilder(
                         c: GmosCommonDynamicConfig,
                         g: GmosGratingBuilder[GmosSouthDisperser],
                         f: Option[GmosSouthFilter],
                         u: GmosFpuBuilder[GmosSouthFpu]) {

        val toDynamicConfig: GmosSouth =
          GmosSouth(c, g.toGrating, f, u.toFpu)
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
          fr"""WHERE s.observation_id = $oid"""

      def selectAllNorth(oid: Observation.Id): Query0[(Loc, GmosNorth)] =
        selectFragment(withLocation = true, "step_gmos_north", oid)
          .query[(Loc, GmosNorthBuilder)].map(_.map(_.toDynamicConfig))

      def selectOneNorth(oid: Observation.Id, loc: Loc): Query0[GmosNorth] =
        (selectFragment(withLocation = false, "step_gmos_north", oid) ++
          fr"""AND s.location = $loc"""
          ).query[GmosNorthBuilder].map(_.toDynamicConfig)

      def selectAllSouth(oid: Observation.Id): Query0[(Loc, GmosSouth)] =
        selectFragment(withLocation = true, "step_gmos_south", oid)
          .query[(Loc, GmosSouthBuilder)].map(_.map(_.toDynamicConfig))

      def selectOneSouth(oid: Observation.Id, loc: Loc): Query0[GmosSouth] =
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

      def insertNorth(id: Int, g: GmosNorth): Update0 =
        sql"""
          INSERT INTO step_gmos_north (
            step_id,
            disperser, disperser_order, wavelength, filter, mdf_file_name, custom_slit_width, fpu
          )
          VALUES (
            $id,
            ${g.grating.map(_.disperser)},
            ${g.grating.map(_.order)},
            ${g.grating.map(_.wavelength.toAngstroms)},
            ${g.filter},
            ${g.fpu.flatMap(_.swap.toOption.map(_.maskDefinitionFilename))},
            ${g.fpu.flatMap(_.swap.toOption.map(_.slitWidth))},
            ${g.fpu.flatMap(_.toOption)})
        """.update

      def insertSouth(id: Int, g: GmosSouth): Update0 =
        sql"""
          INSERT INTO step_gmos_south (
            step_id,
            disperser, disperser_order, wavelength, filter, mdf_file_name, custom_slit_width, fpu
          )
          VALUES (
            $id,
            ${g.grating.map(_.disperser)},
            ${g.grating.map(_.order)},
            ${g.grating.map(_.wavelength.toAngstroms)},
            ${g.filter},
            ${g.fpu.flatMap(_.swap.toOption.map(_.maskDefinitionFilename))},
            ${g.fpu.flatMap(_.swap.toOption.map(_.slitWidth))},
            ${g.fpu.flatMap(_.toOption)})
        """.update
    }
  }
}
