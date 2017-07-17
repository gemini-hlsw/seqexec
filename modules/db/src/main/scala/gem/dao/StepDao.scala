// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package dao

import gem.Location
import gem.config._
import gem.config.GcalConfig.GcalLamp
import gem.enum._
import gem.math.Offset
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
              case Step.Gcal(_, g)      => GcalDao.insert(g, Some(id)) >>= (Statements.insertGcalStep(id, _).run)
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

  /** Selects all steps with their F2 instrument configuration data, assuming
    * the indicated observation is an F2 observation.  If not, fails with an
    * exception.
    *
    * @param oid F2 observation whose steps are sought
    */
  def selectAllF2(oid: Observation.Id): ConnectionIO[Loc ==>> Step[DynamicConfig.F2]] =
    selectAllʹ(oid, allF2Only)

  /** Selects all steps with their GMOS North instrument configuration data,
    * assuming the indicated observation is a GMOS North observation.  If not,
    * fails with an exception.
    *
    * @param oid GMOS North observation whose steps are sought
    */
  def selectAllGmosNorth(oid: Observation.Id): ConnectionIO[Loc ==>> Step[DynamicConfig.GmosNorth]] =
    selectAllʹ(oid, allGmosNorthOnly)

  /** Selects all steps with their GMOS South instrument configuration data,
    * assuming the indicated observation is a GMOS South observation.  If not,
    * fails with an exception.
    *
    * @param oid GMOS South observation whose steps are sought
    */
  def selectAllGmosSouth(oid: Observation.Id): ConnectionIO[Loc ==>> Step[DynamicConfig.GmosSouth]] =
    selectAllʹ(oid, allGmosSouthOnly)

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
        case Instrument.Flamingos2 => oneF2Only(oid, loc)       .maybe.widen[DynamicConfig]
        case Instrument.GmosN      => oneGmosNorthOnly(oid, loc).maybe.widen[DynamicConfig]
        case Instrument.GmosS      => oneGmosSouthOnly(oid, loc).maybe.widen[DynamicConfig]
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
        case Instrument.Flamingos2 => allF2Only(oid)       .toMap.map(_.widen[DynamicConfig])
        case Instrument.GmosN      => allGmosNorthOnly(oid).toMap.map(_.widen[DynamicConfig])
        case Instrument.GmosS      => allGmosSouthOnly(oid).toMap.map(_.widen[DynamicConfig])
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

  private def insertConfigSlice(id: Int, i: DynamicConfig): ConnectionIO[Int] =
    i match {
      case _: DynamicConfig.AcqCam    => 0.point[ConnectionIO]
      case _: DynamicConfig.Bhros     => 0.point[ConnectionIO]
      case f2: DynamicConfig.F2       => insertF2Config(id, f2).run
      case g: DynamicConfig.GmosNorth => insertGmosCommonConfig(id, g.common).run *>
                                        insertGmosNorthConfig(id, g).run
      case g: DynamicConfig.GmosSouth => insertGmosCommonConfig(id, g.common).run *>
                                        insertGmosSouthConfig(id, g).run
      case _: DynamicConfig.Gnirs     => 0.point[ConnectionIO]
      case _: DynamicConfig.Gpi       => 0.point[ConnectionIO]
      case _: DynamicConfig.Gsaoi     => 0.point[ConnectionIO]
      case _: DynamicConfig.Michelle  => 0.point[ConnectionIO]
      case _: DynamicConfig.Nici      => 0.point[ConnectionIO]
      case _: DynamicConfig.Nifs      => 0.point[ConnectionIO]
      case _: DynamicConfig.Niri      => 0.point[ConnectionIO]
      case _: DynamicConfig.Phoenix   => 0.point[ConnectionIO]
      case _: DynamicConfig.Trecs     => 0.point[ConnectionIO]
      case _: DynamicConfig.Visitor   => 0.point[ConnectionIO]
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

  private def selectAllʹ[I](oid: Observation.Id, q: Observation.Id => Query0[(Loc, I)]): ConnectionIO[Loc ==>> Step[I]] =
    for {
      ss <- selectAllEmpty(oid)
      is <- q(oid).toMap
    } yield ss.intersectionWith(is) { (s, i) => s.as(i) }

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

    def allF2Only(oid: Observation.Id): Query0[(Loc, DynamicConfig.F2)] =
      sql"""
        SELECT s.location,
               i.disperser,
               i.exposure_time,
               i.filter,
               i.fpu,
               i.lyot_wheel,
               i.read_mode,
               i.window_cover
          FROM step s
               LEFT OUTER JOIN step_f2 i
                 ON i.step_f2_id = s.step_id
         WHERE s.observation_id = $oid
      """.query[(Loc, DynamicConfig.F2)]

    def oneF2Only(oid: Observation.Id, loc: Loc): Query0[DynamicConfig.F2] =
      sql"""
        SELECT i.disperser,
               i.exposure_time,
               i.filter,
               i.fpu,
               i.lyot_wheel,
               i.read_mode,
               i.window_cover
          FROM step s
               LEFT OUTER JOIN step_f2 i
                 ON i.step_f2_id = s.step_id
         WHERE s.observation_id = $oid AND s.location = $loc
      """.query[DynamicConfig.F2]

    final case class GmosGratingBuilder[D](
      disperser:      Option[D],
      disperserOrder: Option[GmosDisperserOrder],
      wavelength:     Option[BigDecimal]
    ) {
      import Gmos._

      def toGrating: Option[Gmos.GmosGrating[D]] =
        for {
          d <- disperser
          o <- disperserOrder
          w <- wavelength
        } yield GmosGrating(d, o, GmosCentralWavelength(w.toDouble))
    }

    final case class GmosFpuBuilder[U](
      mdfFileName:     Option[String],
      customSlitWidth: Option[GmosCustomSlitWidth],
      builtin:         Option[U]
    ) {
      import Gmos._

      val toFpu: Option[Gmos.GmosCustomMask \/ U] = {
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
      c: Gmos.GmosCommonDynamicConfig,
      g: GmosGratingBuilder[GmosNorthDisperser],
      f: Option[GmosNorthFilter],
      u: GmosFpuBuilder[GmosNorthFpu]
    ) {
      val toDynamicConfig: DynamicConfig.GmosNorth =
        DynamicConfig.GmosNorth(c, g.toGrating, f, u.toFpu)
    }

    final case class GmosSouthBuilder(
      c: Gmos.GmosCommonDynamicConfig,
      g: GmosGratingBuilder[GmosSouthDisperser],
      f: Option[GmosSouthFilter],
      u: GmosFpuBuilder[GmosSouthFpu]
    ) {
      val toDynamicConfig: DynamicConfig.GmosSouth =
        DynamicConfig.GmosSouth(c, g.toGrating, f, u.toFpu)
    }

    private def gmosSelectFragment(withLocation: Boolean, table: String, oid: Observation.Id): Fragment =
      Fragment.const(
        s"""SELECT ${if (withLocation) "s.location," else ""}
                   c.x_binning,
                   c.y_binning,
                   c.amp_count,
                   c.amp_gain,
                   c.amp_read_mode,
                   c.dtax,
                   c.exposure_time,
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

    def allGmosNorthOnly(oid: Observation.Id): Query0[(Loc, DynamicConfig.GmosNorth)] =
      gmosSelectFragment(withLocation = true, "step_gmos_north", oid)
        .query[(Loc, GmosNorthBuilder)].map(_.map(_.toDynamicConfig))

    def oneGmosNorthOnly(oid: Observation.Id, loc: Loc): Query0[DynamicConfig.GmosNorth] =
      (gmosSelectFragment(withLocation = false, "step_gmos_north", oid) ++
        fr"""AND s.location = $loc"""
      ).query[GmosNorthBuilder].map(_.toDynamicConfig)

    def allGmosSouthOnly(oid: Observation.Id): Query0[(Loc, DynamicConfig.GmosSouth)] =
      gmosSelectFragment(withLocation = true, "step_gmos_south", oid)
        .query[(Loc, GmosSouthBuilder)].map(_.map(_.toDynamicConfig))

    def oneGmosSouthOnly(oid: Observation.Id, loc: Loc): Query0[DynamicConfig.GmosSouth] =
      (gmosSelectFragment(withLocation = false, "step_gmos_south", oid) ++
        fr"""AND s.location = $loc"""
      ).query[GmosSouthBuilder].map(_.toDynamicConfig)


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
               LEFT OUTER JOIN step_gcal sg
                  ON sg.step_gcal_id       = s.step_id
               LEFT OUTER JOIN gcal gc
                  ON gc.gcal_id            = sg.gcal_id
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
               LEFT OUTER JOIN step_gcal sg
                  ON sg.step_gcal_id       = s.step_id
               LEFT OUTER JOIN gcal gc
                  ON gc.gcal_id            = sg.gcal_id
               LEFT OUTER JOIN step_science sc
                  ON sc.step_science_id    = s.step_id
               LEFT OUTER JOIN step_smart_gcal ss
                  ON ss.step_smart_gcal_id = s.step_id
         WHERE s.observation_id = $oid AND s.location = $loc
      """.query[StepKernel].map(_.toStep)

    def insertF2Config(id: Int, f2: DynamicConfig.F2): Update0 =
      sql"""
        INSERT INTO step_f2 (
          step_f2_id,
          disperser, exposure_time, filter, fpu, lyot_wheel, read_mode, window_cover
        )
        VALUES (
          $id,
          ${f2.disperser},
          ${f2.exposureTime},
          ${f2.filter},
          ${f2.fpu},
          ${f2.lyotWheel},
          ${f2.readMode},
          ${f2.windowCover})
      """.update

    def insertGmosCommonConfig(id: Int, g: Gmos.GmosCommonDynamicConfig): Update0 =
      sql"""
        INSERT INTO step_gmos_common (
          step_id,
          x_binning, y_binning, amp_count, amp_gain, amp_read_mode, dtax, exposure_time
        )
        VALUES (
          $id,
          ${g.ccdReadout.xBinning},
          ${g.ccdReadout.yBinning},
          ${g.ccdReadout.ampCount},
          ${g.ccdReadout.ampGain},
          ${g.ccdReadout.ampReadMode},
          ${g.dtaxOffset},
          ${g.exposureTime})
      """.update

    def insertGmosNorthConfig(id: Int, g: DynamicConfig.GmosNorth): Update0 =
      sql"""
        INSERT INTO step_gmos_north (
          step_id,
          disperser, disperser_order, wavelength, filter, mdf_file_name, custom_slit_width, fpu
        )
        VALUES (
          $id,
          ${g.grating.map(_.disperser)},
          ${g.grating.map(_.order)},
          ${g.grating.map(_.wavelength.nm)},
          ${g.filter},
          ${g.fpu.flatMap(_.swap.toOption.map(_.maskDefinitionFilename))},
          ${g.fpu.flatMap(_.swap.toOption.map(_.slitWidth))},
          ${g.fpu.flatMap(_.toOption)})
      """.update

    def insertGmosSouthConfig(id: Int, g: DynamicConfig.GmosSouth): Update0 =
      sql"""
        INSERT INTO step_gmos_south (
          step_id,
          disperser, disperser_order, wavelength, filter, mdf_file_name, custom_slit_width, fpu
        )
        VALUES (
          $id,
          ${g.grating.map(_.disperser)},
          ${g.grating.map(_.order)},
          ${g.grating.map(_.wavelength.nm)},
          ${g.filter},
          ${g.fpu.flatMap(_.swap.toOption.map(_.maskDefinitionFilename))},
          ${g.fpu.flatMap(_.swap.toOption.map(_.slitWidth))},
          ${g.fpu.flatMap(_.toOption)})
      """.update

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

    def insertGcalStep(id: Int, gcal_id: Int): Update0 =
      sql"""
        INSERT into step_gcal (step_gcal_id, gcal_id)
        VALUES ($id, $gcal_id)
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

  }

}
