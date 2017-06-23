// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

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

  private implicit class ToMap[C](q: Query0[(Loc, C)]) {
    def toMap: ConnectionIO[Loc ==>> C] =
      q.list.map(==>>.fromList(_))
  }

  import Statements._


  def insert[I <: DynamicConfig](oid: Observation.Id, loc: Loc, s: Step[I]): ConnectionIO[Int] =
    for {
      id <- Statements.insertBaseSlice(oid, loc, s.dynamicConfig, StepType.forStep(s)).withUniqueGeneratedKeys[Int]("step_id")
      _  <- s match {
              case BiasStep(_)         => Statements.insertBiasSlice(id).run
              case DarkStep(_)         => Statements.insertDarkSlice(id).run
              case ScienceStep(_, t)   => Statements.insertScienceSlice(id, t).run
              case SmartGcalStep(_, t) => Statements.insertSmartGcalSlice(id, t).run
              case GcalStep(_, g)      => GcalDao.insert(g, Some(id)) >>= (Statements.insertGcalStep(id, _).run)
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
  def selectAllF2(oid: Observation.Id): ConnectionIO[Loc ==>> Step[F2DynamicConfig]] =
    selectAllʹ(oid, allF2Only)

  /** Selects all steps with their GMOS North instrument configuration data,
    * assuming the indicated observation is a GMOS North observation.  If not,
    * fails with an exception.
    *
    * @param oid GMOS North observation whose steps are sought
    */
  def selectAllGmosNorth(oid: Observation.Id): ConnectionIO[Loc ==>> Step[GmosNorthDynamicConfig]] =
    selectAllʹ(oid, allGmosNorthOnly)

  /** Selects all steps with their GMOS South instrument configuration data,
    * assuming the indicated observation is a GMOS South observation.  If not,
    * fails with an exception.
    *
    * @param oid GMOS South observation whose steps are sought
    */
  def selectAllGmosSouth(oid: Observation.Id): ConnectionIO[Loc ==>> Step[GmosSouthDynamicConfig]] =
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
        case Instrument.AcqCam     => point(AcqCamDynamicConfig())
        case Instrument.Bhros      => point(BhrosDynamicConfig())
        case Instrument.Flamingos2 => oneF2Only(oid, loc)       .maybe.widen[DynamicConfig]
        case Instrument.GmosN      => oneGmosNorthOnly(oid, loc).maybe.widen[DynamicConfig]
        case Instrument.GmosS      => oneGmosSouthOnly(oid, loc).maybe.widen[DynamicConfig]
        case Instrument.Gnirs      => point(GnirsDynamicConfig())
        case Instrument.Gpi        => point(GpiDynamicConfig())
        case Instrument.Gsaoi      => point(GsaoiDynamicConfig())
        case Instrument.Michelle   => point(MichelleDynamicConfig())
        case Instrument.Nici       => point(NiciDynamicConfig())
        case Instrument.Nifs       => point(NifsDynamicConfig())
        case Instrument.Niri       => point(NiriDynamicConfig())
        case Instrument.Phoenix    => point(PhoenixDynamicConfig())
        case Instrument.Trecs      => point(TrecsDynamicConfig())
        case Instrument.Visitor    => point(VisitorDynamicConfig())
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
        case Instrument.AcqCam     => point(AcqCamDynamicConfig())
        case Instrument.Bhros      => point(BhrosDynamicConfig())
        case Instrument.Flamingos2 => allF2Only(oid)       .toMap.map(_.widen[DynamicConfig])
        case Instrument.GmosN      => allGmosNorthOnly(oid).toMap.map(_.widen[DynamicConfig])
        case Instrument.GmosS      => allGmosSouthOnly(oid).toMap.map(_.widen[DynamicConfig])
        case Instrument.Gnirs      => point(GnirsDynamicConfig())
        case Instrument.Gpi        => point(GpiDynamicConfig())
        case Instrument.Gsaoi      => point(GsaoiDynamicConfig())
        case Instrument.Michelle   => point(MichelleDynamicConfig())
        case Instrument.Nici       => point(NiciDynamicConfig())
        case Instrument.Nifs       => point(NifsDynamicConfig())
        case Instrument.Niri       => point(NiriDynamicConfig())
        case Instrument.Phoenix    => point(PhoenixDynamicConfig())
        case Instrument.Trecs      => point(TrecsDynamicConfig())
        case Instrument.Visitor    => point(VisitorDynamicConfig())
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
      case _: AcqCamDynamicConfig    => 0.point[ConnectionIO]
      case _: BhrosDynamicConfig     => 0.point[ConnectionIO]
      case f2: F2DynamicConfig       => insertF2Config(id, f2).run
      case g: GmosNorthDynamicConfig => insertGmosCommonConfig(id, g.common).run *>
                                        insertGmosNorthConfig(id, g).run
      case g: GmosSouthDynamicConfig => insertGmosCommonConfig(id, g.common).run *>
                                        insertGmosSouthConfig(id, g).run
      case _: GnirsDynamicConfig     => 0.point[ConnectionIO]
      case _: GpiDynamicConfig       => 0.point[ConnectionIO]
      case _: GsaoiDynamicConfig     => 0.point[ConnectionIO]
      case _: MichelleDynamicConfig  => 0.point[ConnectionIO]
      case _: NiciDynamicConfig      => 0.point[ConnectionIO]
      case _: NifsDynamicConfig      => 0.point[ConnectionIO]
      case _: NiriDynamicConfig      => 0.point[ConnectionIO]
      case _: PhoenixDynamicConfig   => 0.point[ConnectionIO]
      case _: TrecsDynamicConfig     => 0.point[ConnectionIO]
      case _: VisitorDynamicConfig   => 0.point[ConnectionIO]
    }

  // The type we get when we select the fully joined step
  private final case class StepKernel(
    i: Instrument,
    stepType: StepType, // todo: make an enum
    gcal: (Option[GcalContinuum], Option[Boolean], Option[Boolean], Option[Boolean], Option[Boolean], Option[GcalFilter], Option[GcalDiffuser], Option[GcalShutter], Option[Duration], Option[Short]),
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
          } yield GcalStep(i, GcalConfig(l, f, d, s, e, c))).getOrElse(sys.error(s"missing gcal information: $gcal"))

        case StepType.SmartGcal =>
          smartGcalType.map(t => SmartGcalStep(i, t)).getOrElse(sys.error("missing smart gcal type"))

        case StepType.Science =>
          telescope.apply2(TelescopeConfig(_, _))
            .map(ScienceStep(i, _))
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

    def allF2Only(oid: Observation.Id): Query0[(Loc, F2DynamicConfig)] =
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
      """.query[(Loc, F2DynamicConfig)]

    def oneF2Only(oid: Observation.Id, loc: Loc): Query0[F2DynamicConfig] =
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
      """.query[F2DynamicConfig]

    final case class GmosNorthIntermediary(
      disperser:       Option[GmosNorthDisperser],
      disperserOrder:  Option[GmosDisperserOrder],
      wavelength:      Option[BigDecimal],
      filter:          Option[GmosNorthFilter],
      mdfFileName:     Option[String],
      customSlitWidth: Option[GmosCustomSlitWidth],
      builtin:         Option[GmosNorthFpu]
    ) {
      import Gmos._

      def toDynamicConfig(common: GmosCommonDynamicConfig): GmosNorthDynamicConfig = {
        val grating: Option[GmosNorthGrating] =
          for {
            d <- disperser
            o <- disperserOrder
            w <- wavelength
          } yield GmosNorthGrating(d, o, GmosCentralWavelength(w.toDouble))

        val customMask: Option[GmosCustomMask] =
          for {
            m <- mdfFileName
            w <- customSlitWidth
          } yield GmosCustomMask(m, w)

        val fpu = customMask.map(_.left[GmosNorthFpu]) orElse
                  builtin.map(_.right[GmosCustomMask])

        GmosNorthDynamicConfig(common, grating, filter, fpu)
      }
    }

    def allGmosNorthOnly(oid: Observation.Id): Query0[(Loc, GmosNorthDynamicConfig)] =
      sql"""
        SELECT s.location,
               i.disperser,
               i.disperser_order,
               i.wavelength,
               i.filter,
               i.mdf_file_name,
               i.custom_slit_width,
               i.fpu,
               c.x_binning,
               c.y_binning,
               c.amp_count,
               c.amp_gain,
               c.amp_read_mode,
               c.dtax,
               c.exposure_time
          FROM step s
               LEFT OUTER JOIN step_gmos_common c
                 ON c.step_id = s.step_id
               LEFT OUTER JOIN step_gmos_north i
                 ON i.step_id = s.step_id
         WHERE s.observation_id = $oid
      """.query[(Loc, GmosNorthIntermediary, Gmos.GmosCommonDynamicConfig)].map {
        case (l, i, c) => (l, i.toDynamicConfig(c))
      }


    def oneGmosNorthOnly(oid: Observation.Id, loc: Loc): Query0[GmosNorthDynamicConfig] =
      sql"""
        SELECT i.disperser,
               i.disperser_order,
               i.wavelength,
               i.filter,
               i.mdf_file_name,
               i.custom_slit_width,
               i.fpu,
               c.x_binning,
               c.y_binning,
               c.amp_count,
               c.amp_gain,
               c.amp_read_mode,
               c.dtax,
               c.exposure_time
          FROM step s
               LEFT OUTER JOIN step_gmos_common c
                 ON c.step_id = s.step_id
               LEFT OUTER JOIN step_gmos_north i
                 ON i.step_id = s.step_id
         WHERE s.observation_id = $oid AND s.location = $loc
      """.query[(GmosNorthIntermediary, Gmos.GmosCommonDynamicConfig)].map {
        case (i, c) => i.toDynamicConfig(c)
      }

    final case class GmosSouthIntermediary(
      disperser:       Option[GmosSouthDisperser],
      disperserOrder:  Option[GmosDisperserOrder],
      wavelength:      Option[BigDecimal],
      filter:          Option[GmosSouthFilter],
      mdfFileName:     Option[String],
      customSlitWidth: Option[GmosCustomSlitWidth],
      builtin:         Option[GmosSouthFpu]
    ) {
      import Gmos._

      def toDynamicConfig(common: GmosCommonDynamicConfig): GmosSouthDynamicConfig = {
        val grating: Option[GmosSouthGrating] =
          for {
            d <- disperser
            o <- disperserOrder
            w <- wavelength
          } yield GmosSouthGrating(d, o, GmosCentralWavelength(w.toDouble))

        val customMask: Option[GmosCustomMask] =
          for {
            m <- mdfFileName
            w <- customSlitWidth
          } yield GmosCustomMask(m, w)

        val fpu = customMask.map(_.left[GmosSouthFpu]) orElse
                  builtin.map(_.right[GmosCustomMask])

        GmosSouthDynamicConfig(common, grating, filter, fpu)
      }
    }

    def allGmosSouthOnly(oid: Observation.Id): Query0[(Loc, GmosSouthDynamicConfig)] =
      sql"""
        SELECT s.location,
               i.disperser,
               i.disperser_order,
               i.wavelength,
               i.filter,
               i.mdf_file_name,
               i.custom_slit_width,
               i.fpu,
               c.x_binning,
               c.y_binning,
               c.amp_count,
               c.amp_gain,
               c.amp_read_mode,
               c.dtax,
               c.exposure_time
          FROM step s
               LEFT OUTER JOIN step_gmos_common c
                 ON c.step_id = s.step_id
               LEFT OUTER JOIN step_gmos_south i
                 ON i.step_id = s.step_id
         WHERE s.observation_id = $oid
      """.query[(Loc, GmosSouthIntermediary, Gmos.GmosCommonDynamicConfig)].map {
        case (l, i, c) => (l, i.toDynamicConfig(c))
      }


    def oneGmosSouthOnly(oid: Observation.Id, loc: Loc): Query0[GmosSouthDynamicConfig] =
      sql"""
        SELECT i.disperser,
               i.disperser_order,
               i.wavelength,
               i.filter,
               i.mdf_file_name,
               i.custom_slit_width,
               i.fpu,
               c.x_binning,
               c.y_binning,
               c.amp_count,
               c.amp_gain,
               c.amp_read_mode,
               c.dtax,
               c.exposure_time
          FROM step s
               LEFT OUTER JOIN step_gmos_common c
                 ON c.step_id = s.step_id
               LEFT OUTER JOIN step_gmos_south i
                 ON i.step_id = s.step_id
         WHERE s.observation_id = $oid AND s.location = $loc
      """.query[(GmosSouthIntermediary, Gmos.GmosCommonDynamicConfig)].map {
        case (i, c) => i.toDynamicConfig(c)
      }

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

    def insertF2Config(id: Int, f2: F2DynamicConfig): Update0 =
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

    def insertGmosNorthConfig(id: Int, g: GmosNorthDynamicConfig): Update0 =
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

    def insertGmosSouthConfig(id: Int, g: GmosSouthDynamicConfig): Update0 =
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
