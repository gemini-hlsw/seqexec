// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.dao

import cats.implicits._
import doobie._, doobie.implicits._
import gem._
import gem.SmartGcal._
import gem.config._
import gem.dao.composite._
import gem.dao.meta._
import gem.enum._
import gem.util.Location
import gsp.math.Wavelength
import fs2.Stream
import scala.collection.immutable.TreeMap

object SmartGcalDao {
  import EnumeratedMeta._
  import WavelengthMeta._
  import EitherComposite._

  def select(k: SmartGcalSearchKey, t: SmartGcalType): ConnectionIO[List[GcalConfig]] =
    for {
      ids <- k match {
               case f2:    SmartGcalKey.Flamingos2      => selectF2(f2, t)
               case gn:    SmartGcalKey.GmosNorthSearch => selectGmosNorth(gn, t)
               case gs:    SmartGcalKey.GmosSouthSearch => selectGmosSouth(gs, t)
               case gnirs: SmartGcalKey.GnirsSearch     => selectGnirs(gnirs, t)
             }
      gcs <- ids.traverse { GcalDao.selectSmartGcal }.map(_.collect { case Some(a) => a })
    } yield gcs

  def selectF2(k: SmartGcalKey.Flamingos2, t: SmartGcalType): ConnectionIO[List[Int]] =
    t.fold(Statements.Flamingos2.selectByLamp(k), Statements.Flamingos2.selectByBaseline(k)).to[List]

  def selectGmosNorth(k: SmartGcalKey.GmosNorthSearch, t: SmartGcalType): ConnectionIO[List[Int]] =
    t.fold(Statements.GmosNorth.selectByLamp(k), Statements.GmosNorth.selectByBaseline(k)).to[List]

  def selectGmosSouth(k: SmartGcalKey.GmosSouthSearch, t: SmartGcalType): ConnectionIO[List[Int]] =
    t.fold(Statements.GmosSouth.selectByLamp(k), Statements.GmosSouth.selectByBaseline(k)).to[List]

  def selectGnirs(k: SmartGcalKey.GnirsSearch, t: SmartGcalType): ConnectionIO[List[Int]] =
    t.fold(Statements.Gnirs.selectByLamp(k), Statements.Gnirs.selectByBaseline(k)).to[List]

  val createIndexF2: ConnectionIO[Int] =
    Statements.Flamingos2.createIndex.run

  val dropIndexF2: ConnectionIO[Int] =
    Statements.Flamingos2.dropIndex.run

  val createIndexGmosNorth: ConnectionIO[Int] =
    Statements.GmosNorth.createIndex.run

  val dropIndexGmosNorth: ConnectionIO[Int] =
    Statements.GmosNorth.dropIndex.run

  val createIndexGmosSouth: ConnectionIO[Int] =
    Statements.GmosSouth.createIndex.run

  val dropIndexGmosSouth: ConnectionIO[Int] =
    Statements.GmosSouth.dropIndex.run

  val createIndexGnirs: ConnectionIO[Int] =
    Statements.Gnirs.createIndex.run

  val dropIndexGnirs: ConnectionIO[Int] =
    Statements.Gnirs.dropIndex.run

  def bulkInsertF2(entries: Vector[(GcalLampType, GcalBaselineType, SmartGcalKey.Flamingos2, GcalConfig)]): Stream[ConnectionIO, Int] =
    bulkInsert(Statements.Flamingos2.bulkInsert, entries)

  def bulkInsertGmosNorth(entries: Vector[(GcalLampType, GcalBaselineType, SmartGcalKey.GmosNorthDefinition, GcalConfig)]): Stream[ConnectionIO, Int] =
    bulkInsert[SmartGcalKey.GmosNorthDefinition](Statements.GmosNorth.bulkInsert, entries)

  def bulkInsertGmosSouth(entries: Vector[(GcalLampType, GcalBaselineType, SmartGcalKey.GmosSouthDefinition, GcalConfig)]): Stream[ConnectionIO, Int] =
    bulkInsert[SmartGcalKey.GmosSouthDefinition](Statements.GmosSouth.bulkInsert, entries)

  def bulkInsertGnirs(entries: Vector[(GcalLampType, GcalBaselineType, SmartGcalKey.GnirsDefinition, GcalConfig)]): Stream[ConnectionIO, Int] =
    bulkInsert[SmartGcalKey.GnirsDefinition](Statements.Gnirs.bulkInsert, entries)

  private def bulkInsert[K](
                update:  Update[((GcalLampType, GcalBaselineType, K), Int)],
                entries: Vector[(GcalLampType, GcalBaselineType, K, GcalConfig)]): Stream[ConnectionIO, Int] = {

    // Workaround for issue https://github.com/functional-streams-for-scala/fs2/issues/1099
    val tups = entries.map(t => (t._1, t._2, t._3))
    GcalDao.bulkInsertSmartGcal(entries.map(_._4))
      .chunkN(tups.size)
      .flatMap { rows =>
        Stream.eval(update.updateMany(tups.zip(rows.toVector)))
      }

    /* https://github.com/functional-streams-for-scala/fs2/issues/1099
      Stream.emits(entries.map(t => (t._1, t._2, t._3)))          // Stream[Pure, (Lamp, Baseline, Key)]
        .zip(GcalDao.bulkInsertSmartGcal(entries.map(_._4)))      // Stream[ConnectionIO, ((Lamp, Baseline, Key), Id)]
        .segmentN(4096)                                           // Stream[ConnectionIO, Segment[((Lamp, Baseline, Key), Id)]]
        .flatMap { rows => Stream.eval(update.updateMany(rows.force.toVector)) } // Stream[ConnectionIO, Int]
    */
  }

  type ExpansionResult[A] = EitherConnectionIO[ExpansionError, A]

  private def lookupʹ(step: MaybeConnectionIO[Step], loc: Location.Middle, static: StaticConfig): ExpansionResult[ExpandedSteps] = {

    // Information we need to extract from a smart gcal step in order to expand
    // it into manual gcal steps.  The key is used to look up the gcal config
    // from the instrument's smart table (e.g., smart_f2).  The type is used to
    // extract the matching steps (arc vs. flat, or night vs. day baseline).
    // The instrument config is needed to create the corresponding gcal steps.
    type SmartContext = (SmartGcalSearchKey, SmartGcalType, DynamicConfig)

    // Get the key, type, and instrument config from the step.  We'll need this
    // information to lookup the corresponding GcalConfig.
    val stepToContext: (Step) => ExpansionResult[SmartContext] = { step =>
      (step.dynamicConfig, step.base) match {
        case (d, Step.Base.SmartGcal(t)) =>
          EitherConnectionIO.fromDisjunction {
            (d.smartGcalKey(static).toRight(noMappingDefined)).map { k => (k, t, d) }
          }
        case _                    =>
          EitherConnectionIO.pointLeft(notSmartGcal)
      }
    }

    def expand(k: SmartGcalSearchKey, t: SmartGcalType, d: DynamicConfig): ExpansionResult[ExpandedSteps] =
      EitherConnectionIO(select(k, t).map {
        case Nil => Left(noMappingDefined)
        case cs  => Right(cs.map(c => d.toStep(Step.Base.Gcal(c))))
      })

    // Find the corresponding smart gcal mapping, if any.
    for {
      step <- step.toRight(stepNotFound(loc))
      ktd  <- stepToContext(step)
      (k, t, d) = ktd
      gcal <- expand(k, t, d)
    } yield gcal
  }

  /** Lookup the corresponding `GcalStep`s for the smart step at `loc`, leaving
    * the sequence unchanged.
    *
    * @param oid observation whose smart gcal expansion is desired for the step
    *            at `loc`
    * @param loc position of the smart gcal step to expand
    *
    * @return expansion of the smart gcal step into `GcalStep`s if `loc` is
    *         found, refers to a smart gcal step, and a mapping is defined for
    *         its instrument configuration
    */
  def lookup(oid: Observation.Id, loc: Location.Middle): ExpansionResult[ExpandedSteps] =
    for {
      o <- ObservationDao.fetchStatic(oid).injectRight
      s <- lookupʹ(StepDao.selectOne(oid, loc), loc, o._2)
    } yield s

  /** Expands a smart gcal step into the corresponding gcal steps so that they
    * may be executed. Updates the sequence to replace a smart gcal step with
    * one or more manual gcal steps.
    *
    * @param oid observation whose smart gcal expansion is desired for the step
    * @param loc position of the smart gcal step to expand
    *
    * @return expansion of the smart gcal step into `GcalConfig` if `loc` is
    *         found, refers to a smart gcal step, and a mapping is defined for
    *         its instrument configuration
    */
  def expand(oid: Observation.Id, loc: Location.Middle): ExpansionResult[ExpandedSteps] = {
    // Find the previous and next location for the smart gcal step that we are
    // replacing.  This is needed to generate locations for the steps that will
    // be inserted.
    def bounds(steps: TreeMap[Location.Middle, Step]): (Location, Location) =
      steps.span { case (k, _) => k < loc } match {
        case (prev, next) => (prev.lastOption.map(_._1).widen[Location] getOrElse Location.beginning,
                              next.headOption.map(_._1).widen[Location] getOrElse Location.end)
      }

    // Inserts manual gcal steps between `before` and `after` locations.
    def insert(before: Location, gcal: ExpandedSteps, after: Location): ConnectionIO[Unit] =
      Location.find(gcal.size, before, after).toList.zip(gcal).traverse { case (l, s) =>
        StepDao.insert(oid, l, s)
      }.void

    for {
      obs   <- ObservationDao.fetchStatic(oid).injectRight
      steps <- StepDao.selectAll(oid).injectRight
      (locBefore, locAfter) = bounds(steps)
      gcal  <- lookupʹ(MaybeConnectionIO.fromOption(steps.get(loc)), loc, obs._2)
      // replaces the smart gcal step with the expanded manual gcal steps
      _     <- StepDao.deleteAtLocation(oid, loc).injectRight
      _     <- insert(locBefore, gcal, locAfter).injectRight
    } yield gcal
  }

  object Statements {


    object Flamingos2 {

      val createIndex: Update0 =
        sql"""
        CREATE INDEX IF NOT EXISTS smart_f2_index ON smart_f2
          (disperser, filter, fpu)
      """.update

      val dropIndex: Update0 =
        sql"""
        DROP INDEX IF EXISTS smart_f2_index
      """.update

      def selectByLamp(k: SmartGcalKey.Flamingos2)(l: GcalLampType): Query0[Int] =
        sql"""
          SELECT gcal_id
            FROM smart_f2
           WHERE lamp      = $l :: gcal_lamp_type
             AND disperser = ${k.disperser}
             AND filter    = ${k.filter}
             AND fpu       = ${k.fpu}
        """.query[Int]

      def selectByBaseline(k: SmartGcalKey.Flamingos2)(b: GcalBaselineType): Query0[Int] =
        sql"""
          SELECT gcal_id
            FROM smart_f2
           WHERE baseline  = $b :: gcal_baseline_type
             AND disperser = ${k.disperser}
             AND filter    = ${k.filter}
             AND fpu       = ${k.fpu}
        """.query[Int]

      type Row = ((GcalLampType, GcalBaselineType, SmartGcalKey.Flamingos2), Int)

      val bulkInsert: Update[Row] = {
        val sql =
          """
          INSERT INTO smart_f2 (lamp,
                                baseline,
                                disperser,
                                filter,
                                fpu,
                                gcal_id)
               VALUES (? :: gcal_lamp_type, ? :: gcal_baseline_type, ?, ?, ?, ?)
        """
        Update[Row](sql)
      }

    }

    sealed trait Gmos {
      implicit val WavelengthMeta = WavelengthMetaAsNanometers

      def select[D, F, U](table: String, searchType: Fragment, dfu: Fragment, k: SmartGcalKey.GmosCommon[D, F, U], w: Option[Wavelength]): Fragment =
        Fragment.const(
          s"""SELECT gcal_id
               FROM $table
              WHERE """) ++ searchType ++ dfu ++
          fr"""
                AND x_binning       = ${k.xBinning}
                AND y_binning       = ${k.yBinning}
                AND amp_gain        = ${k.ampGain}
                AND min_wavelength <= ${w.getOrElse(Wavelength.Max)}
                AND max_wavelength >  ${w.getOrElse(Wavelength.Min)}
         """

      def lampFragment(l: GcalLampType): Fragment =
        fr"""lamp = $l :: gcal_lamp_type"""

      def baselineFragment(b: GcalBaselineType): Fragment =
        fr"""baseline = $b :: gcal_baseline_type"""

      def bulkInsertSql(table: String): String =
        s"""
        INSERT INTO $table (lamp,
                            baseline,
                            disperser,
                            filter,
                            fpu,
                            x_binning,
                            y_binning,
                            amp_gain,
                            min_wavelength,
                            max_wavelength,
                            gcal_id)
             VALUES (? :: gcal_lamp_type, ? :: gcal_baseline_type, ?, ?, ?, ?, ?, ?, ?, ?, ?)
      """

    }

    object GmosNorth extends Gmos {

      val createIndex: Update0 =
        sql"""
        CREATE INDEX IF NOT EXISTS smart_gmos_north_index ON smart_gmos_north
          (disperser, filter, fpu)
      """.update

      val dropIndex: Update0 =
        sql"""
        DROP INDEX IF EXISTS smart_gmos_north_index
      """.update

      def fragment(k: SmartGcalKey.GmosNorthCommon): Fragment =
        fr"""
              AND disperser       = ${k.disperser}
              AND filter          = ${k.filter}
              AND fpu             = ${k.fpu}
        """

      def selectByLamp(k: SmartGcalKey.GmosNorthSearch)(l: GcalLampType): Query0[Int] =
        select("smart_gmos_north", lampFragment(l), fragment(k.gmos), k.gmos, k.wavelength).query[Int]

      def selectByBaseline(k: SmartGcalKey.GmosNorthSearch)(b: GcalBaselineType): Query0[Int] =
        select("smart_gmos_north", baselineFragment(b), fragment(k.gmos), k.gmos, k.wavelength).query[Int]

      type Row = ((GcalLampType, GcalBaselineType, SmartGcalKey.GmosNorthDefinition), Int)

      val bulkInsert: Update[Row] =
        Update[Row](bulkInsertSql("smart_gmos_north"))

    }

    object GmosSouth extends Gmos {

      val createIndex: Update0 =
        sql"""
        CREATE INDEX IF NOT EXISTS smart_gmos_south_index ON smart_gmos_south
          (disperser, filter, fpu)
      """.update

      val dropIndex: Update0 =
        sql"""
        DROP INDEX IF EXISTS smart_gmos_south_index
      """.update

      def fragment(k: SmartGcalKey.GmosSouthCommon): Fragment =
        fr"""
              AND disperser       = ${k.disperser}
              AND filter          = ${k.filter}
              AND fpu             = ${k.fpu}
        """

      def selectByLamp(k: SmartGcalKey.GmosSouthSearch)(l: GcalLampType): Query0[Int] =
        select("smart_gmos_south", lampFragment(l), fragment(k.gmos), k.gmos, k.wavelength).query[Int]

      def selectByBaseline(k: SmartGcalKey.GmosSouthSearch)(b: GcalBaselineType): Query0[Int] =
        select("smart_gmos_south", baselineFragment(b), fragment(k.gmos), k.gmos, k.wavelength).query[Int]

      type Row = ((GcalLampType, GcalBaselineType, SmartGcalKey.GmosSouthDefinition), Int)

      val bulkInsert: Update[Row] =
        Update[Row](bulkInsertSql("smart_gmos_south"))
    }

    object Gnirs {

      implicit val WavelengthMeta = WavelengthMetaAsMicrometers

      val createIndex: Update0 =
        sql"""
        CREATE INDEX IF NOT EXISTS smart_gnirs_index ON smart_gnirs (disperser, prism)
      """.update

      val dropIndex: Update0 =
        sql"""
        DROP INDEX IF EXISTS smart_gnirs_index
      """.update

      def selectByLamp(k: SmartGcalKey.GnirsSearch)(l: GcalLampType): Query0[Int] =
        sql"""
          SELECT gcal_id
            FROM smart_gnirs
           WHERE lamp       = $l :: gcal_lamp_type
             AND acquisition_mirror = ${k.gnirs.acquisitionMirror}
             AND pixel_scale        = ${k.gnirs.pixelScale}
             AND disperser          = ${k.gnirs.disperser}
             AND fpu_slit           = ${k.gnirs.fpu.right.toOption}
             AND fpu_other          = ${k.gnirs.fpu.left.toOption}
             AND prism              = ${k.gnirs.prism}
             AND well_depth         = ${k.gnirs.wellDepth}
             AND min_wavelength    <= ${k.wavelength}
             AND max_wavelength     > ${k.wavelength}
        """.query[Int]

      def selectByBaseline(k: SmartGcalKey.GnirsSearch)(b: GcalBaselineType): Query0[Int] =
        sql"""
          SELECT gcal_id
            FROM smart_gnirs
           WHERE baseline   = $b :: gcal_baseline_type
             AND acquisition_mirror = ${k.gnirs.acquisitionMirror}
             AND pixel_scale        = ${k.gnirs.pixelScale}
             AND disperser          = ${k.gnirs.disperser}
             AND fpu_slit           = ${k.gnirs.fpu.right.toOption}
             AND fpu_other          = ${k.gnirs.fpu.left.toOption}
             AND prism              = ${k.gnirs.prism}
             AND well_depth         = ${k.gnirs.wellDepth}
             AND min_wavelength    <= ${k.wavelength}
             AND max_wavelength     > ${k.wavelength}
        """.query[Int]

      type Row = ((GcalLampType, GcalBaselineType, SmartGcalKey.GnirsDefinition), Int)

      val bulkInsert: Update[Row] = {
        val sql =
          """
          INSERT INTO smart_gnirs (lamp,
                                   baseline,
                                   acquisition_mirror,
                                   pixel_scale,
                                   disperser,
                                   fpu_other,
                                   fpu_slit,
                                   prism,
                                   well_depth,
                                   min_wavelength,
                                   max_wavelength,
                                   gcal_id)
               VALUES (? :: gcal_lamp_type, ? :: gcal_baseline_type, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
        """
        Update[Row](sql)
      }
    }

  }

}
