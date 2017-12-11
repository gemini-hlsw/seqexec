// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.dao

import cats.implicits._
import doobie._, doobie.implicits._
import gem._
import gem.SmartGcal._
import gem.config._
import gem.config.DynamicConfig.{ SmartGcalKey, SmartGcalSearchKey }
import gem.dao.meta._
import gem.enum._
import gem.math.Wavelength
import gem.util.Location
import fs2.Stream
import scala.collection.immutable.TreeMap

object SmartGcalDao {
  import EnumeratedMeta._
  import WavelengthMeta._

  def select(k: SmartGcalSearchKey, t: SmartGcalType): ConnectionIO[List[GcalConfig]] =
    for {
      ids <- k match {
               case f2: SmartGcalKey.F2              => selectF2(f2, t)
               case gn: SmartGcalKey.GmosNorthSearch => selectGmosNorth(gn, t)
               case gs: SmartGcalKey.GmosSouthSearch => selectGmosSouth(gs, t)
             }
      gcs <- ids.traverse { GcalDao.selectSmartGcal }.map(_.collect { case Some(a) => a })
    } yield gcs

  def selectF2(k: SmartGcalKey.F2, t: SmartGcalType): ConnectionIO[List[Int]] =
    t.fold(Statements.selectF2ByLamp(k), Statements.selectF2ByBaseline(k)).list

  def selectGmosNorth(k: SmartGcalKey.GmosNorthSearch, t: SmartGcalType): ConnectionIO[List[Int]] =
    t.fold(Statements.selectGmosNorthByLamp(k), Statements.selectGmosNorthByBaseline(k)).list

  def selectGmosSouth(k: SmartGcalKey.GmosSouthSearch, t: SmartGcalType): ConnectionIO[List[Int]] =
    t.fold(Statements.selectGmosSouthByLamp(k), Statements.selectGmosSouthByBaseline(k)).list

  val createIndexF2: ConnectionIO[Int] =
    Statements.createIndexF2.run

  val dropIndexF2: ConnectionIO[Int] =
    Statements.dropIndexF2.run

  val createIndexGmosNorth: ConnectionIO[Int] =
    Statements.createIndexGmosNorth.run

  val dropIndexGmosNorth: ConnectionIO[Int] =
    Statements.dropIndexGmosNorth.run

  val createIndexGmosSouth: ConnectionIO[Int] =
    Statements.createIndexGmosSouth.run

  val dropIndexGmosSouth: ConnectionIO[Int] =
    Statements.dropIndexGmosSouth.run

  def bulkInsertF2(entries: Vector[(GcalLampType, GcalBaselineType, SmartGcalKey.F2, GcalConfig)]): Stream[ConnectionIO, Int] =
    bulkInsert(Statements.bulkInsertF2, entries)

  def bulkInsertGmosNorth(entries: Vector[(GcalLampType, GcalBaselineType, SmartGcalKey.GmosNorthDefinition, GcalConfig)]): Stream[ConnectionIO, Int] =
    bulkInsert[SmartGcalKey.GmosNorthDefinition](Statements.bulkInsertGmosNorth, entries)

  def bulkInsertGmosSouth(entries: Vector[(GcalLampType, GcalBaselineType, SmartGcalKey.GmosSouthDefinition, GcalConfig)]): Stream[ConnectionIO, Int] =
    bulkInsert[SmartGcalKey.GmosSouthDefinition](Statements.bulkInsertGmosSouth, entries)

  private def bulkInsert[K](
                update:  Update[((GcalLampType, GcalBaselineType, K), Int)],
                entries: Vector[(GcalLampType, GcalBaselineType, K, GcalConfig)]): Stream[ConnectionIO, Int] =

    Stream.emits(entries.map(t => (t._1, t._2, t._3)))          // Stream[Pure, (Lamp, Baseline, Key)]
      .zip(GcalDao.bulkInsertSmartGcal(entries.map(_._4)))      // Stream[ConnectionIO, ((Lamp, Baseline, Key), Id)]
      .segmentN(4096)                                           // Stream[ConnectionIO, Segment[((Lamp, Baseline, Key), Id)]]
      .flatMap { rows => Stream.eval(update.updateMany(rows.force.toVector)) } // Stream[ConnectionIO, Int]


  type ExpansionResult[A] = EitherConnectionIO[ExpansionError, A]

  private def lookupʹ(step: MaybeConnectionIO[Step[DynamicConfig]], loc: Location.Middle): ExpansionResult[ExpandedSteps] = {
    // Information we need to extract from a smart gcal step in order to expand
    // it into manual gcal steps.  The key is used to look up the gcal config
    // from the instrument's smart table (e.g., smart_f2).  The type is used to
    // extract the matching steps (arc vs. flat, or night vs. day baseline).
    // The instrument config is needed to create the corresponding gcal steps.
    type SmartContext = (SmartGcalSearchKey, SmartGcalType, DynamicConfig)

    // Get the key, type, and instrument config from the step.  We'll need this
    // information to lookup the corresponding GcalConfig.
    val context: ExpansionResult[SmartContext] =
      for {
        s <- step.toRight(stepNotFound(loc))
        c <- s match {
               case Step.SmartGcal(i, t) =>
                 EitherConnectionIO.fromDisjunction {
                   (i.smartGcalKey.toRight(noMappingDefined)).map { k => (k, t, i) }
                 }
               case _                    =>
                 EitherConnectionIO.pointLeft(notSmartGcal)
             }
      } yield c


    // Find the corresponding smart gcal mapping, if any.
    for {
      kti  <- context
      (k, t, i) = kti
      gcal <- EitherConnectionIO(select(k, t).map {
                case Nil => Left(noMappingDefined)
                case cs  => Right(cs.map(Step.Gcal(i, _)))
              })
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
    lookupʹ(StepDao.selectOne(oid, loc), loc)

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
    def bounds(steps: TreeMap[Location.Middle, Step[DynamicConfig]]): (Location, Location) =
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
      steps <- StepDao.selectAll(oid).injectRight
      (locBefore, locAfter) = bounds(steps)
      gcal  <- lookupʹ(MaybeConnectionIO.fromOption(steps.get(loc)), loc)
      // replaces the smart gcal step with the expanded manual gcal steps
      _     <- StepDao.deleteAtLocation(oid, loc).injectRight
      _     <- insert(locBefore, gcal, locAfter).injectRight
    } yield gcal
  }

  object Statements {

    // -----------------------------------------------------------------------
    // Flamingos2
    // -----------------------------------------------------------------------

    val createIndexF2: Update0 =
      sql"""
        CREATE INDEX IF NOT EXISTS smart_f2_index ON smart_f2
          (disperser, filter, fpu)
      """.update

    val dropIndexF2: Update0 =
      sql"""
        DROP INDEX IF EXISTS smart_f2_index
      """.update

    def selectF2ByLamp(k: SmartGcalKey.F2)(l: GcalLampType): Query0[Int] =
      sql"""
          SELECT gcal_id
            FROM smart_f2
           WHERE lamp      = $l :: gcal_lamp_type
             AND disperser = ${k.disperser}
             AND filter    = ${k.filter}
             AND fpu       = ${k.fpu}
        """.query[Int]

    def selectF2ByBaseline(k: SmartGcalKey.F2)(b: GcalBaselineType): Query0[Int] =
      sql"""
          SELECT gcal_id
            FROM smart_f2
           WHERE baseline  = $b :: gcal_baseline_type
             AND disperser = ${k.disperser}
             AND filter    = ${k.filter}
             AND fpu       = ${k.fpu}
        """.query[Int]

    type F2Row = ((GcalLampType, GcalBaselineType, SmartGcalKey.F2), Int)

    val bulkInsertF2: Update[F2Row] = {
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
      Update[F2Row](sql)
    }


    // -----------------------------------------------------------------------
    // GmosNorth
    // -----------------------------------------------------------------------

    val createIndexGmosNorth: Update0 =
      sql"""
        CREATE INDEX IF NOT EXISTS smart_gmos_north_index ON smart_gmos_north
          (disperser, filter, fpu)
      """.update

    val dropIndexGmosNorth: Update0 =
      sql"""
        DROP INDEX IF EXISTS smart_gmos_north_index
      """.update

    val createIndexGmosSouth: Update0 =
      sql"""
        CREATE INDEX IF NOT EXISTS smart_gmos_south_index ON smart_gmos_south
          (disperser, filter, fpu)
      """.update

    val dropIndexGmosSouth: Update0 =
      sql"""
        DROP INDEX IF EXISTS smart_gmos_south_index
      """.update

    private val MinWavelength = 0
    private val MaxWavelength = Int.MaxValue

    private def selectGmos[D, F, U](table: String, searchType: Fragment, dfu: Fragment, k: SmartGcalKey.GmosCommon[D,F,U], w: Option[Wavelength]): Fragment =
      Fragment.const(
        s"""SELECT gcal_id
               FROM $table
              WHERE """) ++ searchType ++ dfu ++
        fr"""
                AND x_binning       = ${k.xBinning}
                AND y_binning       = ${k.yBinning}
                AND amp_gain        = ${k.ampGain}
                AND min_wavelength <= ${w.map(_.toAngstroms).getOrElse(MaxWavelength)}
                AND max_wavelength >  ${w.map(_.toAngstroms).getOrElse(MinWavelength)}
         """

    def lampFragment(l: GcalLampType): Fragment =
      fr"""lamp = $l :: gcal_lamp_type"""

    def baselineFragment(b: GcalBaselineType): Fragment =
      fr"""baseline = $b :: gcal_baseline_type"""

    def gmosNorthFragment(k: SmartGcalKey.GmosNorthCommon): Fragment =
      fr"""
              AND disperser       = ${k.disperser}
              AND filter          = ${k.filter}
              AND fpu             = ${k.fpu}
        """

    def gmosSouthFragment(k: SmartGcalKey.GmosSouthCommon): Fragment =
      fr"""
              AND disperser       = ${k.disperser}
              AND filter          = ${k.filter}
              AND fpu             = ${k.fpu}
        """


    def selectGmosNorthByLamp(k: SmartGcalKey.GmosNorthSearch)(l: GcalLampType): Query0[Int] =
      selectGmos("smart_gmos_north", lampFragment(l), gmosNorthFragment(k.gmos), k.gmos, k.wavelength).query[Int]

    def selectGmosSouthByLamp(k: SmartGcalKey.GmosSouthSearch)(l: GcalLampType): Query0[Int] =
      selectGmos("smart_gmos_south", lampFragment(l), gmosSouthFragment(k.gmos), k.gmos, k.wavelength).query[Int]

    def selectGmosNorthByBaseline(k: SmartGcalKey.GmosNorthSearch)(b: GcalBaselineType): Query0[Int] =
      selectGmos("smart_gmos_north", baselineFragment(b), gmosNorthFragment(k.gmos), k.gmos, k.wavelength).query[Int]

    def selectGmosSouthByBaseline(k: SmartGcalKey.GmosSouthSearch)(b: GcalBaselineType): Query0[Int] =
      selectGmos("smart_gmos_south", baselineFragment(b), gmosSouthFragment(k.gmos), k.gmos, k.wavelength).query[Int]

    def bulkInsertGmosSql(table: String): String =
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

    type GmosNorthRow = ((GcalLampType, GcalBaselineType, SmartGcalKey.GmosNorthDefinition), Int)
    type GmosSouthRow = ((GcalLampType, GcalBaselineType, SmartGcalKey.GmosSouthDefinition), Int)

    val bulkInsertGmosNorth: Update[GmosNorthRow] =
      Update[GmosNorthRow](bulkInsertGmosSql("smart_gmos_north"))

    val bulkInsertGmosSouth: Update[GmosSouthRow] =
      Update[GmosSouthRow](bulkInsertGmosSql("smart_gmos_south"))
  }
}
