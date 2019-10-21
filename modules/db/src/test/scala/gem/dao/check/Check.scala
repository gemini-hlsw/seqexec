// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.dao
package check

import cats.effect.{ IO, ContextShift }
import doobie.Transactor
import doobie.scalatest.imports._
import gem._
import gem.enum._
import gem.config._
import gem.util.{ Timestamp, Location }
import gsp.math._
import java.time.LocalDate
import org.scalatest.matchers.should.Matchers
import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import scala.collection.immutable.{ TreeMap, TreeSet }

/** Trait for tests that check statement syntax and mappings. */
trait Check extends AnyFlatSpec with Matchers with IOChecker {

  private implicit val contextShift: ContextShift[IO] =
    IO.contextShift(scala.concurrent.ExecutionContext.global)

  val transactor: Transactor[IO] =
    DatabaseConfiguration.forTesting.transactor[IO]

  /**
   * Some dummy values to pass to statement constructors. The actual values don't matter because
   * the statements are never executed, but they are dereferenced and cannot be null.
   */
  object Dummy {
    val instant          = java.time.Instant.EPOCH
    val duration         = java.time.Duration.ZERO
    val programId        = Program.Id.fromString.unsafeGet("GS-foobar") match { case n: Program.Id.Nonstandard => n ; case _ => sys.error("unpossbile") }
    val semester         = Semester.unsafeFromString("2015B")
    val site             = Site.GN
    val programType      = ProgramType.C
    val dailyProgramId   = Program.Id.Daily(site, DailyProgramType.ENG, LocalDate.now())
    val scienceProgramId = Program.Id.Science(site, semester, programType, Index.One)
    val observationId    = Observation.Id(programId, Index.One)
    val datasetLabel     = Dataset.Label(observationId, 0)
    val dataset          = Dataset(datasetLabel, "", instant)
    val eventType        = EventType.Abort
    val gcalLamp         = Left(GcalContinuum.IrGreyBodyLow)
    val gcalFilter       = GcalFilter.None
    val gcalDiffuser     = GcalDiffuser.Ir
    val gcalShutter      = GcalShutter.Open
    val gcalConfig       = GcalConfig(gcalLamp, gcalFilter, gcalDiffuser, gcalShutter, duration, CoAdds.One)
    val user             = User[Nothing]("", "", "", "", false, Map.empty)
    val observation      = Observation.Flamingos2("", TargetEnvironment.Flamingos2(None, TreeSet.empty), StaticConfig.Flamingos2.Default, Nil)
    val program          = Program(programId, "", TreeMap.empty)
    val f2SmartGcalKey   = DynamicConfig.Flamingos2.Default.key
    val gcalLampType     = GcalLampType.Arc
    val gcalBaselineType = GcalBaselineType.Day
    val locationMiddle   = Location.unsafeMiddle(1)
    val f2Config         = DynamicConfig.Flamingos2.Default
    val telescopeConfig  = TelescopeConfig(Offset.P.Zero, Offset.Q.Zero)
    val smartGcalType    = SmartGcalType.Arc
    val instrumentConfig = f2Config
    val stepType         = StepType.Science
    val ephemerisKey     = EphemerisKey.Comet("Lanrezac")
    val horizonsSolnRef  = HorizonsSolutionRef("JPL#K162/5")
    val ephemerisMeta    = EphemerisMeta(Timestamp.Min, Timestamp.Min, Some(horizonsSolnRef))

    val gmosCustomRoiEntry =
      gem.config.GmosConfig.GmosCustomRoiEntry.unsafeFromDescription(1, 1, 1, 1)

    val gmosNorthSmartGcalSearchKey     =
      DynamicConfig.GmosN.Default.key

    val gmosSouthSmartGcalSearchKey     =
      DynamicConfig.GmosS.Default.key

    val gmosNorthSmartGcalDefinitionKey = {
      val sk = DynamicConfig.GmosN.Default.key
      SmartGcalKey.GmosDefinition(
        sk.gmos,
        (Wavelength.Min, Wavelength.Min)
      )
    }

    val gmosSouthSmartGcalDefinitionKey = {
      val sk = DynamicConfig.GmosS.Default.key
      SmartGcalKey.GmosDefinition(
        sk.gmos,
        (Wavelength.Min, Wavelength.Min)
      )
    }

    val target: Target =
      Target("untitled", Right(ProperMotion.const(Coordinates.Zero)))

    val gnirsConfig = DynamicConfig.Gnirs.Default

    val gnirsSmartGcalKey = DynamicConfig.Gnirs.Default.key(StaticConfig.Gnirs.Default)

  }

}
