// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.dao
package check

import cats.effect.IO
import doobie._
import doobie.scalatest.imports._
import gem._
import gem.enum._
import gem.config._
import gem.config.DynamicConfig.SmartGcalKey
import gem.util.Location
import gem.math._
import java.time.LocalDate
import org.scalatest._


/** Trait for tests that check statement syntax and mappings. */
trait Check extends FlatSpec with Matchers with IOChecker {

  def transactor = Transactor.fromDriverManager[IO](
    "org.postgresql.Driver",
    "jdbc:postgresql:gem",
    "postgres",
    ""
  )

  /**
   * Some dummy values to pass to statement constructors. The actual values don't matter because
   * the statements are never executed, but they are dereferenced and cannot be null.
   */
  @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
  object Dummy {
    val instant          = java.time.Instant.EPOCH
    val duration         = java.time.Duration.ZERO
    val programId        = Program.Id.unsafeFromString("GS-foobar") match { case n: Program.Id.Nonstandard => n ; case _ => sys.error("unpossbile") }
    val semester         = Semester.unsafeFromString("2015B")
    val site             = Site.GN
    val programType      = ProgramType.C
    val dailyProgramId   = Program.Id.Daily(site, DailyProgramType.ENG, LocalDate.now())
    val scienceProgramId = Program.Id.Science(site, semester, programType, 0)
    val observationId    = Observation.Id(programId, 0)
    val datasetLabel     = Dataset.Label(observationId, 0)
    val dataset          = Dataset(datasetLabel, "", instant)
    val eventType        = EventType.Abort
    val gcalLamp         = Left(GcalContinuum.IrGreyBodyLow)
    val gcalFilter       = GcalFilter.None
    val gcalDiffuser     = GcalDiffuser.Ir
    val gcalShutter      = GcalShutter.Open
    val gcalConfig       = GcalConfig(gcalLamp, gcalFilter, gcalDiffuser, gcalShutter, duration, 0)
    val user             = User[Nothing]("", "", "", "", false, Map.empty)
    val observation      = Observation[StaticConfig, Nothing](observationId, "", StaticConfig.F2.Default, Nil)
    val program          = Program(programId, "", Nil)
    val f2SmartGcalKey   = DynamicConfig.F2.Default.key
    val gcalLampType     = GcalLampType.Arc
    val gcalBaselineType = GcalBaselineType.Day
    val locationMiddle   = Location.unsafeMiddle(1)
    val f2Config         = DynamicConfig.F2.Default
    val telescopeConfig  = TelescopeConfig(Offset.P.Zero, Offset.Q.Zero)
    val smartGcalType    = SmartGcalType.Arc
    val instrumentConfig = f2Config
    val stepType         = StepType.Science
    val ephemerisKey     = EphemerisKey.Comet("Lanrezac")

    val gmosCustomRoiEntry =
      gem.config.GmosConfig.GmosCustomRoiEntry.unsafeFromDescription(1, 1, 1, 1)

    val gmosNorthSmartGcalSearchKey     =
      DynamicConfig.GmosNorth.Default.key

    val gmosSouthSmartGcalSearchKey     =
      DynamicConfig.GmosSouth.Default.key

    val gmosNorthSmartGcalDefinitionKey = {
      val sk = DynamicConfig.GmosNorth.Default.key
      SmartGcalKey.GmosDefinition(
        sk.gmos,
        (Wavelength.ZeroAngstroms, Wavelength.ZeroAngstroms)
      )
    }

    val gmosSouthSmartGcalDefinitionKey = {
      val sk = DynamicConfig.GmosSouth.Default.key
      SmartGcalKey.GmosDefinition(
        sk.gmos,
        (Wavelength.ZeroAngstroms, Wavelength.ZeroAngstroms)
      )
    }

    val target: Target =
      Target("untitled", Track.Sidereal(ProperMotion.const(Coordinates.Zero)))

  }



}
