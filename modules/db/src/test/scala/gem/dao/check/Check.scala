package gem.dao
package check

import gem._
import gem.enum._
import gem.config._
import edu.gemini.spModel.core.{ Site, ProgramType, Semester, _ }

import doobie.imports._
import doobie.scalatest.imports._
import org.scalatest._

import scalaz._, Scalaz._

/** Trait for tests that check statement syntax and mappings. */
trait Check extends FlatSpec with Matchers with IOLiteChecker {

  def transactor = DriverManagerTransactor[IOLite](
    "org.postgresql.Driver",
    "jdbc:postgresql:gem",
    "postgres",
    ""
  )

  /**
   * Some dummy values to pass to statement constructors. The actual values don't matter because
   * the statements are never executed, but they are dereferenced and cannot be null.
   */
  object Dummy {
    val instant          = java.time.Instant.EPOCH
    val duration         = java.time.Duration.ZERO
    val programId        = Program.Id.Arbitrary(None, None, None, "")
    val semester         = Semester.parse("2015-B")
    val site             = Site.GN
    val programType      = ProgramType.Classical
    val dailyProgramId   = Program.Id.Daily(site, programType, 0, 0, 0)
    val scienceProgramId = Program.Id.Science(site, semester, programType, 0)
    val observationId    = Observation.Id(programId, 0)
    val datasetLabel     = Dataset.Label(observationId, 0)
    val dataset          = Dataset(datasetLabel, "", instant)
    val eventType        = EventType.Abort
    val gcalLamp         = GcalContinuum.IrGreyBodyLow.left
    val gcalFilter       = GcalFilter.None
    val gcalDiffuser     = GcalDiffuser.Ir
    val gcalShutter      = GcalShutter.Open
    val gcalConfig       = GcalConfig(gcalLamp, gcalFilter, gcalDiffuser, gcalShutter, duration, 0)
    val user             = User[Nothing]("", "", "", "", false, Map.empty)
    val observation      = Observation[StaticConfig, Nothing](observationId, "", F2StaticConfig(mosPreImaging = false), Nil)
    val program          = Program(programId, "", Nil)
    val f2SmartGcalKey   = F2SmartGcalKey(F2Disperser.NoDisperser, F2Filter.Dark, F2FpUnit.LongSlit1)
    val gcalLampType     = GcalLampType.Arc
    val gcalBaselineType = GcalBaselineType.Day
    val locationMiddle   = Location.unsafeMiddle(1)
    val f2Config         = F2DynamicConfig(F2Disperser.NoDisperser, duration, F2Filter.Dark, F2FpUnit.LongSlit1,
      F2LyotWheel.F16, F2ReadMode.Bright, F2WindowCover.Close)
    val telescopeConfig  = TelescopeConfig(OffsetP.Zero, OffsetQ.Zero)
    val smartGcalType    = SmartGcalType.Arc
    val instrumentConfig = f2Config
    val stepType         = StepType.Science
    val f2Static         = F2StaticConfig(mosPreImaging = false)
  }



}
