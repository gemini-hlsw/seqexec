package gem.dao

import gem._
import gem.config._
import gem.enum._

import doobie.imports._
import org.scalatest.{FlatSpec, Matchers}

import java.time.Duration

import scalaz.effect.IO

import scalaz._, Scalaz._

// TODO
// - Unknown location
// - Not smart gcal
// - No mapping
// - valid mapping, first step
// - valid mapping, in between step
// - mapping arc, flat, night, day



class SmartGcalSpec extends FlatSpec with Matchers {

  import SmartGcalSpec._

  "SmartGcalDao" should "expand smart gcal steps into gcal steps" in {

    val loc = Location.unsafeMiddle(1)
    val f2  = F2Config(
      F2Disperser.R1200JH,
      Duration.ofMillis(1000),
      F2Filter.JH,
      F2FpUnit.LongSlit1,
      F2LyotWheel.F16,
      mosPreimaging = true,
      F2ReadMode.Bright,
      F2WindowCover.Open
    )
    val gcal = GcalConfig(
      GcalConfig.unsafeMkLamp(None, GcalArc.ArArc -> true),
      GcalFilter.Nir,
      GcalDiffuser.Ir,
      GcalShutter.Closed,
      Duration.ofMillis(30000),
      1
    )

    val steps = genSteps {
      for {
        _  <- StepDao.insert(oid, loc, new SmartGcalStep(f2, SmartGcalType.Arc))
        _  <- SmartGcalDao.expand(oid, loc)
        ss <- StepDao.selectAll(oid)
      } yield ss
    }

    steps.values shouldEqual List(GcalStep(f2, gcal))
  }
}

object SmartGcalSpec {
  val pid = Program.Id.parse("GS-2345A-Q-1")
  val oid = Observation.Id(pid, 1)

  val xa  = DriverManagerTransactor[IO](
    "org.postgresql.Driver",
    "jdbc:postgresql:gem",
    "postgres",
    ""
  )

  def genSteps(test: ConnectionIO[Location.Middle ==>> Step[InstrumentConfig]]): Location.Middle ==>> Step[InstrumentConfig] =
    (for {
      _  <- ProgramDao.insert(Program(pid, "SmartGcalSpec Prog", List.empty[Step[Nothing]]))
      _  <- ObservationDao.insert(Observation(oid, "SmartGcalSpec Obs", Some(Instrument.Flamingos2), List.empty[Step[Nothing]]))
      ss <- test
      _  <- ss.keys.traverseU { StepDao.delete(oid, _) }
      _  <- sql"""DELETE FROM observation WHERE observation_id = $oid""".update.run
      _  <- sql"""DELETE FROM program     WHERE program_id     = $pid""".update.run
    } yield ss).transact(xa).unsafePerformIO()

}
