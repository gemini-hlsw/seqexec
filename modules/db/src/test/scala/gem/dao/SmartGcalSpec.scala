package gem.dao

import gem._
import gem.SmartGcal._
import gem.config._
import gem.config.GcalConfig.GcalLamp
import gem.enum._
import GcalLampType.{Arc, Flat}
import GcalBaselineType.Night

import doobie.imports._
import org.scalatest.{Assertion, FlatSpec, Matchers}

import java.time.Duration

import scalaz.effect.IO

import scalaz._, Scalaz._

class SmartGcalSpec extends FlatSpec with Matchers {

  import SmartGcalSpec._

  "SmartGcalDao" should "expand smart gcal arc steps" in {
    runF2Expansion(SmartGcalType.Arc) { (_, steps) =>
      verifySteps(GcalLampType.Arc.left, steps)
    }
  }

  it should "expand smart gcal flat steps" in {
    runF2Expansion(SmartGcalType.Flat) { (_, steps) =>
      verifySteps(GcalLampType.Flat.left, steps)
    }
  }

  it should "expand smart gcal baseline steps" in {
    runF2Expansion(SmartGcalType.NightBaseline) { (_, steps) =>
      verifySteps(GcalBaselineType.Night.right, steps)
    }
  }

  it should "fail when there is no corresponding mapping" in {
    runF2Expansion(SmartGcalType.DayBaseline) { (expansion, steps) =>
      expansion    shouldEqual noMappingDefined.left[ExpandedSteps]
      steps.values shouldEqual List(SmartGcalStep(f2, SmartGcalType.DayBaseline))
    }
  }

  it should "fail when the location is not found" in {
    runF2Expansion(SmartGcalType.Arc, loc1, loc2) { (expansion, steps) =>
      expansion shouldEqual stepNotFound(loc2).left[ExpandedSteps]
      steps.values shouldEqual List(SmartGcalStep(f2, SmartGcalType.Arc))
    }
  }

  it should "fail when the location is not a smart gcal step" in {
    val expansion = doTest {
      for {
        _  <- StepDao.insert(oid, loc1, BiasStep(f2))
        ex <- SmartGcalDao.expand(oid, loc1).run
        ss <- StepDao.selectAll(oid)
        _  <- ss.keys.traverseU { StepDao.delete(oid, _) }
      } yield ex
    }

    expansion shouldEqual notSmartGcal.left[ExpandedSteps]
  }

  it should "expand intermediate smart gcal steps" in {
    val steps = doTest {
      for {
        _  <- StepDao.insert(oid, loc1, BiasStep(f2))
        _  <- StepDao.insert(oid, loc2, SmartGcalStep(f2, SmartGcalType.NightBaseline))
        _  <- StepDao.insert(oid, loc9, DarkStep(f2))
        _  <- SmartGcalDao.expand(oid, loc2).run
        ss <- StepDao.selectAll(oid)
        _  <- ss.keys.traverseU { StepDao.delete(oid, _) }
      } yield ss
    }

    val exp = gcals.filter(_._2 == GcalBaselineType.Night).map { case (_, _, config) => GcalStep(f2, config) }
    steps.values shouldEqual (BiasStep(f2) :: exp) :+ DarkStep(f2)
  }

  private def verifySteps(m: GcalLampType \/ GcalBaselineType, ss: Location.Middle ==>> Step[DynamicConfig]): Assertion = {
    def lookup(m: GcalLampType \/ GcalBaselineType): List[GcalConfig] =
      gcals.filter(t => m.fold(_ == t._1, _ == t._2)).map(_._3)

    ss.values shouldEqual lookup(m).map(GcalStep(f2, _))
  }
}

object SmartGcalSpec {
  private val pid = Program.Id.parse("GS-2345A-Q-1")
  private val oid = Observation.Id(pid, 1)

  private val xa  = DriverManagerTransactor[IO](
    "org.postgresql.Driver",
    "jdbc:postgresql:gem",
    "postgres",
    ""
  )

  private val loc1: Location.Middle = Location.unsafeMiddle(1)
  private val loc2: Location.Middle = Location.unsafeMiddle(2)
  private val loc9: Location.Middle = Location.unsafeMiddle(9)

  private val f2: F2DynamicConfig =
    F2DynamicConfig(
      /* Disperser             */ F2Disperser.R1200JH,
      Duration.ofMillis(1000),
      /* Filter                */ F2Filter.JH,
      /* FPU                   */ F2FpUnit.LongSlit1,
      F2LyotWheel.F16,
      F2ReadMode.Bright,
      F2WindowCover.Open)


  private val gcals: List[(GcalLampType, GcalBaselineType, GcalConfig)] =
    List(
      (Arc, Night, GcalConfig(
        GcalLamp.fromArcs(GcalArc.ArArc),
        GcalFilter.Nir,
        GcalDiffuser.Ir,
        GcalShutter.Closed,
        Duration.ofMillis(30000),
        1
      )),
      (Flat, Night, GcalConfig(
        GcalLamp.fromContinuum(GcalContinuum.IrGreyBodyHigh),
        GcalFilter.Nd20,
        GcalDiffuser.Ir,
        GcalShutter.Open,
        Duration.ofMillis(20000),
        1
      ))
    )

  private def runF2Expansion(t: SmartGcalType)(verify: (ExpansionError \/ ExpandedSteps, Location.Middle ==>> Step[DynamicConfig]) => Assertion): Assertion =
    runF2Expansion(t, loc1, loc1)(verify)

  private def runF2Expansion(t: SmartGcalType, insertionLoc: Location.Middle, searchLoc: Location.Middle)(verify: (ExpansionError \/ ExpandedSteps, Location.Middle ==>> Step[DynamicConfig]) => Assertion): Assertion = {
    val (expansion, steps) = doTest {
      for {
        _  <- StepDao.insert(oid, insertionLoc, SmartGcalStep(f2, t))
        ex <- SmartGcalDao.expand(oid, searchLoc).run
        ss <- StepDao.selectAll(oid)
        _  <- ss.keys.traverseU { StepDao.delete(oid, _) }
      } yield (ex, ss)
    }

    verify(expansion, steps)
  }

  private def doTest[A](test: ConnectionIO[A]): A =
    (for {
      _ <- ProgramDao.insert(Program(pid, "SmartGcalSpec Prog", List.empty[Step[Nothing]]))
      _ <- ObservationDao.insert(Observation(oid, "SmartGcalSpec Obs", F2StaticConfig(mosPreImaging = false), List.empty[Step[DynamicConfig]]))
      a <- test
      _ <- sql"""DELETE FROM observation WHERE observation_id = $oid""".update.run
      _ <- sql"""DELETE FROM program     WHERE program_id     = $pid""".update.run
    } yield a).transact(xa).unsafePerformIO()

}
