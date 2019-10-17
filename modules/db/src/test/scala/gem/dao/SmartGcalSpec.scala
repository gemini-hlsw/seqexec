// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.dao

import cats.implicits._
import doobie._, doobie.implicits._
import gem._
import gem.SmartGcal._
import gem.config._
import gem.config.F2Config.F2FpuChoice.Builtin
import gem.config.GcalConfig.GcalLamp
import gem.enum._
import gem.util.Location
import gsp.math.Index
import org.scalatest.Assertion
import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import java.time.Duration
import scala.collection.immutable.{ TreeMap, TreeSet }

class SmartGcalSpec extends AnyFlatSpec with Matchers with DaoTest {
  import GcalLampType.{ Arc, Flat }
  import GcalBaselineType.Night
  import SmartGcalSpec._

  "SmartGcalDao" should "expand smart gcal arc steps" in {
    runF2Expansionʹ(SmartGcalType.Arc) { (_, steps) =>
      verifySteps(Left(Arc), steps)
    }
  }

  it should "expand smart gcal flat steps" in {
    runF2Expansionʹ(SmartGcalType.Flat) { (_, steps) =>
      verifySteps(Left(Flat), steps)
    }
  }

  it should "expand smart gcal baseline steps" in {
    runF2Expansionʹ(SmartGcalType.NightBaseline) { (_, steps) =>
      verifySteps(Right(Night), steps)
    }
  }

  it should "fail when there is no corresponding mapping" in {
    runF2Expansionʹ(SmartGcalType.DayBaseline) { (expansion, steps) =>
      expansion    shouldEqual Left(noMappingDefined)
      steps.values.toList shouldEqual List(f2.toStep(Step.Base.SmartGcal(SmartGcalType.DayBaseline)))
    }
  }

  it should "fail when the location is not found" in {
    runF2Expansion(SmartGcalType.Arc, loc1, loc2) { (expansion, steps) =>
      expansion shouldEqual Left(stepNotFound(loc2))
      steps.values.toList shouldEqual List(f2.toStep(Step.Base.SmartGcal(SmartGcalType.Arc)))
    }
  }

  it should "fail when the location is not a smart gcal step" in {
    val expansion = doTest {
      for {
        _  <- StepDao.insert(oid, loc1, f2.toStep(Step.Base.Bias))
        ex <- SmartGcalDao.expand(oid, loc1).value
        ss <- StepDao.selectAll(oid)
        _  <- ss.keys.toList.traverse { StepDao.deleteAtLocation(oid, _) }
      } yield ex
    }

    expansion shouldEqual Left(notSmartGcal)
  }

  it should "expand intermediate smart gcal steps" in {
    val steps = doTest {
      for {
        _  <- StepDao.insert(oid, loc1, f2.toStep(Step.Base.Bias))
        _  <- StepDao.insert(oid, loc2, f2.toStep(Step.Base.SmartGcal(SmartGcalType.NightBaseline)))
        _  <- StepDao.insert(oid, loc9, f2.toStep(Step.Base.Dark))
        _  <- SmartGcalDao.expand(oid, loc2).value
        ss <- StepDao.selectAll(oid)
        _  <- ss.keys.toList.traverse { StepDao.deleteAtLocation(oid, _) }
      } yield ss
    }

    val exp = gcals.filter(_._2 == GcalBaselineType.Night).map { case (_, _, config) => f2.toStep(Step.Base.Gcal(config)) }
    steps.values.toList shouldEqual (f2.toStep(Step.Base.Bias) :: exp) :+ f2.toStep(Step.Base.Dark)
  }

  private def verifySteps(m: Either[GcalLampType, GcalBaselineType], ss: TreeMap[Location.Middle, Step]): Assertion = {
    def lookup(m: Either[GcalLampType, GcalBaselineType]): List[GcalConfig] =
      gcals.filter(t => m.fold(_ == t._1, _ == t._2)).map(_._3)

    ss.values.toList shouldEqual lookup(m).map(a => f2.toStep(Step.Base.Gcal(a)))
  }

  private val oid = Observation.Id(pid, Index.One)

  private def doTest[A](test: ConnectionIO[A]): A =
    withProgram {
      for {
        _ <- ObservationDao.insert(oid, Observation.Flamingos2("SmartGcalSpec Obs", TargetEnvironment.Flamingos2(None, TreeSet.empty), StaticConfig.Flamingos2.Default, Nil))
        a <- test
      } yield a
    }

  private def runF2Expansionʹ(t: SmartGcalType)(
    verify: (Either[ExpansionError, ExpandedSteps], TreeMap[Location.Middle, Step]) => Assertion
  ): Assertion =
    runF2Expansion(t, loc1, loc1)(verify)

  private def runF2Expansion(t: SmartGcalType, insertionLoc: Location.Middle, searchLoc: Location.Middle)(
    verify: (Either[ExpansionError, ExpandedSteps], TreeMap[Location.Middle, Step]) => Assertion
  ): Assertion = {
    val (expansion, steps) = doTest {
      for {
        _  <- StepDao.insert(oid, insertionLoc, f2.toStep(Step.Base.SmartGcal(t)))
        ex <- SmartGcalDao.expand(oid, searchLoc).value
        ss <- StepDao.selectAll(oid)
        _  <- ss.keys.toList.traverse { StepDao.deleteAtLocation(oid, _) }
      } yield (ex, ss)
    }

    verify(expansion, steps)
  }
}

object SmartGcalSpec {
  import GcalLampType.{ Arc, Flat }
  import GcalBaselineType.Night

  private val loc1: Location.Middle = Location.unsafeMiddle(1)
  private val loc2: Location.Middle = Location.unsafeMiddle(2)
  private val loc9: Location.Middle = Location.unsafeMiddle(9)

  private val f2: DynamicConfig.Flamingos2 =
    DynamicConfig.Flamingos2(
      /* Disperser             */ Some(F2Disperser.R1200JH),
      Duration.ofMillis(1000),
      /* Filter                */ F2Filter.JH,
      /* FPU                   */ Some(Builtin(F2Fpu.LongSlit1)),
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
        CoAdds.One
      )),
      (Flat, Night, GcalConfig(
        GcalLamp.fromContinuum(GcalContinuum.IrGreyBodyHigh),
        GcalFilter.Nd20,
        GcalDiffuser.Ir,
        GcalShutter.Open,
        Duration.ofMillis(20000),
        CoAdds.One
      ))
    )
}
