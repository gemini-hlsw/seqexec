// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package ui

import gem.config._
import gem.enum._
import gem.math._

import java.time.{ Duration, Year }
import scala.collection.immutable.{ TreeMap, TreeSet }

object TestProgram {
  val pid: Program.Id =
    ProgramId.Science.unsafeApply(Site.GS, Semester(Year.of(2018), Half.A), ProgramType.Q, 1)

  val vega: Target =
    Target("Vega",
      Right(ProperMotion(
        Coordinates.fromHmsDms.unsafeGet("18:36:56.336 38:47:01.28"),
        Epoch.J2000,
        Some(Offset(
          Offset.P(Angle.fromMicroarcseconds(200940L)),
          Offset.Q(Angle.fromMicroarcseconds(286230L))
        )),
        Some(RadialVelocity(-20686)),
        Some(Angle.fromMicroarcseconds(130230L)))
      )
    )

  val gcal: GcalConfig =
    GcalConfig(
      Right(GcalConfig.GcalArcs(GcalArc.ArArc, Nil)),
      GcalFilter.None,
      GcalDiffuser.Ir,
      GcalShutter.Open,
      Duration.ofSeconds(1L),
      1
    )

  val f2: Observation[StaticConfig, Step[DynamicConfig]] =
    Observation(
      "F2 Observation",
      TargetEnvironment(TreeSet(UserTarget(vega, UserTargetType.BlindOffset))),
      StaticConfig.F2.Default,
      List(Step.Gcal(DynamicConfig.F2.Default, gcal))
    )

  val gmosS: Observation[StaticConfig, Step[DynamicConfig]] =
    Observation(
      "GMOS-S Observation",
      TargetEnvironment(TreeSet(UserTarget(vega, UserTargetType.BlindOffset))),
      StaticConfig.GmosSouth.Default,
      List(Step.SmartGcal(DynamicConfig.GmosSouth.Default, SmartGcalType.Arc))
    )

  val gmosN: Observation[StaticConfig, Step[DynamicConfig]] =
    Observation(
      "GMOS-N Observation",
      TargetEnvironment(TreeSet(UserTarget(vega, UserTargetType.BlindOffset))),
      StaticConfig.GmosNorth.Default,
      List(Step.Bias(DynamicConfig.GmosNorth.Default))
    )

  val p: Program[Observation[StaticConfig, Step[DynamicConfig]]] =
    Program(
      pid,
      "Test Program",
      TreeMap(
        Observation.Index.unsafeFromShort(1) -> f2,
        Observation.Index.unsafeFromShort(2) -> gmosS,
        Observation.Index.unsafeFromShort(3) -> gmosN
      )
    )

}

object Main {
  def main(args: Array[String]): Unit = {
    // scalastyle:off
    println(TestProgram.p)
    // scalastyle:on
  }
}
