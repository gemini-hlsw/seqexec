// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package ui

import gem.CoAdds
import gem.config._
import gem.enum._
import gem.math._
import gem.json.instances.program._
import io.circe.syntax._
import java.time.{ Duration, Year }
import scala.collection.immutable.{ TreeMap, TreeSet }

object TestProgram {
  val pid: Program.Id =
    ProgramId.Science(Site.GS, Semester(Year.of(2018), Half.A), ProgramType.Q, Index.One)

  val vega: Target =
    Target("Vega",
      Right(ProperMotion(
        Coordinates.fromHmsDms.unsafeGet("18:36:56.336 38:47:01.28"),
        Epoch.J2000,
        Some(Offset(
          Offset.P(Angle.fromMicroarcseconds(200940L)),
          Offset.Q(Angle.fromMicroarcseconds(286230L))
        )),
        Some(RadialVelocity.fromMetersPerSecond.unsafeGet(-20686)),
        Some(Angle.fromMicroarcseconds(130230L)))
      )
    )

  val gcal: GcalConfig =
    GcalConfig(
      Right(GcalConfig.GcalArcs.of(GcalArc.ArArc)),
      GcalFilter.None,
      GcalDiffuser.Ir,
      GcalShutter.Open,
      Duration.ofSeconds(1L),
      CoAdds.One
    )

  val f2: Observation.Flamingos2 =
    Observation.Flamingos2(
      "F2 Observation",
      TargetEnvironment.Flamingos2(None, TreeSet(UserTarget(vega, UserTargetType.BlindOffset))),
      StaticConfig.Flamingos2.Default,
      List(DynamicConfig.Flamingos2.Default.toStep(Step.Base.Gcal(gcal)))
    )

  val gmosS: Observation.GmosS =
    Observation.GmosS(
      "GMOS-S Observation",
      TargetEnvironment.GmosS(None, TreeSet(UserTarget(vega, UserTargetType.BlindOffset))),
      StaticConfig.GmosS.Default,
      List(DynamicConfig.GmosS.Default.toStep(Step.Base.SmartGcal(SmartGcalType.Arc)))
    )

  val gmosN: Observation.GmosN =
    Observation.GmosN(
      "GMOS-N Observation",
      TargetEnvironment.GmosN(None, TreeSet(UserTarget(vega, UserTargetType.BlindOffset))),
      StaticConfig.GmosN.Default,
      List(DynamicConfig.GmosN.Default.toStep(Step.Base.Bias))
    )

  val p: Program =
    Program(
      pid,
      "Test Program",
      TreeMap[Index, Observation](
        Index.fromShort.unsafeGet(1) -> f2,
        Index.fromShort.unsafeGet(2) -> gmosS,
        Index.fromShort.unsafeGet(3) -> gmosN
      )
    )

}

object Main {
  def main(args: Array[String]): Unit = {
    // scalastyle:off
    println(TestProgram.p.asJson.spaces2)
    // scalastyle:on
  }
}
