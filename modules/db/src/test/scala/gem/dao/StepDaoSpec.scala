// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.dao

import doobie.imports._
import gem._
import gem.config._
import gem.config.F2Config.F2FpuChoice.Builtin
import gem.enum._
import gem.math._
import java.time.Duration
import org.scalatest._

class StepDaoSpec extends FlatSpec with Matchers with DaoTest {

  "StepDao" should "serialize telescope configurations properly" in {

    // We specifically want to test round-tripping of telescope offsets.
    val pid  = Program.Id.unsafeFromString("GS-1234A-Q-1")
    val orig = Program(
      pid,
      "Untitled Prog",
      List(
        Observation(
          Observation.Id(pid, 1),
          "Untitled Obs",
          StaticConfig.F2(MosPreImaging.IsNotMosPreImaging),
          List(
            Step.Science(
              DynamicConfig.F2(
                None,
                Duration.ZERO,
                F2Filter.Dark,
                Some(Builtin(F2FpUnit.LongSlit1)),
                F2LyotWheel.F16,
                F2ReadMode.Bright,
                F2WindowCover.Close
              ),
              TelescopeConfig(
                Offset.P(Angle.fromMilliarcseconds( 1250)), // 1.25 arcseconds
                Offset.Q(Angle.fromMilliarcseconds(-2750))
              )
            )
          )
        )
      )
    )

    // We should be able to round=trip the program.
    import ProgramDao._
    val rted = (insert(orig) >>= selectFull).transact(xa).unsafePerformIO
    rted shouldEqual Some(orig)

  }

}
