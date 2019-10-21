// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.dao

import doobie.implicits._
import gem._
import gem.config._
import gem.config.F2Config.F2FpuChoice.Builtin
import gem.enum._
import gsp.math.{ Angle, Index, Offset }
import java.time.Duration
import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import scala.collection.immutable.{ TreeMap, TreeSet }


class StepDaoSpec extends AnyFlatSpec with Matchers with DaoTest {

  "StepDao" should "serialize telescope configurations properly" in {

    val idx  = Index.One

    // We specifically want to test round-tripping of telescope offsets.
    val pid  = Program.Id.fromString.unsafeGet("GS-1234A-Q-1")
    val orig = Program(
      pid,
      "Untitled Prog",
      TreeMap(idx ->
        Observation.Flamingos2(
          "Untitled Obs",
          TargetEnvironment.Flamingos2(None, TreeSet.empty),
          StaticConfig.Flamingos2(MosPreImaging.IsNotMosPreImaging),
          List(
            Step.Flamingos2(
              DynamicConfig.Flamingos2(
                None,
                Duration.ZERO,
                F2Filter.Dark,
                Some(Builtin(F2Fpu.LongSlit1)),
                F2LyotWheel.F16,
                F2ReadMode.Bright,
                F2WindowCover.Close
              ),
              Step.Base.Science(
                TelescopeConfig(
                  Offset.P(Angle.milliarcseconds.reverseGet( 1250)), // 1.25 arcseconds
                  Offset.Q(Angle.milliarcseconds.reverseGet(-2750))
                )
              )
            )
          )
        )
      )
    )

    // We should be able to round-trip the program.
    import ProgramDao._
    val rted = (insert(orig) flatMap selectFull).transact(xa).unsafeRunSync
    rted shouldEqual Some(orig)

  }

}
