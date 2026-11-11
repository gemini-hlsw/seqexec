// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.altair

import cats.effect.IO
import cats.syntax.all._
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.noop.NoOpLogger
import seqexec.server.tcs.TcsController.{ FollowOption, NodChopTrackingConfig, ProbeTrackingConfig }

/**
 * REL-4159, hardware safety. In LGS+P1 / LGS+OI the AOWFS has no target; demanding AO follow there
 * drove the Altair stage into its limit on 2026-07-28 and damaged it. Whatever the step asks for,
 * these modes must never demand follow On.
 */
class AltairSpec extends munit.FunSuite {

  private implicit def unsafeLogger: Logger[IO] = NoOpLogger.impl[IO]

  private val altair = Altair[IO](new AltairControllerDisabled[IO], fieldLensIn = false)

  private val allTracking: List[ProbeTrackingConfig] = List(
    ProbeTrackingConfig.Parked,
    ProbeTrackingConfig.Off,
    ProbeTrackingConfig.Frozen,
    ProbeTrackingConfig.On(NodChopTrackingConfig.Normal)
  )

  List(AltairController.LgsWithP1, AltairController.LgsWithOi).foreach { mode =>
    test(s"$mode never demands AO follow On") {
      allTracking.foreach { t =>
        val d = altair.aoProbeTracking(mode, t)
        assert(d.follow === FollowOption.FollowOff, s"$mode with step tracking $t demanded $d")
        assert(d.getNodChop === NodChopTrackingConfig.AllOff,
               s"$mode with step tracking $t demanded $d"
        )
      }
    }
  }

}
