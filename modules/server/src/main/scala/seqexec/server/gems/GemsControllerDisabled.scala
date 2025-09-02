// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gems

import cats.Applicative
import cats.implicits._
import org.typelevel.log4cats.Logger
import seqexec.server.overrideLogMessage
import seqexec.server.gems.Gems.GemsWfsState
import seqexec.server.tcs.Gaos
import seqexec.server.tcs.Gaos.PauseResume

class GemsControllerDisabled[F[_]: Logger: Applicative] extends GemsController[F] {
  override def pauseResume(
    pauseReasons:  Gaos.PauseConditionSet,
    resumeReasons: Gaos.ResumeConditionSet
  )(cfg: GemsController.GemsConfig): F[Gaos.PauseResume[F]] =
    PauseResume(
      overrideLogMessage("GeMS", "pause AO loops").some,
      overrideLogMessage("GeMS", "resume AO loops").some
    ).pure[F]

  override val stateGetter: GemsWfsState[F] = GemsWfsState.allOff
}
