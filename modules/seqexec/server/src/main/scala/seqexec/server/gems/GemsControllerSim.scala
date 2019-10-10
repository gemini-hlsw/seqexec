// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gems

import cats.Applicative
import cats.implicits._
import io.chrisdavenport.log4cats.Logger
import seqexec.server.tcs.Gaos.{PauseConditionSet, PauseResume, ResumeConditionSet}
import seqexec.server.gems.Gems.{GemsWfsState, Cwfs1DetectorState, Cwfs2DetectorState, Cwfs3DetectorState, Odgw1DetectorState, Odgw2DetectorState, Odgw3DetectorState, Odgw4DetectorState}
import seqexec.server.gems.GemsController.GemsConfig

object GemsControllerSim {
  def apply[F[_]: Applicative](implicit L: Logger[F]): GemsController[F] =
    new GemsController[F] {
      override def pauseResume(pauseReasons: PauseConditionSet, resumeReasons: ResumeConditionSet)
                              (cfg: GemsConfig)
      : F[PauseResume[F]] =
        PauseResume(
          L.info(s"Simulate pausing GeMS loops because of $pauseReasons").some,
          L.info(s"Simulate restoring GeMS configuration $cfg because of $resumeReasons").some
        ).pure[F]

      override val stateGetter: Gems.GemsWfsState[F] = GemsWfsState[F](
        (Cwfs1DetectorState.Off:Cwfs1DetectorState).pure[F],
        (Cwfs2DetectorState.Off:Cwfs2DetectorState).pure[F],
        (Cwfs3DetectorState.Off:Cwfs3DetectorState).pure[F],
        (Odgw1DetectorState.Off:Odgw1DetectorState).pure[F],
        (Odgw2DetectorState.Off:Odgw2DetectorState).pure[F],
        (Odgw3DetectorState.Off:Odgw3DetectorState).pure[F],
        (Odgw4DetectorState.Off:Odgw4DetectorState).pure[F]
      )
    }
}
