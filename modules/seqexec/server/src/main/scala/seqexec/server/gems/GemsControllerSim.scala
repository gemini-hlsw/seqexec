// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gems
import cats.effect.Sync
import cats.implicits._
import seqexec.server.tcs.Gaos.{PauseConditionSet, PauseResume, ResumeConditionSet}
import org.log4s.getLogger
import seqexec.server.gems.Gems.{GemsWfsState, Cwfs1DetectorState, Cwfs2DetectorState, Cwfs3DetectorState, Odgw1DetectorState, Odgw2DetectorState, Odgw3DetectorState, Odgw4DetectorState}
import seqexec.server.gems.GemsController.GemsConfig

final class GemsControllerSim[F[_]: Sync] private extends GemsController[F] {
  import GemsControllerSim._

  override def pauseResume(pauseReasons: PauseConditionSet, resumeReasons: ResumeConditionSet)
                          (cfg: GemsConfig)
  : F[PauseResume[F]] = { PauseResume(
    Sync[F].delay(Log.info(s"Simulate pausing GeMS loops because of $pauseReasons")).some,
    Sync[F].delay(Log.info(s"Simulate restoring GeMS configuration $cfg because of $resumeReasons")).some
  ) }.pure[F]

  override val stateGetter: Gems.GemsWfsState[F] = GemsWfsState[F](
    (Cwfs1DetectorState.Off:Cwfs1DetectorState).some.pure[F],
    (Cwfs2DetectorState.Off:Cwfs2DetectorState).some.pure[F],
    (Cwfs3DetectorState.Off:Cwfs3DetectorState).some.pure[F],
    (Odgw1DetectorState.Off:Odgw1DetectorState).some.pure[F],
    (Odgw2DetectorState.Off:Odgw2DetectorState).some.pure[F],
    (Odgw3DetectorState.Off:Odgw3DetectorState).some.pure[F],
    (Odgw4DetectorState.Off:Odgw4DetectorState).some.pure[F]
  )
}

object GemsControllerSim {
  val Log = getLogger

  def apply[F[_]: Sync]: GemsController[F] = new GemsControllerSim[F]
}