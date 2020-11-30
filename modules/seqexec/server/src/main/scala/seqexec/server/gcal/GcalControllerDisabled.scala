package seqexec.server.gcal

import io.chrisdavenport.log4cats.Logger
import seqexec.server.SystemOverrides.overrideLogMessage

class GcalControllerDisabled[F[_]: Logger] extends GcalController[F] {
  override def applyConfig(config: GcalController.GcalConfig): F[Unit] = overrideLogMessage("GCAL", "applyConfig")
}
