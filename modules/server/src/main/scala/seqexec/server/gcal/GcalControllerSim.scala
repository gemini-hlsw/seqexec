// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gcal

import cats.implicits._
import org.typelevel.log4cats.Logger
import seqexec.server.gcal.GcalController.GcalConfig
import seqexec.server.gcal.GcalController.gcalConfigShow

object GcalControllerSim {
  def apply[F[_]: Logger]: GcalController[F] = new GcalController[F] {
    override def applyConfig(config: GcalConfig): F[Unit] =
      Logger[F].debug(s"Simulating GCAL configuration: ${config.show}")
  }
}
