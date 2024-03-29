// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gsaoi

import cats.Applicative
import cats.effect.Async
import cats.syntax.all._
import org.typelevel.log4cats.Logger
import seqexec.model.dhs.ImageFileId
import seqexec.model.enum.ObserveCommandResult
import seqexec.server.InstrumentControllerSim
import seqexec.server.InstrumentSystem.ElapsedTime
import seqexec.server.Progress
import seqexec.server.gsaoi.GsaoiController.DCConfig
import seqexec.server.gsaoi.GsaoiController.GsaoiConfig
import squants.Time
import squants.time.TimeConversions._

object GsaoiControllerSim {
  def apply[F[_]: Logger: Async]: F[GsaoiFullHandler[F]] =
    InstrumentControllerSim[F]("GSAOI").map { sim =>
      new GsaoiFullHandler[F] {

        override def observe(fileId: ImageFileId, cfg: DCConfig): F[ObserveCommandResult] =
          calcTotalExposureTime(cfg).flatMap {
            sim.observe(fileId, _)
          }

        override def applyConfig(config: GsaoiConfig): F[Unit] =
          sim.applyConfig(config)

        override def stopObserve: F[Unit] = sim.stopObserve

        override def abortObserve: F[Unit] = sim.abortObserve

        override def endObserve: F[Unit] = sim.endObserve

        override def observeProgress(total: Time): fs2.Stream[F, Progress] =
          sim.observeCountdown(total, ElapsedTime(0.seconds))

        override def currentState: F[GsaoiGuider.GuideState] = (new GsaoiGuider.GuideState {
          override def isGuideActive: Boolean = false

          override def isOdgwGuiding(odgwId: GsaoiGuider.OdgwId): Boolean = false
        }).pure[F]

        override def guide: F[Unit] = Applicative[F].unit

        override def endGuide: F[Unit] = Applicative[F].unit
      }
    }

}
