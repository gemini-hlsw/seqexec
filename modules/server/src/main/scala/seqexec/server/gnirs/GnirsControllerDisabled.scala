// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gnirs

import cats.Applicative
import cats.implicits._
import fs2.Stream
import org.typelevel.log4cats.Logger
import seqexec.model.dhs.ImageFileId
import seqexec.model.`enum`.ObserveCommandResult
import seqexec.server.Progress
import seqexec.server.overrideLogMessage
import squants.Time

class GnirsControllerDisabled[F[_]: Logger: Applicative] extends GnirsController[F] {
  private val name = "GNIRS"

  override def applyConfig(config: GnirsController.GnirsConfig): F[Unit]            =
    overrideLogMessage(name, "applyConfig")

  override def observe(fileId: ImageFileId, expTime: Time): F[ObserveCommandResult] =
    overrideLogMessage(name, s"observe $fileId").as(ObserveCommandResult.Success)

  override def endObserve: F[Unit]                                                  = overrideLogMessage(name, "endObserve")

  override def stopObserve: F[Unit] = overrideLogMessage(name, "stopObserve")

  override def abortObserve: F[Unit] = overrideLogMessage(name, "abortObserve")

  override def observeProgress(total: Time): Stream[F, Progress] = Stream.empty

  override def calcTotalExposureTime(cfg: GnirsController.DCConfig): F[Time] =
    GnirsController.calcTotalExposureTime[F](cfg)
}
