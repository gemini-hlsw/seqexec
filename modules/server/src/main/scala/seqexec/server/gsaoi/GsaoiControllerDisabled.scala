// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gsaoi

import cats.Functor
import cats.implicits._
import fs2.Stream
import org.typelevel.log4cats.Logger
import seqexec.model.dhs.ImageFileId
import seqexec.model.`enum`.ObserveCommandResult
import seqexec.server.Progress
import seqexec.server.overrideLogMessage
import squants.Time

class GsaoiControllerDisabled[F[_]: Logger: Functor] extends GsaoiController[F] {
  private val name = "GSAOI"

  override def applyConfig(config: GsaoiController.GsaoiConfig): F[Unit] =
    overrideLogMessage(name, "")

  override def observe(
    fileId: ImageFileId,
    cfg:    GsaoiController.DCConfig
  ): F[ObserveCommandResult] =
    overrideLogMessage(name, s"observe $fileId").as(ObserveCommandResult.Success)

  override def endObserve: F[Unit] = overrideLogMessage(name, "endObserve")

  override def stopObserve: F[Unit] = overrideLogMessage(name, "stopObserve")

  override def abortObserve: F[Unit] = overrideLogMessage(name, "abortObserve")

  override def observeProgress(total: Time): Stream[F, Progress] = Stream.empty
}
