// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.flamingos2

import cats.Functor
import cats.implicits._
import fs2.Stream
import org.typelevel.log4cats.Logger
import seqexec.model.dhs.ImageFileId
import seqexec.model.`enum`.ObserveCommandResult
import seqexec.server.Progress
import seqexec.server.overrideLogMessage
import squants.Time

class Flamingos2ControllerDisabled[F[_]: Logger: Functor] extends Flamingos2Controller[F] {
  override def applyConfig(config: Flamingos2Controller.Flamingos2Config): F[Unit] =
    overrideLogMessage("Flamingos-2", "applyConfig")

  override def observe(fileId: ImageFileId, expTime: Time): F[ObserveCommandResult] =
    overrideLogMessage("Flamingos-2", s"observe $fileId").as(ObserveCommandResult.Success)

  override def endObserve: F[Unit] = overrideLogMessage("FLAMINGOS-2", "endObserve")

  override def observeProgress(total: Time): Stream[F, Progress] = Stream.empty
}
