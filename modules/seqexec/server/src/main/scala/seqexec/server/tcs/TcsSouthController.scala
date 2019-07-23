// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.tcs

import cats.data.NonEmptySet
import seqexec.server.gems.Gems
import seqexec.server.tcs.TcsController.{Subsystem, TcsConfig}

trait TcsSouthController[F[_]] {
  import TcsSouthController._

  def applyConfig(subsystems: NonEmptySet[Subsystem],
                  gaos: Option[Gems[F]],
                  tc: TcsSouthConfig): F[Unit]

  def notifyObserveStart: F[Unit]

  def notifyObserveEnd: F[Unit]
}

object TcsSouthController {

  type TcsSouthConfig = TcsConfig[Nothing, Nothing]

}
