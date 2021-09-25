// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import scala.concurrent.duration._

import cats.data.Kleisli
import fs2.Stream
import seqexec.model.dhs.ImageFileId
import seqexec.model.enum.Instrument
import seqexec.model.enum.ObserveCommandResult
import seqexec.server.keywords.KeywordsClient
import squants.Time

trait InstrumentSystem[F[_]] extends System[F] {
  override val resource: Instrument

  val contributorName: String

  def observeControl(config: CleanConfig): InstrumentSystem.ObserveControl[F]

  def observe(config: CleanConfig): Kleisli[F, ImageFileId, ObserveCommandResult]

  //Expected total observe lapse, used to calculate timeout
  def calcObserveTime(config: CleanConfig): F[Time]

  def observeTimeout: FiniteDuration = 1.minute

  def keywordsClient: KeywordsClient[F]

  def observeProgress(total: Time, elapsed: InstrumentSystem.ElapsedTime): Stream[F, Progress]

  def instrumentActions(config: CleanConfig): InstrumentActions[F]

}

object InstrumentSystem {
  val ObserveOperationsTimeout = 1.minute

  final case class StopObserveCmd[F[_]](self: Boolean => F[Unit])
  final case class AbortObserveCmd[F[_]](self: F[Unit])
  final case class PauseObserveCmd[F[_]](self: Boolean => F[Unit])

  final case class ContinuePausedCmd[F[_]](self: Time => F[ObserveCommandResult])
  final case class StopPausedCmd[F[_]](self: F[ObserveCommandResult])
  final case class AbortPausedCmd[F[_]](self: F[ObserveCommandResult])

  sealed trait ObserveControl[+F[_]]       extends Product with Serializable
  case object Uncontrollable               extends ObserveControl[Nothing]
  final case class CompleteControl[F[_]](
    stop:        StopObserveCmd[F],
    abort:       AbortObserveCmd[F],
    pause:       PauseObserveCmd[F],
    continue:    ContinuePausedCmd[F],
    stopPaused:  StopPausedCmd[F],
    abortPaused: AbortPausedCmd[F]
  ) extends ObserveControl[F]
  // Special class for instrument, that cannot pause/resume like IR instruments and GSAOI
  final case class UnpausableControl[F[_]](stop: StopObserveCmd[F], abort: AbortObserveCmd[F])
      extends ObserveControl[F]

  final case class ElapsedTime(self: Time) extends AnyVal
}
