// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import cats.data.Kleisli
import fs2.Stream
import gem.enum.LightSinkName
import seqexec.model.dhs.ImageFileId
import seqexec.model.enum.Instrument
import seqexec.model.enum.ObserveCommandResult
import seqexec.server.keywords.KeywordsClient
import squants.{Length, Time}
import scala.concurrent.duration._

trait InstrumentSystem[F[_]] extends System[F] with InstrumentGuide {
  override val resource: Instrument
  // The name used for this instrument in the science fold configuration
  def sfName(config: CleanConfig): LightSinkName
  val contributorName: String

  def observeControl(config: CleanConfig): InstrumentSystem.ObserveControl[F]

  def observe(config: CleanConfig): Kleisli[F, ImageFileId, ObserveCommandResult]

  //Expected total observe lapse, used to calculate timeout
  def calcObserveTime(config: CleanConfig): F[Time]

  def observeTimeout: FiniteDuration = 1.minute

  def keywordsClient: KeywordsClient[F]

  def observeProgress(total: Time, elapsed: InstrumentSystem.ElapsedTime): Stream[F, Progress]

  def instrumentActions(config: CleanConfig): InstrumentActions[F]

  def calcStepType(config: CleanConfig, isNightSeq: Boolean): Either[SeqexecFailure, StepType] =
    SequenceConfiguration.calcStepType(config, isNightSeq)

  override val oiOffsetGuideThreshold: Option[Length] = None

  override def instrument: Instrument = resource

}

object InstrumentSystem {
  val ObserveOperationsTimeout = 1.minute

  final case class StopObserveCmd[F[_]](self: Boolean => F[Unit])
  final case class AbortObserveCmd[F[_]](self: F[Unit])
  final case class PauseObserveCmd[F[_]](self: Boolean => F[Unit])

  final case class ContinuePausedCmd[F[_]](self: Time => F[ObserveCommandResult])
  final case class StopPausedCmd[F[_]](self: F[ObserveCommandResult])
  final case class AbortPausedCmd[F[_]](self: F[ObserveCommandResult])

  sealed trait ObserveControl[+F[_]] extends Product with Serializable
  case object Uncontrollable extends ObserveControl[Nothing]
  final case class CompleteControl[F[_]](
    stop: StopObserveCmd[F],
    abort: AbortObserveCmd[F],
    pause: PauseObserveCmd[F],
    continue: ContinuePausedCmd[F],
    stopPaused: StopPausedCmd[F],
    abortPaused: AbortPausedCmd[F]
  )
      extends ObserveControl[F]
  // Special class for instrument, that cannot pause/resume like IR instruments and GSAOI
  final case class UnpausableControl[F[_]](stop: StopObserveCmd[F], abort: AbortObserveCmd[F])
      extends ObserveControl[F]

  final case class ElapsedTime(self: Time) extends AnyVal
}
