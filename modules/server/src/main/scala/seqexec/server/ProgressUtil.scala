// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.duration.SECONDS

import cats.data.StateT
import cats.syntax.all._
import fs2.Stream
import seqexec.model.ObserveStage
import squants.time.Milliseconds
import squants.time.Seconds
import squants.time.Time
import squants.time.TimeConversions._
import cats.effect.Temporal

object ProgressUtil {
  private val PollPeriod = FiniteDuration(1, SECONDS)

  def fromF[F[_]: Temporal](f: FiniteDuration => F[Progress]): Stream[F, Progress] =
    Stream.awakeEvery[F](PollPeriod).evalMap(f)

  def fromFOption[F[_]: Temporal](
    f: FiniteDuration => F[Option[Progress]]
  ): Stream[F, Progress] =
    Stream.awakeEvery[F](PollPeriod).evalMap(f).collect { case Some(p) => p }

  def fromStateT[F[_]: Temporal, S](
    fs: FiniteDuration => StateT[F, S, Progress]
  ): S => Stream[F, Progress] = s0 =>
    Stream
      .awakeEvery[F](PollPeriod)
      .evalMapAccumulate(s0) { case (st, t) => fs(t).run(st) }
      .map(_._2)

  def fromStateTOption[F[_]: Temporal, S](
    fs: FiniteDuration => StateT[F, S, Option[Progress]]
  ): S => Stream[F, Progress] = s0 =>
    Stream
      .awakeEvery[F](PollPeriod)
      .evalMapAccumulate(s0) { case (st, t) => fs(t).run(st) }
      .map(_._2)
      .collect { case Some(p) => p }

  /**
   * Simple simulated countdown
   */
  def countdown[F[_]: Temporal](total: Time, elapsed: Time): Stream[F, Progress] =
    ProgressUtil
      .fromF[F] { t: FiniteDuration =>
        val progress  = Milliseconds(t.toMillis) + elapsed
        val remaining = total - progress
        val clipped   = if (remaining.value >= 0.0) remaining else Seconds(0.0)
        ObsProgress(total, RemainingTime(clipped), ObserveStage.Acquiring).pure[F].widen[Progress]
      }
      .takeThrough(_.remaining.self.value > 0.0)

  /**
   * Simulated countdown with simulated observation stage
   */
  def obsCountdown[F[_]: Temporal](total: Time, elapsed: Time): Stream[F, Progress] =
    Stream.emit(ObsProgress(total, RemainingTime(total), ObserveStage.Preparing)) ++
      countdown[F](total, elapsed) ++
      Stream.emit(ObsProgress(total, RemainingTime(0.0.seconds), ObserveStage.ReadingOut))

  /**
   * Simulated countdown with observation stage provided by instrument
   */
  def obsCountdownWithObsStage[F[_]: Temporal](
    total:   Time,
    elapsed: Time,
    stage:   F[ObserveStage]
  ): Stream[F, Progress] =
    ProgressUtil
      .fromF[F] { t: FiniteDuration =>
        val progress  = Milliseconds(t.toMillis) + elapsed
        val remaining = total - progress
        val clipped   = if (remaining.value >= 0.0) remaining else Seconds(0.0)
        stage.map(v => ObsProgress(total, RemainingTime(clipped), v)).widen[Progress]
      }
      .takeThrough(x => x.remaining.self.value > 0.0 || x.stage === ObserveStage.Acquiring)

  /**
   * Simulated countdown with observation stage provided by instrument
   */
  def realCountdownWithObsStage[F[_]: Temporal](
    total:       Time,
    secondsLeft: Stream[F, Time],
    stage:       => F[ObserveStage]
  ): Stream[F, Progress] =
    for {
      t <- secondsLeft
      p <- Stream.eval(stage.map(ObsProgress(total, RemainingTime(t), _)))
    } yield p
}
