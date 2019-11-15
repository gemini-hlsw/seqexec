// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import cats.{Applicative, FlatMap, Functor}
import cats.data.StateT
import cats.effect.Timer
import cats.implicits._
import fs2.Stream
import seqexec.model.ObserveStage
import squants.time.{Milliseconds, Seconds, Time}
import squants.time.TimeConversions._

import scala.concurrent.duration.{FiniteDuration, SECONDS}

object ProgressUtil {
  private val PollPeriod = FiniteDuration(1, SECONDS)

  def fromF[F[_]: Functor: Timer](f: FiniteDuration => F[Progress]): Stream[F, Progress] =
    Stream.awakeEvery[F](PollPeriod).evalMap(f)

  def fromFOption[F[_]: Functor: Timer](f: FiniteDuration => F[Option[Progress]]): Stream[F, Progress] =
    Stream.awakeEvery[F](PollPeriod).evalMap(f).collect{case Some(p) => p}

  def fromStateT[F[_]: FlatMap: Timer, S](fs: FiniteDuration => StateT[F, S, Progress])
  : S => Stream[F, Progress] = s0 =>
    Stream.awakeEvery[F](PollPeriod).evalMapAccumulate(s0){case (st, t) => fs(t).run(st)}.map(_._2)

  def fromStateTOption[F[_]: FlatMap: Timer, S](fs: FiniteDuration => StateT[F, S, Option[Progress]])
  : S => Stream[F, Progress] = s0 =>
      Stream.awakeEvery[F](PollPeriod).evalMapAccumulate(s0){case (st, t) => fs(t).run(st)}.map(_._2)
      .collect{ case Some(p) => p }

  /**
    * Simple simulated countdown
    */
  def countdown[F[_]: Applicative: Timer](total: Time, elapsed: Time): Stream[F, Progress] =
    ProgressUtil.fromF[F] {
      t: FiniteDuration => {
          val progress = Milliseconds(t.toMillis) + elapsed
          val remaining = total - progress
          val clipped = if(remaining.value >= 0.0) remaining else Seconds(0.0)
          ObsProgress(total, RemainingTime(clipped), ObserveStage.Acquiring).pure[F].widen[Progress]
        }
    }.takeThrough(_.remaining.self.value > 0.0)

  /**
    * Simulated countdown with simulated observation stage
    */
  def obsCountdown[F[_]: Applicative: Timer](total: Time, elapsed: Time): Stream[F, Progress] =
    Stream.emit(ObsProgress(total, RemainingTime(total), ObserveStage.Preparing)) ++
      countdown[F](total, elapsed) ++
      Stream.emit(ObsProgress(total, RemainingTime(0.0.seconds), ObserveStage.ReadingOut))

  /**
    * Simulated countdown with observation stage provided by instrument
    */
  def obsCountdownWithObsStage[F[_]: Applicative: Timer](total: Time, elapsed: Time, stage: F[ObserveStage])
  : Stream[F, Progress] =
    ProgressUtil.fromF[F] {
      t: FiniteDuration => {
          val progress = Milliseconds(t.toMillis) + elapsed
          val remaining = total - progress
          val clipped = if(remaining.value >= 0.0) remaining else Seconds(0.0)
          stage.map(v => ObsProgress(total, RemainingTime(clipped), v)).widen[Progress]
        }
    }.takeThrough(x => x.remaining.self.value > 0.0 || x.stage === ObserveStage.Acquiring)
}
