// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import cats.Applicative
import cats.data.StateT
import cats.effect.Effect
import fs2.{Scheduler, Stream}
import squants.time.{Milliseconds, Seconds, Time}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.{FiniteDuration, SECONDS}

object ProgressUtil {
  private val PollPeriod = FiniteDuration(1, SECONDS)

  def fromF[F[_]: Effect](f: FiniteDuration => F[Progress]): Stream[F, Progress] =
    Scheduler[F](1).flatMap(sch => sch.awakeEvery(PollPeriod).evalMap(f))

  def fromFOption[F[_]: Effect](f: FiniteDuration => F[Option[Progress]]): Stream[F, Progress] = Scheduler[F](1)
    .flatMap(sch => sch.awakeEvery(PollPeriod).evalMap(f).collect{case Some(p) => p})

  def fromStateT[F[_]: Effect, S](fs: FiniteDuration => StateT[F, S, Progress])
  : S => Stream[F, Progress] = s0 =>
    Scheduler[F](1).flatMap(sch =>
      sch.awakeEvery(PollPeriod).evalMapAccumulate(s0){case (st, t) => fs(t).run(st)}.map(_._2))

  def fromStateTOption[F[_]: Effect, S](fs: FiniteDuration => StateT[F, S, Option[Progress]])
  : S => Stream[F, Progress] = s0 =>
    Scheduler[F](1).flatMap(sch =>
      sch.awakeEvery(PollPeriod).evalMapAccumulate(s0){case (st, t) => fs(t).run(st)}.map(_._2))
      .collect{ case Some(p) => p }

  def countdown[F[_]: Effect](total: Time, elapsed: Time): Stream[F, Progress] =
    ProgressUtil.fromF[F] {
      t: FiniteDuration => {
          val progress = Milliseconds(t.toMillis) + elapsed
          val remaining = total - progress
          val clipped = if(remaining.value >= 0.0) remaining else Seconds(0.0)
          Applicative[F].pure(Progress(total, RemainingTime(clipped)))
        }
    }.takeThrough(_.remaining.self.value > 0.0)
}
