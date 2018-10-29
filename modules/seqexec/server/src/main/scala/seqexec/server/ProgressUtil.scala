// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import cats.Applicative
import cats.data.StateT
import cats.effect.Effect
import fs2.{Scheduler, Stream}
import squants.time.{Milliseconds, Time}
import scala.concurrent.ExecutionContext.Implicits.global

import scala.concurrent.duration.{FiniteDuration, SECONDS}

object ProgressUtil {
  private val PollPeriod = FiniteDuration(1, SECONDS)

  def fromF[F[_]: Effect](f: F[Progress]): Stream[F, Progress] = Scheduler[F](1).flatMap(
    sch => sch.awakeEvery(PollPeriod).evalMap(_ => f))

  def fromFOption[F[_]: Effect](f: F[Option[Progress]]): Stream[F, Progress] = Scheduler[F](1)
    .flatMap(sch => sch.awakeEvery(PollPeriod).evalMap(_ => f).collect{case Some(p) => p})

  def fromStateT[F[_]: Effect, S](s: StateT[F, S, Progress]): S => Stream[F, Progress] = s0 =>
    Scheduler[F](1).flatMap(sch =>
      sch.awakeEvery(PollPeriod).evalMapAccumulate(s0){case (st, _) => s.run(st)}.map(_._2))

  def fromStateTParam[F[_]: Effect, S](fs: FiniteDuration => StateT[F, S, Progress])
  : S => Stream[F, Progress] = s0 =>
    Scheduler[F](1).flatMap(sch =>
      sch.awakeEvery(PollPeriod).evalMapAccumulate(s0){case (st, t) => fs(t).run(st)}.map(_._2))

  def countdown[F[_]: Effect: Applicative](total: Time, elapsed: Time): Stream[F, Progress] = {
    val r = ProgressUtil.fromStateTParam[F, Time] {
      t: FiniteDuration =>
        StateT[F, Time, Progress](st => {
          val progress = Milliseconds(t.toMillis) + st
          Applicative[F].pure((progress, Progress(total, RemainingTime(total - progress))))
        })
    }
    r(elapsed)
  }

}
