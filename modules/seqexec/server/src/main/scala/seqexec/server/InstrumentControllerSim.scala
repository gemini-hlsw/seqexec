// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import cats._
import cats.effect.Timer
import cats.effect.Sync
import cats.effect.concurrent.Ref
import cats.implicits._
import fs2.Stream
import gov.aps.jca.TimeoutException
import io.chrisdavenport.log4cats.Logger
import monocle.macros.Lenses
import mouse.all._
import seqexec.model.dhs.ImageFileId
import seqexec.model.enum.ObserveCommandResult
import seqexec.server.SeqexecFailure.SeqexecException
import seqexec.server.InstrumentSystem.ElapsedTime
import squants.time.Time
import scala.concurrent.duration._

sealed trait InstrumentControllerSim[F[_]] {
  def log(msg: => String): F[Unit]

  def observe(fileId: ImageFileId, expTime: Time): F[ObserveCommandResult]

  def applyConfig[C: Show](config: C): F[Unit]

  def stopObserve: F[Unit]

  def abortObserve: F[Unit]

  def endObserve: F[Unit]

  def pauseObserve: F[Unit]

  def resumePaused: F[ObserveCommandResult]

  def stopPaused: F[ObserveCommandResult]

  def abortPaused: F[ObserveCommandResult]

  def observeCountdown(total: Time, elapsed: ElapsedTime): Stream[F, Progress]

}

object InstrumentControllerSim {
  @Lenses
  final case class ObserveState(
    stopFlag      : Boolean,
    abortFlag     : Boolean,
    pauseFlag     : Boolean,
    remainingTime : Long
  )

  object ObserveState {
    val Zero: ObserveState = ObserveState(stopFlag = false, abortFlag = false, pauseFlag = false, remainingTime = 0)

    val pauseFalse = (o: ObserveState) => (ObserveState.pauseFlag.set(false)(o), ObserveState.pauseFlag.set(false)(o))

    def unsafeRef[F[_]: Sync]: Ref[F, ObserveState] = Ref.unsafe(Zero)

    def ref[F[_]: Sync]: F[Ref[F, ObserveState]] = Ref.of(Zero)
  }

  private[server] final class InstrumentControllerSimImpl[F[_]](
    name:               String,
    useTimeout:         Boolean,
    readOutDelay:       FiniteDuration,
    stopObserveDelay:   FiniteDuration,
    configurationDelay: FiniteDuration,
    obsStateRef: Ref[F, ObserveState]
  )(implicit val F:     MonadError[?[_], Throwable][F], L: Logger[F], T: Timer[F])
      extends InstrumentControllerSim[F] {
    private val TIC = 200L

    def log(msg: => String): F[Unit] =
      L.info(msg)

    private def tupledUpdate[A, B](f: A => B) = (x: A) => (f(x), f(x))

    private[server] def observeTic(
      timeout: Option[Long]
    ): F[ObserveCommandResult] = obsStateRef.get.flatMap { observeState =>
      if (observeState.stopFlag) {
        ObserveCommandResult.Stopped.pure[F].widen
      } else if (observeState.abortFlag) {
        ObserveCommandResult.Aborted.pure[F].widen
      } else if (observeState.pauseFlag) {
        obsStateRef.update(ObserveState.remainingTime.set(observeState.remainingTime)) *>
          ObserveCommandResult.Paused.pure[F].widen
      } else if (timeout.exists(_ <= 0)) {
        F.raiseError(SeqexecException(new TimeoutException()))
      } else if (observeState.remainingTime < TIC) {
        log(s"Simulate $name observation completed") *>
          ObserveCommandResult.Success.pure[F].widen
      } else {
        val upd = ObserveState.remainingTime.modify(_ - TIC)
        // Use flatMap to ensure we don't stack overflow
        obsStateRef.modify(tupledUpdate(upd)) *>
        T.sleep(FiniteDuration(TIC.toLong, MILLISECONDS)) *>
          observeTic(timeout.map(_ - TIC))
      }
    }

    def observe(fileId: ImageFileId, expTime: Time): F[ObserveCommandResult] = {
      val totalTime = (expTime.millis + readOutDelay.toMillis)
      log(s"Simulate taking $name observation with label $fileId") *> {
        val upd = ObserveState.stopFlag.set(false) >>>
        ObserveState.pauseFlag.set(false) >>>
        ObserveState.abortFlag.set(false) >>>
        ObserveState.remainingTime.set(totalTime)
        obsStateRef.modify(tupledUpdate(upd))
      } *> observeTic(useTimeout.option(totalTime + 2 * TIC))
    }

    def applyConfig[C: Show](config: C): F[Unit] =
      log(s"Simulate applying $name configuration ${config.show}") *>
        T.sleep(configurationDelay)

    def stopObserve: F[Unit] =
      log(s"Simulate stopping $name exposure") *>
        T.sleep(stopObserveDelay) *>
        obsStateRef.update(ObserveState.stopFlag.set(true))

    def abortObserve: F[Unit] =
      log(s"Simulate aborting $name exposure") *>
        obsStateRef.update(ObserveState.abortFlag.set(true))

    def endObserve: F[Unit] =
      log(s"Simulate sending endObserve to $name")

    def pauseObserve: F[Unit] =
      log(s"Simulate pausing $name exposure") *>
        obsStateRef.update(ObserveState.pauseFlag.set(true))

    def resumePaused: F[ObserveCommandResult] =
      log(s"Simulate resuming $name observation") *> {
        val upd = ObserveState.stopFlag.set(false) >>>
          ObserveState.pauseFlag.set(false) >>>
          ObserveState.abortFlag.set(false)
        obsStateRef.modify(tupledUpdate(upd))
      } >>= {s => observeTic(useTimeout.option(s.remainingTime + 2 * TIC))}

    def stopPaused: F[ObserveCommandResult] =
      log(s"Simulate stopping $name paused observation") *> {
        val upd = ObserveState.stopFlag.set(true) >>>
          ObserveState.pauseFlag.set(false) >>>
          ObserveState.abortFlag.set(false) >>>
          ObserveState.remainingTime.set(1000)
        obsStateRef.modify(tupledUpdate(upd))
      } *> observeTic(none)

    def abortPaused: F[ObserveCommandResult] =
      log(s"Simulate aborting $name paused observation") *> {
        val upd = ObserveState.stopFlag.set(false) >>>
          ObserveState.pauseFlag.set(false) >>>
          ObserveState.abortFlag.set(true) >>>
          ObserveState.remainingTime.set(1000)
        obsStateRef.modify(tupledUpdate(upd))
      } *> observeTic(none)

    def observeCountdown(
      total:   Time,
      elapsed: ElapsedTime
    ): Stream[F, Progress] =
      ProgressUtil.obsCountdown[F](total, elapsed.self)

  }

  def apply[F[_]: Sync: Logger: Timer](name: String): F[InstrumentControllerSim[F]] = {
    InstrumentControllerSim.ObserveState.ref[F].map { obsStateRef =>
      new InstrumentControllerSimImpl[F](name,
                                         false,
                                         FiniteDuration(5, SECONDS),
                                         FiniteDuration(1500, MILLISECONDS),
                                         FiniteDuration(5, SECONDS),
                                         obsStateRef)
    }
  }

  def unsafeWithTimes[F[_]: Sync: Logger: Timer](
    name:               String,
    readOutDelay:       FiniteDuration,
    stopObserveDelay:   FiniteDuration,
    configurationDelay: FiniteDuration
  ): InstrumentControllerSim[F] = {
    val obsStateRef = InstrumentControllerSim.ObserveState.unsafeRef[F]
    new InstrumentControllerSimImpl[F](name,
                                       true,
                                       readOutDelay,
                                       stopObserveDelay,
                                       configurationDelay,
                                       obsStateRef)
  }

}
