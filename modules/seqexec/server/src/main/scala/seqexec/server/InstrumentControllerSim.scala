// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger}

import cats.Show
import cats.Monad
import cats.ApplicativeError
import cats.data.EitherT
import cats.effect.{ IO, Timer }
import cats.implicits._
import fs2.Stream
import seqexec.model.dhs.ImageFileId
import seqexec.server.SeqexecFailure.SeqexecException
import gov.aps.jca.TimeoutException
import mouse.all._
import org.log4s.getLogger
import scala.concurrent.ExecutionContext
import seqexec.server.InstrumentSystem.ElapsedTime
import squants.time.{Seconds, Time}

import scala.annotation.tailrec

class InstrumentControllerSim(name: String, useTimeout: Boolean) {
  private val Log = getLogger

  private val stopFlag = new AtomicBoolean(false)
  private val abortFlag = new AtomicBoolean(false)
  private val pauseFlag = new AtomicBoolean(false)
  private val remainingTime = new AtomicInteger(0)

  private val tic = 200

  private val ReadoutDelay = Seconds(5)
  private val ConfigurationDelay = Seconds(5)

  @tailrec
  private def observeTic(stop: Boolean, abort: Boolean, pause: Boolean, remain: Int, timeout: Option[Int]): TrySeq[ObserveCommand.Result] =
    if(remain < tic) {
      Log.info(s"Simulate $name observation completed")
      TrySeq(ObserveCommand.Success)
    } else if(stop) TrySeq(ObserveCommand.Stopped)
      else if(abort) TrySeq(ObserveCommand.Aborted)
      else if(pause) {
        remainingTime.set(remain)
        TrySeq(ObserveCommand.Paused)
      }
      else if(timeout.exists(_<= 0)) TrySeq.fail(SeqexecException(new TimeoutException()))
      else {
        Thread.sleep(tic.toLong)
        observeTic(stopFlag.get, abortFlag.get, pauseFlag.get, remain - tic, timeout.map(_ - tic))
      }

  def observe(fileId: ImageFileId, expTime: Time): SeqAction[ObserveCommand.Result] = EitherT( IO {
    Log.info(s"Simulate taking $name observation with label $fileId")
    pauseFlag.set(false)
    stopFlag.set(false)
    abortFlag.set(false)
    val totalTime = (expTime + ReadoutDelay).toMilliseconds.toInt
    remainingTime.set(totalTime)
    observeTic(stop = false, abort = false, pause = false, totalTime, useTimeout.option(totalTime + 2 * tic))
  } )

  def applyConfig[C: Show](config: C): SeqAction[Unit] = EitherT( IO {
    Log.info(s"Simulate applying $name configuration ${config.show}")
    Thread.sleep(ConfigurationDelay.toMilliseconds.toLong)
    TrySeq(())
  } )

  def stopObserve: SeqAction[Unit] = EitherT( IO {
    Log.info(s"Simulate stopping $name exposure")
    stopFlag.set(true)
    TrySeq(())
  } )

  def abortObserve: SeqAction[Unit] = EitherT( IO {
    Log.info(s"Simulate aborting $name exposure")
    abortFlag.set(true)
    TrySeq(())
  } )

  def endObserve: SeqAction[Unit] = EitherT( IO {
    Log.info(s"Simulate sending endObserve to $name")
    TrySeq(())
  } )

  def pauseObserve: SeqAction[Unit] = EitherT( IO {
    Log.info(s"Simulate pausing $name exposure")
    pauseFlag.set(true)
    TrySeq(())
  } )

  def resumePaused: SeqAction[ObserveCommand.Result] = EitherT( IO {
    Log.info(s"Simulate resuming $name observation")
    pauseFlag.set(false)
    observeTic(stop = false, abort = false, pause = false, remainingTime.get,
      useTimeout.option(remainingTime.get + 2 * tic))
  } )

  def stopPaused: SeqAction[ObserveCommand.Result] = EitherT( IO {
    Log.info(s"Simulate stopping $name paused observation")
    pauseFlag.set(false)
    observeTic(stop = true, abort = false, pause = false, 1000, None)
  } )

  def abortPaused: SeqAction[ObserveCommand.Result] = EitherT( IO {
    Log.info(s"Simulate aborting $name paused observation")
    pauseFlag.set(false)
    observeTic(stop = false, abort = true, pause = false, 1000, None)
  } )

  def observeCountdown(total: Time, elapsed: ElapsedTime): Stream[IO, Progress] = {
    implicit val ioTimer: Timer[IO] = IO.timer(ExecutionContext.global)
    ProgressUtil.countdown[IO](total, elapsed.self)
  }

}

object InstrumentControllerSim {
  def apply(name: String): InstrumentControllerSim = new InstrumentControllerSim(name, false)
  def withTimeout(name: String): InstrumentControllerSim = new InstrumentControllerSim(name, true)

  implicit class SimulationExceptionOps[F[_]: Monad, A](s: EitherT[F, SeqexecFailure, A]) {
    def orSimulationError(implicit ev: ApplicativeError[F, SeqexecFailure]): F[A] =
      s.getOrElseF(ev.raiseError(SeqexecFailure.FailedSimulation))
  }

}
