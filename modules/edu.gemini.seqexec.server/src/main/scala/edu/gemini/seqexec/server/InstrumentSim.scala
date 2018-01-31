// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.server

import java.util.concurrent.atomic.AtomicBoolean

import edu.gemini.seqexec.model.dhs.ImageFileId
import edu.gemini.seqexec.server.SeqexecFailure.SeqexecException
import gov.aps.jca.TimeoutException
import org.log4s.getLogger
import squants.Time

import scala.annotation.tailrec
import scalaz.{EitherT, Show}
import scalaz.concurrent.Task
import scalaz.syntax.show._
import scalaz.syntax.std.boolean._

class InstrumentSim(name: String) {
  private val Log = getLogger

  private val stopFlag = new AtomicBoolean(false)
  private val abortFlag = new AtomicBoolean(false)
  private val pauseFlag = new AtomicBoolean(false)

  private val tic = 200

  @tailrec
  private def observeTic(stop: Boolean, abort: Boolean, pause: Boolean, remain: Int, timeout: Option[Int]): TrySeq[ObserveCommand.Result] =
    if(remain < tic) {
      Log.info(s"Simulate $name observation completed")
      TrySeq(ObserveCommand.Success)
    } else if(stop) TrySeq(ObserveCommand.Stopped)
      else if(abort) TrySeq(ObserveCommand.Aborted)
      else if(pause) TrySeq(ObserveCommand.Paused)
      else if(timeout.exists(_<= 0)) TrySeq.fail(SeqexecException(new TimeoutException()))
      else {
        Thread.sleep(tic.toLong)
        observeTic(stopFlag.get, abortFlag.get, pauseFlag.get, remain-tic, timeout.map(_ - tic))
      }

  def observe(obsid: ImageFileId, expTime: Time): SeqAction[ObserveCommand.Result] = EitherT( Task {
    Log.info(s"Simulate taking $name observation with label $obsid")
    pauseFlag.set(false)
    stopFlag.set(false)
    abortFlag.set(false)
    observeTic(false, false, false, 7000, (expTime.value > 0.0).option(expTime.toMilliseconds.toInt))
  } )

  def applyConfig[C: Show](config: C): SeqAction[Unit] = EitherT( Task {
    Log.info(s"Simulate applying $name configuration ${config.shows}")
    Thread.sleep(1000)
    TrySeq(())
  } )

  def stopObserve: SeqAction[Unit] = EitherT( Task {
    Log.info(s"Simulate stopping $name exposure")
    stopFlag.set(true)
    TrySeq(())
  } )

  def abortObserve: SeqAction[Unit] = EitherT( Task {
    Log.info(s"Simulate aborting $name exposure")
    abortFlag.set(true)
    TrySeq(())
  } )

  def endObserve: SeqAction[Unit] = EitherT( Task {
    Log.info(s"Simulate sending endObserve to $name")
    TrySeq(())
  } )

  def pauseObserve: SeqAction[Unit] = EitherT( Task {
    Log.info(s"Simulate pausing $name exposure")
    pauseFlag.set(true)
    TrySeq(())
  } )

  def resumePaused(expTime: Time): SeqAction[ObserveCommand.Result] = EitherT( Task {
    Log.info(s"Simulate resuming $name observation")
    pauseFlag.set(false)
    observeTic(false, false, false, 5000, (expTime.value > 0.0).option(expTime.toMilliseconds.toInt))
  } )

  def stopPaused: SeqAction[ObserveCommand.Result] = EitherT( Task {
    Log.info(s"Simulate stopping $name paused observation")
    pauseFlag.set(false)
    observeTic(true, false, false, 1000, None)
  } )

  def abortPaused: SeqAction[ObserveCommand.Result] = EitherT( Task {
    Log.info(s"Simulate aborting $name paused observation")
    pauseFlag.set(false)
    observeTic(false, true, false, 1000, None)
  } )

}

object InstrumentSim {
  def apply(name: String): InstrumentSim = new InstrumentSim(name)
}
