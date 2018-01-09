// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.server.gmos

import java.util.concurrent.atomic.AtomicBoolean

import edu.gemini.seqexec.model.dhs.ImageFileId
import edu.gemini.seqexec.server.SeqexecFailure.SeqexecException
import edu.gemini.seqexec.server.gmos.GmosController.{GmosConfig, NorthTypes, SiteDependentTypes, SouthTypes}
import edu.gemini.seqexec.server.{ObserveCommand, SeqAction, TrySeq}
import gov.aps.jca.TimeoutException
import org.log4s._
import squants.Time

import scala.annotation.tailrec
import scalaz.{EitherT, Show}
import scalaz.concurrent.Task
import scalaz.syntax.show._
import scalaz.syntax.std.boolean._

private class GmosControllerSim[T<:SiteDependentTypes](name: String) extends GmosController[T] {
  private val Log = getLogger

  implicit val configShow: Show[GmosConfig[T]] = Show.shows { config => s"(${config.cc.filter}, ${config.cc.disperser}, ${config.cc.fpu}, ${config.cc.stage}, ${config.cc.stage}, ${config.cc.dtaX}, ${config.cc.adc}, ${config.cc.useElectronicOffset}, ${config.dc.t}, ${config.dc.b}, ${config.dc.s}, ${config.dc.bi}, ${config.dc.roi.rois})" }

  override def getConfig: SeqAction[GmosConfig[T]] = ??? // scalastyle:ignore

  private val stopFlag = new AtomicBoolean(false)
  private val abortFlag = new AtomicBoolean(false)
  private val pauseFlag = new AtomicBoolean(false)

  private val tic = 200

  @tailrec
  private def observeTic(stop: Boolean, abort: Boolean, pause: Boolean, remain: Int, timeout: Option[Int]): TrySeq[ObserveCommand.Result] =
    if(remain < tic) {
      Log.debug(s"Simulate Gmos $name observation completed")
      TrySeq(ObserveCommand.Success)
    } else if(stop) TrySeq(ObserveCommand.Stopped)
      else if(abort) TrySeq(ObserveCommand.Aborted)
      else if(pause) TrySeq(ObserveCommand.Paused)
      else if(timeout.exists(_<= 0)) TrySeq.fail(SeqexecException(new TimeoutException()))
      else {
        Thread.sleep(tic.toLong)
        observeTic(stopFlag.get, abortFlag.get, pauseFlag.get, remain-tic, timeout.map(_ - tic))
      }

  override def observe(obsid: ImageFileId, expTime: Time): SeqAction[ObserveCommand.Result] = EitherT( Task {
    Log.debug(s"Simulate taking Gmos $name observation with label $obsid")
    pauseFlag.set(false)
    stopFlag.set(false)
    abortFlag.set(false)
    observeTic(false, false, false, 50000, (expTime.value > 0.0).option(expTime.toMilliseconds.toInt))
  } )

  override def applyConfig(config: GmosConfig[T]): SeqAction[Unit] = EitherT( Task {
    Log.debug(s"Simulate applying Gmos $name configuration ${config.shows}")
    TrySeq(())
  } )

  override def stopObserve: SeqAction[Unit] = EitherT( Task {
    Log.debug(s"Simulate stopping Gmos $name exposure")
    stopFlag.set(true)
    TrySeq(())
  } )

  override def abortObserve: SeqAction[Unit] = EitherT( Task {
    Log.debug(s"Simulate aborting Gmos $name exposure")
    abortFlag.set(true)
    TrySeq(())
  } )

  override def endObserve: SeqAction[Unit] = EitherT( Task {
    Log.debug("Simulate sending endObserve to Gmos")
    TrySeq(())
  } )

  override def pauseObserve: SeqAction[Unit] = EitherT( Task {
    Log.debug(s"Simulate pausing Gmos $name exposure")
    pauseFlag.set(true)
    TrySeq(())
  } )

  override def resumePaused(expTime: Time): SeqAction[ObserveCommand.Result] = EitherT( Task {
      Log.debug(s"Simulate resuming Gmos $name observation")
      pauseFlag.set(false)
      observeTic(false, false, false, 5000, (expTime.value > 0.0).option(expTime.toMilliseconds.toInt))
    } )

  override def stopPaused: SeqAction[ObserveCommand.Result] = EitherT( Task {
    Log.debug(s"Simulate stopping Gmos $name paused observation")
    pauseFlag.set(false)
    observeTic(true, false, false, 1000, None)
  } )

  override def abortPaused: SeqAction[ObserveCommand.Result] = EitherT( Task {
    Log.debug(s"Simulate aborting Gmos $name paused observation")
    pauseFlag.set(false)
    observeTic(false, true, false, 1000, None)
  } )
}

object GmosControllerSim {
  val south: GmosController[SouthTypes] = new GmosControllerSim[SouthTypes]("South")
  val north: GmosController[NorthTypes] = new GmosControllerSim[NorthTypes]("North")
}
