// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.server.gmos

import java.util.concurrent.atomic.AtomicBoolean

import edu.gemini.seqexec.model.dhs.ImageFileId
import edu.gemini.seqexec.server.gmos.GmosController.{GmosConfig, NorthTypes, SiteDependentTypes, SouthTypes}
import edu.gemini.seqexec.server.{ObserveCommand, SeqAction, TrySeq}
import org.log4s._

import scala.annotation.tailrec
import scalaz.{Show, EitherT}
import scalaz.concurrent.Task
import scalaz.syntax.show._

private class GmosControllerSim[T<:SiteDependentTypes](name: String) extends GmosController[T] {
  private val Log = getLogger

  implicit val configShow: Show[GmosConfig[T]] = Show.shows { config => s"(${config.cc.filter}, ${config.cc.disperser}, ${config.cc.fpu}, ${config.cc.stage}, ${config.cc.stage}, ${config.cc.dtaX}, ${config.cc.adc}, ${config.cc.useElectronicOffset}, ${config.dc.t}, ${config.dc.b}, ${config.dc.s}, ${config.dc.bi}, ${config.dc.roi.rois})" }

  override def getConfig: SeqAction[GmosConfig[T]] = ??? // scalastyle:ignore

  private val stopFlag = new AtomicBoolean(false)
  private val abortFlag = new AtomicBoolean(false)

  private val tic = 200

  @tailrec
  private def observeTic(obsid: ImageFileId, stop: Boolean, abort: Boolean, remain: Int): TrySeq[ObserveCommand.Result] =
    if(remain < tic) {
      Log.debug(s"Simulate Gmos $name observation completed")
      TrySeq(ObserveCommand.Success)
    } else if(stop) TrySeq(ObserveCommand.Stopped)
      else if(abort) TrySeq(ObserveCommand.Aborted)
      else {
        Thread.sleep(tic.toLong)
        observeTic(obsid, stopFlag.get, abortFlag.get, remain-tic)
      }

  override def observe(obsid: ImageFileId): SeqAction[ObserveCommand.Result] = EitherT( Task {
    Log.debug(s"Simulate taking Gmos $name observation with label $obsid")
    stopFlag.set(false)
    abortFlag.set(false)
    observeTic(obsid, false, false, 5000)
  })

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
}

object GmosControllerSim {
  val south: GmosController[SouthTypes] = new GmosControllerSim[SouthTypes]("South")
  val north: GmosController[NorthTypes] = new GmosControllerSim[NorthTypes]("North")
}
