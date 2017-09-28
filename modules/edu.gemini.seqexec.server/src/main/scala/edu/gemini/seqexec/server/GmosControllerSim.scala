// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.server

import java.util.concurrent.atomic.AtomicBoolean

import org.log4s._
import edu.gemini.seqexec.model.dhs.ImageFileId
import GmosController.{GmosConfig, NorthTypes, SiteDependentTypes, SouthTypes}

import scalaz.{EitherT, \/}
import scalaz.Scalaz._
import scalaz.concurrent.Task

private class GmosControllerSim[T<:SiteDependentTypes](name: String) extends GmosController[T] {
  private val Log = getLogger

  override def getConfig: SeqAction[GmosConfig[T]] = ??? // scalastyle:ignore

  private val stopFlag = new AtomicBoolean(false)
  private val abortFlag = new AtomicBoolean(false)

  private val tic = 200
  private def observeTic(obsid: ImageFileId, remain: Int): Task[\/[SeqexecFailure, ImageFileId]] =
    if(remain < tic) Task {
      Log.info(s"Simulate Gmos $name observation completed")
      TrySeq(obsid)
    }
    else Task.delay(stopFlag.get).flatMap( stop => Task.delay(abortFlag.get).flatMap( abort =>
      if(stop) Task(TrySeq.fail(SeqexecFailure.Execution("Exposure stopped by user.")))
      else if(abort) Task(TrySeq.fail(SeqexecFailure.Execution("Exposure aborted by user.")))
      else Task{ Thread.sleep(tic.toLong)} *> observeTic(obsid, remain-tic)))

  override def observe(obsid: ImageFileId): SeqAction[ImageFileId] = EitherT( Task {
    Log.info(s"Simulate taking Gmos $name observation with label " + obsid)
    stopFlag.set(false)
    abortFlag.set(false)
  } *> observeTic(obsid, 5000) )

  override def applyConfig(config: GmosConfig[T]): SeqAction[Unit] = EitherT( Task {
    Log.info(s"Simulate applying Gmos $name configuration")
    TrySeq(())
  } )

  override def stopObserve: SeqAction[Unit] = EitherT( Task {
    Log.info(s"Simulate stopping Gmos $name exposure")
    stopFlag.set(true)
    TrySeq(())
  } )
}

object GmosControllerSim {
  val south: GmosController[SouthTypes] = new GmosControllerSim[SouthTypes]("South")
  val north: GmosController[NorthTypes] = new GmosControllerSim[NorthTypes]("North")
}
