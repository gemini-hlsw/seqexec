// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.server

import org.log4s._

import edu.gemini.seqexec.model.dhs.ImageFileId
import GmosController.{GmosConfig, NorthTypes, SiteDependentTypes, SouthTypes}

import scalaz.EitherT
import scalaz.concurrent.Task

private class GmosControllerSim[T<:SiteDependentTypes](name: String) extends GmosController[T] {
  private val Log = getLogger

  override def getConfig: SeqAction[GmosConfig[T]] = ??? // scalastyle:ignore

  override def observe(obsid: ImageFileId): SeqAction[ImageFileId] = EitherT( Task {
    Log.info(s"Simulate taking Gmos $name observation with label " + obsid)
    Thread.sleep(5000)
    Log.info(s"Simulate Gmos $name observation completed")
    TrySeq(obsid)
  } )

  override def applyConfig(config: GmosConfig[T]): SeqAction[Unit] = EitherT( Task {
    Log.info(s"Simulate applying Gmos $name configuration ")
    TrySeq(())
  } )
}

object GmosControllerSim {
  val south: GmosController[SouthTypes] = new GmosControllerSim[SouthTypes]("South")
  val north: GmosController[NorthTypes] = new GmosControllerSim[NorthTypes]("North")
}
