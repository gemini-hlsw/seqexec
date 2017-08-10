package edu.gemini.seqexec.server

import java.util.logging.Logger

import edu.gemini.seqexec.model.dhs.ImageFileId
import GmosController.{GmosConfig, NorthTypes, SiteDependentTypes, SouthTypes}

import scalaz.EitherT
import scalaz.concurrent.Task

private class GmosControllerSim[T<:SiteDependentTypes](name: String) extends GmosController[T] {
  private val Log = Logger.getLogger(getClass.getName)

  override def getConfig: SeqAction[GmosConfig[T]] = ???

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

