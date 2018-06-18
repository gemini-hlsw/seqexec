// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gpi

import cats.data.EitherT
import cats.effect.IO
import seqexec.model.dhs.ImageFileId
import seqexec.server.gpi.GPIController.GPIConfig
import seqexec.server.{SeqAction, TrySeq}
import org.log4s.getLogger
import squants.Time

/**
 * Simulator for GPI
 */
object GPIControllerSim {
  private val Log = getLogger

  def observe(fileId: ImageFileId, expTime: Time): SeqAction[ImageFileId] = EitherT( IO {
    Thread.sleep(5000)
    println(expTime)
    TrySeq(fileId)
  } )

  def applyConfig(config: GPIConfig): SeqAction[Unit] = EitherT( IO {
    Log.info(s"Applying GPI configuration $config")
    TrySeq(())
  } )

  // def endObserve = EitherT( IO {
  //   Log.info("Sending endObserve to GPI")
  //   TrySeq(())
  // } )
}
