// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.server

import java.util.logging.Logger

import edu.gemini.seqexec.model.dhs.ImageFileId
import edu.gemini.seqexec.server.Flamingos2Controller.Flamingos2Config
import edu.gemini.seqexec.server.SeqexecFailure.Execution

import scalaz.EitherT
import scalaz.concurrent.Task

object Flamingos2ControllerSim extends Flamingos2Controller {
  private val Log = Logger.getLogger(getClass.getName)

  override def getConfig: SeqAction[Flamingos2Config] = ??? // scalastyle:ignore

  override def observe(obsid: ImageFileId): SeqAction[ImageFileId] = EitherT( Task {
    Log.info("Taking Flamingos-2 observation with label " + obsid)
    Thread.sleep(5000)
    Log.info("Flamingos-2 observation completed")
    TrySeq(obsid)
  } )

  override def applyConfig(config: Flamingos2Config): SeqAction[Unit] = EitherT( Task {
    Log.info("Applying Flamingos-2 configuration " + config)
    TrySeq(())
  } )
}

object Flamingos2ControllerSimBad extends Flamingos2Controller {
  private val Log = Logger.getLogger(getClass.getName)

  override def getConfig: SeqAction[Flamingos2Config] = ??? // scalastyle:ignore

  override def observe(obsid: ImageFileId): SeqAction[ImageFileId] = EitherT( Task {
    Log.info("Taking Flamingos-2 observation with label " + obsid)
    Thread.sleep(5000)
    Log.info("Flamingos-2 observation completed")
    TrySeq(obsid)
  } )

  override def applyConfig(config: Flamingos2Config): SeqAction[Unit] = EitherT( Task {
    Log.info("Applying Flamingos-2 configuration " + config)
    TrySeq.fail(Execution("simulated error"))
  } )
}
