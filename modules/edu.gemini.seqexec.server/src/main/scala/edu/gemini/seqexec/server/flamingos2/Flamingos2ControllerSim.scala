// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.server.flamingos2

import edu.gemini.seqexec.model.dhs.ImageFileId
import edu.gemini.seqexec.server.SeqexecFailure.Execution
import edu.gemini.seqexec.server.flamingos2.Flamingos2Controller.Flamingos2Config
import edu.gemini.seqexec.server.{SeqAction, TrySeq}
import org.log4s.getLogger

import scalaz.EitherT
import scalaz.concurrent.Task

object Flamingos2ControllerSim extends Flamingos2Controller {
  private val Log = getLogger

  override def getConfig: SeqAction[Flamingos2Config] = ??? // scalastyle:ignore

  override def observe(obsid: ImageFileId): SeqAction[ImageFileId] = EitherT( Task {
    Log.info("Taking Flamingos-2 observation with label " + obsid)
    Thread.sleep(5000)
    Log.info("Flamingos-2 observation completed")
    TrySeq(obsid)
  } )

  override def applyConfig(config: Flamingos2Config): SeqAction[Unit] = EitherT( Task {
    Log.info(s"Applying Flamingos-2 configuration $config")
    TrySeq(())
  } )

  override def endObserve = EitherT( Task {
    Log.info("Sending endObserve to Flamingos-2")
    TrySeq(())
  } )
}

object Flamingos2ControllerSimBad extends Flamingos2Controller {
  private val Log = getLogger

  override def getConfig: SeqAction[Flamingos2Config] = ??? // scalastyle:ignore

  override def observe(obsid: ImageFileId): SeqAction[ImageFileId] = EitherT( Task {
    Log.info("Taking Flamingos-2 observation with label " + obsid)
    Thread.sleep(5000)
    Log.info("Flamingos-2 observation completed")
    TrySeq(obsid)
  } )

  override def applyConfig(config: Flamingos2Config): SeqAction[Unit] = EitherT( Task {
    Log.info(s"Applying Flamingos-2 configuration $config")
    TrySeq.fail(Execution("simulated error"))
  } )

  override def endObserve = EitherT( Task {
    Log.info("Sending endObserve to Flamingos-2")
    TrySeq(())
  } )
}
