// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.server.flamingos2

import java.util.concurrent.atomic.AtomicInteger

import edu.gemini.seqexec.model.dhs.ImageFileId
import edu.gemini.seqexec.server.SeqexecFailure.Execution
import edu.gemini.seqexec.server.flamingos2.Flamingos2Controller.Flamingos2Config
import edu.gemini.seqexec.server.{SeqAction, TrySeq}
import org.log4s.getLogger

import scalaz.EitherT
import scalaz.concurrent.Task
import scalaz.syntax.equal._
import scalaz.std.anyVal._
import squants.Time

object Flamingos2ControllerSim extends Flamingos2Controller {
  private val Log = getLogger

  override def getConfig: SeqAction[Flamingos2Config] = ??? // scalastyle:ignore

  override def observe(obsid: ImageFileId, expTime: Time): SeqAction[ImageFileId] = EitherT( Task {
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

/**
 * This controller will run correctly but fail at step `failAt`
 */
final case class Flamingos2ControllerSimBad(failAt: Int) extends Flamingos2Controller {
  private val Log = getLogger

  override def getConfig: SeqAction[Flamingos2Config] = ??? // scalastyle:ignore

  private val counter: AtomicInteger = new AtomicInteger(0)

  override def observe(obsid: ImageFileId, expTime: Time): SeqAction[ImageFileId] = EitherT( Task {
    Log.info("Taking Flamingos-2 observation with label " + obsid)
    Thread.sleep(5000)
    Log.info("Flamingos-2 observation completed")
    TrySeq(obsid)
  } )

  override def applyConfig(config: Flamingos2Config): SeqAction[Unit] = EitherT( Task {
    Log.info(s"Applying Flamingos-2 configuration $config")
    if (counter.addAndGet(1) === failAt) {
      counter.set(0)
      Log.error(s"Error applying Flamingos-2 configuration")
      TrySeq.fail(Execution("simulated error"))
    } else {
      TrySeq(())
    }
  } )

  override def endObserve = EitherT( Task {
    Log.info("Sending endObserve to Flamingos-2")
    TrySeq(())
  } )
}
