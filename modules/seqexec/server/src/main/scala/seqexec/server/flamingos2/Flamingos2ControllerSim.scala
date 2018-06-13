// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.flamingos2

import java.util.concurrent.atomic.AtomicInteger

import cats.data.EitherT
import cats.effect.IO
import seqexec.model.dhs.ImageFileId
import seqexec.server.SeqexecFailure.Execution
import seqexec.server.flamingos2.Flamingos2Controller.Flamingos2Config
import seqexec.server.{SeqAction, TrySeq}
import org.log4s.getLogger
import squants.Time
import cats.implicits._

object Flamingos2ControllerSim extends Flamingos2Controller {
  private val Log = getLogger

  override def getConfig: SeqAction[Flamingos2Config] = ??? // scalastyle:ignore

  override def observe(fileId: ImageFileId, expTime: Time): SeqAction[ImageFileId] = EitherT( IO {
    Thread.sleep(5000)
    TrySeq(fileId)
  } )

  override def applyConfig(config: Flamingos2Config): SeqAction[Unit] = EitherT( IO {
    Log.info(s"Applying Flamingos-2 configuration $config")
    TrySeq(())
  } )

  override def endObserve = EitherT( IO {
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

  override def observe(fileId: ImageFileId, expTime: Time): SeqAction[ImageFileId] = EitherT( IO {
    Log.info(s"Simulating Flamingos-2 observe with fileId: $fileId")
    Thread.sleep(5000)
    TrySeq(fileId)
  } )

  override def applyConfig(config: Flamingos2Config): SeqAction[Unit] = EitherT( IO {
    Log.info(s"Applying Flamingos-2 configuration $config")
    if (counter.addAndGet(1) === failAt) {
      counter.set(0)
      Log.error(s"Error applying Flamingos-2 configuration")
      TrySeq.fail(Execution("simulated error"))
    } else {
      TrySeq(())
    }
  } )

  override def endObserve = EitherT( IO {
    Log.info("Sending endObserve to Flamingos-2")
    TrySeq(())
  } )
}
