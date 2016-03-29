package edu.gemini.seqexec.server

import java.util.logging.Logger

import edu.gemini.seqexec.server.DhsClient.ObsId
import edu.gemini.seqexec.server.Flamingos2Controller.Flamingos2Config

import scalaz.EitherT
import scalaz.concurrent.Task

/**
 * Created by jluhrs on 11/24/15.
 */
object Flamingos2ControllerSim extends Flamingos2Controller {
  private val Log = Logger.getLogger(getClass.getName)

  override def getConfig: SeqAction[Flamingos2Config] = ???

  override def observe(obsid: ObsId): SeqAction[ObsId] = EitherT( Task {
    Log.info("Taking Flamingos-2 observation with label " + obsid)
    TrySeq(obsid)
  } )

  override def applyConfig(config: Flamingos2Config): SeqAction[Unit] = EitherT( Task {
    Log.info("Applying Flamingos-2 configuration " + config)
    TrySeq(())
  } )
}
