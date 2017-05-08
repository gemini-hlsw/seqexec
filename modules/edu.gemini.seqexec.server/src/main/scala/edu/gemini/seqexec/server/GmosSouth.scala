package edu.gemini.seqexec.server

import java.util.logging.{Level, Logger}

import edu.gemini.seqexec.model.dhs.ImageFileId
import edu.gemini.spModel.config2.Config
import edu.gemini.spModel.gemini.gmos.InstGmosSouth.INSTRUMENT_NAME_PROP
import edu.gemini.spModel.seqcomp.SeqConfigNames.INSTRUMENT_KEY

import scalaz.{EitherT, Reader}
import scalaz.concurrent.Task

final case class GmosSouth() extends Instrument {

  override val name: String = INSTRUMENT_NAME_PROP

  override val sfName: String = "gmos"

  val Log = Logger.getLogger(getClass.getName)

  override def observe(config: Config): SeqObserve[ImageFileId, ObserveResult] = ???

  override def configure(config: Config): SeqAction[ConfigResult] = EitherT(Task {
    val items = config.getAll(INSTRUMENT_KEY).itemEntries()

    //    Log.log(Level.INFO, "Configuring " + name + " with :" + ItemEntryUtil.showItems(items))
    Log.log(Level.INFO, "Configuring " + name)
    Thread.sleep(2000)
    Log.log(Level.INFO, name + " configured")

    TrySeq(ConfigResult(this))
  })

  override val contributorName: String = "gmos"
  override val dhsInstrumentName: String = "GMOS-S"
}

object GmosSouth {

}
