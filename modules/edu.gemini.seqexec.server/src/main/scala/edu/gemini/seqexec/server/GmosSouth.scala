package edu.gemini.seqexec.server

import java.util.logging.{Level, Logger}

import edu.gemini.seqexec.model.dhs.ImageFileId
import edu.gemini.spModel.config2.Config
import edu.gemini.spModel.gemini.gmos.InstGmosSouth.INSTRUMENT_NAME_PROP
import edu.gemini.spModel.seqcomp.SeqConfigNames.INSTRUMENT_KEY

import scalaz.{EitherT, Reader}
import scalaz.concurrent.Task

final case class GmosSouth(controller: GmosSouthController) extends Instrument {
  import GmosSouth._

  override val name: String = INSTRUMENT_NAME_PROP

  override val sfName: String = "gmos"

  override val contributorName = "gmos"
  val dhsInstrumentName = "GMOS-S"

  val Log = Logger.getLogger(getClass.getName)

  override def observe(config: Config): SeqObserve[ImageFileId, ObserveResult] = Reader {
    fileId => controller.observe(fileId).map(_ => ObserveResult(fileId))
  }

  override def configure(config: Config): SeqAction[ConfigResult] =
    fromSequenceConfig(config).flatMap(controller.applyConfig).map(_ => ConfigResult(this))
}

object GmosSouth {
  import GmosSouthController._

  def fromSequenceConfig(config: Config): SeqAction[GmosSouthConfig] = ???
}
