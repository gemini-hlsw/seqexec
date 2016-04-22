package edu.gemini.seqexec.server

import edu.gemini.seqexec.model.dhs.ObsId
import edu.gemini.spModel.config2.Config

import scalaz.{EitherT, Reader}
import scalaz.concurrent.Task

trait Instrument extends System {
  // The name used for this instrument in the science fold configuration
  val sfName: String
  def observe(config: Config): SeqObserve[DhsClient, ObserveResult]
}

//Placeholder for observe response
case class ObserveResult(dataId: ObsId)

object UnknownInstrument extends Instrument {

  override val name: String = "UNKNOWN"

  override val sfName: String = "unknown"

  var imageCount = 0

  override def configure(config: Config): SeqAction[ConfigResult] = EitherT ( Task {
    TrySeq(ConfigResult(this))
  } )

  override def observe(config: Config): SeqObserve[DhsClient, ObserveResult] = Reader { _ =>
    EitherT(Task {
      imageCount += 1
      TrySeq(ObserveResult(f"S20150519S$imageCount%04d"))
    })
  }
}
