package edu.gemini.seqexec.server

import java.util.logging.{Level, Logger}

import edu.gemini.spModel.config2.Config
import edu.gemini.spModel.gemini.gmos.InstGmosSouth._
import edu.gemini.spModel.seqcomp.SeqConfigNames._

import scalaz.EitherT
import scalaz.concurrent.Task

/**
 * Created by jluhrs on 4/22/15.
 */
trait Instrument extends System {
  // The name used for this instrument in the science fold configuration
  val sfName: String
  def observe(config: Config): SeqAction[ObserveResult]
}

//Placeholder for observe response
case class ObserveResult(dataId: String)

object UnknownInstrument extends Instrument {

  override val name: String = "UNKNOWN"

  override val sfName: String = "unknown"

  var imageCount = 0

  override def configure(config: Config): SeqAction[ConfigResult] = EitherT ( Task {
    TrySeq(ConfigResult(this))
  } )

  override def observe(config: Config): SeqAction[ObserveResult] = EitherT ( Task {
    imageCount += 1
    TrySeq(ObserveResult(f"S20150519S$imageCount%04d"))
  } )
}
