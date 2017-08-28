// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.server

import edu.gemini.seqexec.model.dhs.ImageFileId
import edu.gemini.spModel.config2.Config

import scalaz.{EitherT, Reader}
import scalaz.concurrent.Task

trait InstrumentSystem extends System {
  // The name used for this instrument in the science fold configuration
  val sfName: String
  val contributorName: String
  val dhsInstrumentName: String
  def observe(config: Config): SeqObserve[ImageFileId, ObserveResult]
}

//Placeholder for observe response
case class ObserveResult(dataId: ImageFileId)

object UnknownInstrument extends InstrumentSystem {

  override val name: String = "UNKNOWN"

  override val sfName: String = "unknown"

  override val contributorName: String = "unknown"
  override val dhsInstrumentName: String = "UNKNOWN"

  private var imageCount = 0 // scalastyle:ignore

  override def configure(config: Config): SeqAction[ConfigResult] = EitherT ( Task {
    TrySeq(ConfigResult(this))
  } )

  override def observe(config: Config): SeqObserve[ImageFileId, ObserveResult] = Reader { _ =>
    EitherT(Task {
      imageCount += 1
      TrySeq(ObserveResult(f"S20150519S$imageCount%04d"))
    })
  }

}
