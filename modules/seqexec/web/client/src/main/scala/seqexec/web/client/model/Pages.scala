// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.model

import cats._
import cats.implicits._
import gem.Observation
import seqexec.model.StepId
import seqexec.model.enum._

// Pages
object Pages {
  sealed trait SeqexecPages extends Product with Serializable

  case object Root extends SeqexecPages
  case object SoundTest extends SeqexecPages
  case object EmptyPreviewPage extends SeqexecPages
  final case class PreviewPage(instrument: Instrument, obsId: Observation.Id, step: StepId) extends SeqexecPages
  final case class PreviewConfigPage(instrument: Instrument, obsId: Observation.Id, step: StepId) extends SeqexecPages
  final case class SequencePage(instrument: Instrument, obsId: Observation.Id, step: StepId) extends SeqexecPages
  final case class SequenceConfigPage(instrument: Instrument, obsId: Observation.Id, step: StepId) extends SeqexecPages

  implicit val equal: Eq[SeqexecPages] = Eq.instance {
    case (Root, Root)                                               => true
    case (SoundTest, SoundTest)                                     => true
    case (EmptyPreviewPage, EmptyPreviewPage)                       => true
    case (SequencePage(i, o, s), SequencePage(j, p, r))             => i === j && o === p && s === r
    case (SequenceConfigPage(i, o, s), SequenceConfigPage(j, p, r)) => i === j && o === p && s === r
    case (PreviewPage(i, o, s), PreviewPage(j, p, r))               => i === j && o === p && s === r
    case (PreviewConfigPage(i, o, s), PreviewConfigPage(j, p, r))   => i === j && o === p && s === r
    case _                                                          => false
  }
}
