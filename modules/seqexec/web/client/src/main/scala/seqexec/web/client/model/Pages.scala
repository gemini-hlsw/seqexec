// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.model

import cats._
import cats.implicits._
import diode.Action
import gem.Observation
import japgolly.scalajs.react.extra.router._
import japgolly.scalajs.react.Callback
import seqexec.model.StepId
import seqexec.model.enum._
import seqexec.web.client.actions._
import seqexec.web.client.circuit.SeqexecCircuit
import monocle.Prism

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

  // Pages forms a prism with Page
  val PageActionP: Prism[Action, SeqexecPages] = Prism[Action, SeqexecPages]{
    case SelectRoot => Root.some
    case RequestSoundEcho => SoundTest.some
    case SelectEmptyPreview => EmptyPreviewPage.some
    case SelectSequencePreview(i, id, step) => PreviewPage(i, id, step).some
    case ShowPreviewStepConfig(i, id, step) => PreviewConfigPage(i, id, step).some
    case SelectIdToDisplay(i, id, step) => SequencePage(i, id, step).some
    case ShowStepConfig(i, id, step) => SequenceConfigPage(i, id, step).some
  }{
    case Root => SelectRoot
    case SoundTest => RequestSoundEcho
    case EmptyPreviewPage => SelectEmptyPreview
    case PreviewPage(i, id, step) => SelectSequencePreview(i, id, step)
    case PreviewConfigPage(i, id, step) => ShowPreviewStepConfig(i, id, step)
    case SequencePage(i, id, step) => SelectIdToDisplay(i, id, step)
    case SequenceConfigPage(i, id, step) => ShowStepConfig(i, id, step)
  }

  /**
   * Extensions methods for RouterCtl
   */
  implicit class RouterCtlOps(val r: RouterCtl[SeqexecPages]) extends AnyVal {
    /**
     * Some pages are linked to actions. This methods lets you set the url
     * and dispatch an action at the same time
     */
    def setUrlAndDispatchCB(b: SeqexecPages): Callback =
      r.set(b) *> SeqexecCircuit.dispatchCB(PageActionP.reverseGet(b))

    /**
     * Some actions are linked to a page. This methods lets you dispatch and action
     * and set the url
     */
    def dispatchAndSetUrlCB(b: Action): Callback =
      PageActionP.getOption(b).map(r.set).getOrEmpty *> SeqexecCircuit.dispatchCB(b)

  }
}
