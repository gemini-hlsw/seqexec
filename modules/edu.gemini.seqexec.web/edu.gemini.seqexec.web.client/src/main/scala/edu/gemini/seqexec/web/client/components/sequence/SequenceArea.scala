// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.web.client.components.sequence

import diode.react.ModelProxy
import edu.gemini.seqexec.web.client.components.sequence.toolbars.{SequenceDefaultToolbar, StepConfigToolbar, SequenceAnonymousToolbar}
import edu.gemini.seqexec.web.client.circuit.{SeqexecCircuit, StatusAndStepFocus, InstrumentTabContentFocus}
import edu.gemini.seqexec.web.client.model.Pages.SeqexecPages
import edu.gemini.seqexec.web.client.model.{SectionOpen, SectionClosed}
import edu.gemini.seqexec.web.client.semanticui._
import edu.gemini.seqexec.web.client.semanticui.elements.message.IconMessage
import edu.gemini.seqexec.web.client.semanticui.elements.icon.Icon.IconInbox
import edu.gemini.seqexec.web.client.components.SeqexecStyles
import edu.gemini.seqexec.web.client.components.sequence.steps.StepsTable
import edu.gemini.seqexec.model.Model.SeqexecSite
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{CallbackTo, ScalaComponent, ScalazReact}
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.extra.router.RouterCtl
import japgolly.scalajs.react.ScalazReact._
import scalacss.ScalaCssReact._
import cats.implicits._

object SequenceStepsTableContainer {
  final case class Props(router: RouterCtl[SeqexecPages], site: SeqexecSite, p: ModelProxy[StatusAndStepFocus]) {
    protected[sequence] val sequenceControlConnects = site.instruments.toList.map(i => (i, SeqexecCircuit.connect(SeqexecCircuit.sequenceControlReader(i)))).toMap
    private[sequence] val instrumentConnects = site.instruments.toList.map(i => (i, SeqexecCircuit.connect(SeqexecCircuit.stepsTableReader(i)))).toMap
    protected[sequence] val sequenceObserverConnects = site.instruments.toList.map(i => (i, SeqexecCircuit.connect(SeqexecCircuit.sequenceObserverReader(i)))).toMap
  }
  final case class State(nextStepToRun: Int)

  private val ST = ReactS.Fix[State]

  def updateStepToRun(step: Int): ScalazReact.ReactST[CallbackTo, State, Unit] =
    ST.set(State(step)).liftCB

  def toolbar(p: Props): VdomElement =
    p.p().stepConfigDisplayed.fold{
      <.div(
        SequenceDefaultToolbar(p).when(p.p().isLogged),
        SequenceAnonymousToolbar(p.site, p.p().instrument).unless(p.p().isLogged)
      ): VdomElement
    }(s => StepConfigToolbar(StepConfigToolbar.Props(p.router, p.site, p.p().instrument, p.p().id, s)))

  private val component = ScalaComponent.builder[Props]("SequenceStepsTableContainer")
    .initialState(State(0))
    .renderP { ($, p) =>
      <.div(
        ^.height := "100%",
        toolbar(p),
        p.instrumentConnects.get(p.p().instrument).whenDefined(x => x(m =>
            StepsTable(StepsTable.Props(p.router, m, x => $.runState(updateStepToRun(x))))))
      )
    }.build

  def apply(router: RouterCtl[SeqexecPages], site: SeqexecSite, p: ModelProxy[StatusAndStepFocus]): Unmounted[Props, State, Unit] =
    component(Props(router, site, p))
}

/**
* Content of a single tab with a sequence
*/
object SequenceTabContent {

  final case class Props(router: RouterCtl[SeqexecPages], site: SeqexecSite, p: ModelProxy[InstrumentTabContentFocus]) {
    protected[sequence] val connect = SeqexecCircuit.connect(SeqexecCircuit.statusAndStepReader(p().instrument))
  }

  private val component = ScalaComponent.builder[Props]("SequenceTabContent")
    .stateless
    .render_P { p =>
      val InstrumentTabContentFocus(instrument, active, sequenceSelected, logDisplayed) = p.p()
      <.div(
        ^.cls := "ui attached secondary segment tab",
        ^.classSet(
          "active"    -> active
        ),
        dataTab := instrument.show,
        SeqexecStyles.emptyInstrumentTab.unless(sequenceSelected),
        SeqexecStyles.emptyInstrumentTabLogShown.when(!sequenceSelected && logDisplayed === SectionOpen),
        SeqexecStyles.emptyInstrumentTabLogHidden.when(!sequenceSelected && logDisplayed === SectionClosed),
        SeqexecStyles.instrumentTabSegment.when(sequenceSelected),
        SeqexecStyles.instrumentTabSegmentLogShown.when(sequenceSelected && logDisplayed === SectionOpen),
        SeqexecStyles.instrumentTabSegmentLogHidden.when(sequenceSelected && logDisplayed === SectionClosed),
        IconMessage(IconMessage.Props(IconInbox, Some("No sequence loaded"), IconMessage.Style.Warning)).unless(sequenceSelected),
        p.connect(st => SequenceStepsTableContainer(p.router, p.site, st)).when(sequenceSelected)
      )
    }
    .build

    def apply(router: RouterCtl[SeqexecPages], site: SeqexecSite, p: ModelProxy[InstrumentTabContentFocus]): Unmounted[Props, Unit, Unit] =
      component(Props(router, site, p))
}

/**
 * Contains the area with tabs and the sequence body
 */
object SequenceTabsBody {
  final case class Props(router: RouterCtl[SeqexecPages], site: SeqexecSite) {
    protected[sequence] val instrumentConnects = site.instruments.toList.map(i => SeqexecCircuit.connect(SeqexecCircuit.instrumentTabContentReader(i)))
  }

  private val component = ScalaComponent.builder[Props]("SequenceTabsBody")
    .stateless
    .render_P(p =>
      <.div(
        InstrumentsTabs(InstrumentsTabs.Props(p.router, p.site)),
        p.instrumentConnects.map(c => c(s => SequenceTabContent(p.router, p.site, s))).toList.toTagMod
      )
    ).build

  def apply(router: RouterCtl[SeqexecPages], site: SeqexecSite): Unmounted[Props, Unit, Unit] =
    component(Props(router, site))
}

/**
 * Top level container of the sequence area
 */
object SequenceArea {
  final case class Props(router: RouterCtl[SeqexecPages], site: SeqexecSite)

  private val component = ScalaComponent.builder[Props]("QueueTableSection")
    .stateless
    .render_P( p =>
      <.div(
        ^.cls := "ui sixteen wide column",
        SeqexecStyles.sequencesArea,
        SequenceTabsBody(p.router, p.site)
      )
    ).build

  def apply(router: RouterCtl[SeqexecPages], site: SeqexecSite): Unmounted[Props, Unit, Unit] = component(Props(router, site))
}
