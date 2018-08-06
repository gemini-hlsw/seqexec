// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components.sequence

import cats.implicits._
import diode.react.ModelProxy
import gem.enum.Site
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{CallbackTo, ScalaComponent, CatsReact}
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.extra.router.RouterCtl
import japgolly.scalajs.react.CatsReact._
import seqexec.web.client.components.sequence.toolbars.{SequenceDefaultToolbar, StepConfigToolbar, SequenceAnonymousToolbar}
import seqexec.web.client.circuit.{SeqexecCircuit, StatusAndStepFocus, InstrumentTabContentFocus}
import seqexec.web.client.model.Pages.SeqexecPages
import seqexec.web.client.model.{SectionOpen, SectionClosed}
// import seqexec.web.client.ModelOps._
import seqexec.web.client.semanticui._
import seqexec.web.client.semanticui.elements.message.IconMessage
import seqexec.web.client.semanticui.elements.icon.Icon.IconInbox
import seqexec.web.client.components.SeqexecStyles
import seqexec.web.client.components.sequence.steps.StepsTable
import web.client.style._

object SequenceStepsTableContainer {
  final case class Props(router: RouterCtl[SeqexecPages], site: Site, p: ModelProxy[StatusAndStepFocus]) {
    // protected[sequence] val sequenceControlConnects = site.instruments.toList.fproduct(i => SeqexecCircuit.connect(SeqexecCircuit.sequenceControlReader(i))).toMap
    // private[sequence] val instrumentConnects = site.instruments.toList.fproduct(i => SeqexecCircuit.connect(SeqexecCircuit.stepsTableReader(i))).toMap
    // protected[sequence] val sequenceObserverConnects = site.instruments.toList.fproduct(i => SeqexecCircuit.connect(SeqexecCircuit.sequenceObserverReader(i))).toMap
  }
  final case class State(nextStepToRun: Int)

  private val ST = ReactS.Fix[State]

  def updateStepToRun(step: Int): CatsReact.ReactST[CallbackTo, State, Unit] =
    ST.set(State(step)).liftCB

  def toolbar(p: Props): VdomElement = {
    val statusAndStep = p.p()
    statusAndStep.stepConfigDisplayed.fold{
      <.div(
        SequenceDefaultToolbar(p).when(statusAndStep.isLogged),
        SequenceAnonymousToolbar(p.site, statusAndStep.obsId).unless(statusAndStep.isLogged)
      ): VdomElement
    }(s => StepConfigToolbar(StepConfigToolbar.Props(p.router, p.site, statusAndStep.instrument, statusAndStep.obsId, s, statusAndStep.totalSteps)))
  }

  private val component = ScalaComponent.builder[Props]("SequenceStepsTableContainer")
    .initialState(State(0))
    .renderP { ($, p) =>
      <.div(
        ^.height := "100%",
        toolbar(p),
        SeqexecCircuit.connect(SeqexecCircuit.stepsTableReader(p.p().obsId))(r =>
            StepsTable(StepsTable.Props(p.router, p.p().isLogged, r, x => $.runState(updateStepToRun(x)))))
      )
    }.build

  def apply(router: RouterCtl[SeqexecPages], site: Site, p: ModelProxy[StatusAndStepFocus]): Unmounted[Props, State, Unit] =
    component(Props(router, site, p))
}

/**
* Content of a single tab with a sequence
*/
object SequenceTabContent {

  final case class Props(router: RouterCtl[SeqexecPages], site: Site, p: ModelProxy[InstrumentTabContentFocus]) {
    // protected[sequence] val connect = SeqexecCircuit.connect(SeqexecCircuit.statusAndStepReader(p().obsId))
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
        // p.connect(st => SequenceStepsTableContainer(p.router, p.site, st)).when(sequenceSelected)
      )
    }
    .build

    def apply(router: RouterCtl[SeqexecPages], site: Site, p: ModelProxy[InstrumentTabContentFocus]): Unmounted[Props, Unit, Unit] =
      component(Props(router, site, p))
}

/**
 * Contains the area with tabs and the sequence body
 */
object SequenceTabsBody {
  final case class Props(router: RouterCtl[SeqexecPages], site: Site)

  private val component = ScalaComponent.builder[Props]("SequenceTabsBody")
    .stateless
    .render_P(p =>
      <.div(
        InstrumentsTabs(InstrumentsTabs.Props(p.router))
      )
    ).build

  def apply(router: RouterCtl[SeqexecPages], site: Site): Unmounted[Props, Unit, Unit] =
    component(Props(router, site))
}

/**
 * Top level container of the sequence area
 */
object SequenceArea {
  final case class Props(router: RouterCtl[SeqexecPages], site: Site)

  private val component = ScalaComponent.builder[Props]("SequenceArea")
    .stateless
    .render_P( p =>
      <.div(
        ^.cls := "ui sixteen wide column",
        SeqexecStyles.sequencesArea,
        SequenceTabsBody(p.router, p.site)
      )
    ).build

  def apply(router: RouterCtl[SeqexecPages], site: Site): Unmounted[Props, Unit, Unit] = component(Props(router, site))
}
