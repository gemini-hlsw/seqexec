// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components.sequence

import cats.implicits._
import gem.enum.Site
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{CallbackTo, ScalaComponent, CatsReact}
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.extra.router.RouterCtl
import japgolly.scalajs.react.CatsReact._
import seqexec.web.client.components.sequence.toolbars.{SequenceDefaultToolbar, StepConfigToolbar, SequenceAnonymousToolbar}
import seqexec.web.client.circuit.{SeqexecCircuit, StatusAndStepFocus, SequenceTabContentFocus}
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
  final case class Props(router: RouterCtl[SeqexecPages], statusAndStep: StatusAndStepFocus) {
    // protected[sequence] val sequenceControlConnects = site.instruments.toList.fproduct(i => SeqexecCircuit.connect(SeqexecCircuit.sequenceControlReader(i))).toMap
    // private[sequence] val instrumentConnects = site.instruments.toList.fproduct(i => SeqexecCircuit.connect(SeqexecCircuit.stepsTableReader(i))).toMap
    // protected[sequence] val sequenceObserverConnects = site.instruments.toList.fproduct(i => SeqexecCircuit.connect(SeqexecCircuit.sequenceObserverReader(i))).toMap
  }
  final case class State(nextStepToRun: Int)

  private val ST = ReactS.Fix[State]

  def updateStepToRun(step: Int): CatsReact.ReactST[CallbackTo, State, Unit] =
    ST.set(State(step)).liftCB

  def toolbar(p: Props): VdomElement = {
    val loggedIn            = p.statusAndStep.isLogged
    val stepConfigDisplayed = p.statusAndStep.stepConfigDisplayed.isDefined
    val isPreview           = p.statusAndStep.isPreview
    val showDefault         = loggedIn && !stepConfigDisplayed && !isPreview
    val showAnonymous       = !loggedIn && !stepConfigDisplayed
    val showPreview         = isPreview && !stepConfigDisplayed

    <.div(
      SequenceDefaultToolbar(p).when(showDefault),
      SequenceAnonymousToolbar(SequenceAnonymousToolbar.Props(p.statusAndStep.obsId)).when(showAnonymous || showPreview),
      p.statusAndStep.stepConfigDisplayed.map { s =>
        StepConfigToolbar(StepConfigToolbar.Props(p.router, p.statusAndStep.instrument, p.statusAndStep.obsId, s, p.statusAndStep.totalSteps, isPreview)).when(stepConfigDisplayed)
      }.getOrElse(TagMod.empty)
    )
  }

  private val component = ScalaComponent.builder[Props]("SequenceStepsTableContainer")
    .initialState(State(0))
    .renderP { ($, p) =>
      <.div(
        ^.height := "100%",
        toolbar(p),
        SeqexecCircuit.connect(SeqexecCircuit.stepsTableReader(p.statusAndStep.obsId))(r =>
            StepsTable(StepsTable.Props(p.router, p.statusAndStep.isLogged, r, x => $.runState(updateStepToRun(x)))))
      )
    }.build

  def apply(p: Props): Unmounted[Props, State, Unit] =
    component(p)
}

/**
* Content of a single tab with a sequence
*/
object SequenceTabContent {

  final case class Props(router: RouterCtl[SeqexecPages], p: SequenceTabContentFocus) {
    val sequenceSelected: Boolean = p.id.isDefined
  }

  private val component = ScalaComponent.builder[Props]("SequenceTabContent")
    .stateless
    .render_P { p =>
      val SequenceTabContentFocus(instrument, id, active, logDisplayed) = p.p
      val content = id.map { i =>
        SeqexecCircuit.connect(SeqexecCircuit.statusAndStepReader(i)) { x =>
          x() match {
            case Some(st) => SequenceStepsTableContainer(SequenceStepsTableContainer.Props(p.router, st)): VdomElement
            case _        => IconMessage(IconMessage.Props(IconInbox, Some("No sequence loaded"), IconMessage.Style.Warning)): VdomElement
          }
        }
      }

      <.div(
        ^.cls := "ui attached secondary segment tab",
        ^.classSet(
          "active"    -> active
        ),
        dataTab := instrument.foldMap(_.show),
        SeqexecStyles.emptyInstrumentTab.unless(p.sequenceSelected),
        SeqexecStyles.emptyInstrumentTabLogShown.when(!p.sequenceSelected && logDisplayed === SectionOpen),
        SeqexecStyles.emptyInstrumentTabLogHidden.when(!p.sequenceSelected && logDisplayed === SectionClosed),
        SeqexecStyles.instrumentTabSegment.when(p.sequenceSelected),
        SeqexecStyles.instrumentTabSegmentLogShown.when(p.sequenceSelected && logDisplayed === SectionOpen),
        SeqexecStyles.instrumentTabSegmentLogHidden.when(p.sequenceSelected && logDisplayed === SectionClosed),
        content
      )
    }
    .build

    def apply(p: Props): Unmounted[Props, Unit, Unit] =
      component(p)
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
        InstrumentsTabs(InstrumentsTabs.Props(p.router)),
        SeqexecCircuit.connect(SeqexecCircuit.sequenceTabs)(x => ReactFragment(x().toList.map(t => SequenceTabContent(SequenceTabContent.Props(p.router, t)): VdomNode): _*))
      )
    ).build

  def apply(router: RouterCtl[SeqexecPages], site: Site): Unmounted[Props, Unit, Unit] = component(Props(router, site))
}
