// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.web.client.components.sequence

import diode.react.ModelProxy
import edu.gemini.seqexec.web.client.components.sequence.toolbars.{SequenceInfo, SequenceControl, SequenceObserverField, StepConfigToolbar, SequenceAnonymousToolbar}
import edu.gemini.seqexec.web.client.circuit.{SeqexecCircuit, StatusAndStepFocus, InstrumentTabContentFocus}
import edu.gemini.seqexec.web.client.semanticui._
import edu.gemini.seqexec.web.client.semanticui.elements.message.IconMessage
import edu.gemini.seqexec.web.client.semanticui.elements.icon.Icon.IconInbox
import edu.gemini.seqexec.web.client.components.SeqexecStyles
import edu.gemini.seqexec.model.Model.SeqexecSite
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{CallbackTo, ScalaComponent, ScalazReact}
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.ScalazReact._
import scalacss.ScalaCssReact._

import scalaz.syntax.show._

object SequenceStepsTableContainer {
  final case class Props(site: SeqexecSite, p: ModelProxy[StatusAndStepFocus]) {
    protected[sequence] val sequenceControlConnects = site.instruments.list.toList.map(i => (i, SeqexecCircuit.connect(SeqexecCircuit.sequenceControlReader(i)))).toMap
    private[sequence] val instrumentConnects = site.instruments.list.toList.map(i => (i, SeqexecCircuit.connect(SeqexecCircuit.stepsTableReader(i)))).toMap
    protected[sequence] val sequenceObserverConnects = site.instruments.list.toList.map(i => (i, SeqexecCircuit.connect(SeqexecCircuit.sequenceObserverReader(i)))).toMap
  }
  final case class State(nextStepToRun: Int)

  private val ST = ReactS.Fix[State]

  def updateStepToRun(step: Int): ScalazReact.ReactST[CallbackTo, State, Unit] =
    ST.set(State(step)).liftCB


  private val component = ScalaComponent.builder[Props]("SequenceStepsTableContainer")
    .initialState(State(0))
    .renderP { ($, p) =>
      <.div(
        ^.cls := "ui grid",
        p.p().stepConfigDisplayed.fold{
          if (p.p().isLogged)
            List(<.div(
              ^.cls := "ui row",
              SeqexecStyles.shorterRow,
              <.div(
                ^.cls := "ui sixteen wide column",
                p.sequenceObserverConnects.get(p.p().instrument).whenDefined(c => c(SequenceInfo.apply))
              )
            ),
            <.div(
              ^.cls := "ui row",
              SeqexecStyles.shorterRow,
              SeqexecStyles.lowerRow,
              <.div(
                ^.cls := "ui left column eight wide computer sixteen wide tablet only",
                SeqexecStyles.controlColumn,
                p.sequenceControlConnects.get(p.p().instrument).whenDefined(c => c(SequenceControl.apply))
              ),
              <.div(
                ^.cls := "ui right column eight wide computer eight wide tablet sixteen wide mobile",
                SeqexecStyles.controlColumn,
                p.sequenceObserverConnects.get(p.p().instrument).whenDefined(c => c(m => SequenceObserverField(m)))
              )
            )).toTagMod
        //          SequenceDefaultToolbar(p.site, p.p().instrument): VdomElement
          else
            SequenceAnonymousToolbar(p.site, p.p().instrument): VdomElement
        }(s => StepConfigToolbar(StepConfigToolbar.Props(p.site, p.p().instrument, p.p().isLogged, s))),
        <.div(
          ^.cls := "ui row",
          SeqexecStyles.lowerRow,
          <.div(
            ^.cls := "ui sixteen wide column",
            p.instrumentConnects.get(p.p().instrument).whenDefined(x => x(m =>
                StepsTableContainer(StepsTableContainer.Props(m, x => $.runState(updateStepToRun(x))))))
          )
        )
      )
    }.build

  def apply(site: SeqexecSite, p: ModelProxy[StatusAndStepFocus]): Unmounted[Props, State, Unit] = component(Props(site, p))
}

/**
* Content of a single tab with a sequence
*/
object SequenceTabContent {

  final case class Props(site: SeqexecSite, p: ModelProxy[InstrumentTabContentFocus]) {
    protected[sequence] val connect = SeqexecCircuit.connect(SeqexecCircuit.statusAndStepReader(p().instrument))
  }

  private val component = ScalaComponent.builder[Props]("SequenceTabContent")
    .stateless
    .render_P { p =>
      val InstrumentTabContentFocus(instrument, active, sequenceSelected) = p.p()
      <.div(
        ^.cls := "ui attached secondary segment tab",
        ^.classSet(
          "active" -> active
        ),
        dataTab := instrument.shows,
        IconMessage(IconMessage.Props(IconInbox, Some("No sequence loaded"), IconMessage.Style.Warning)).unless(sequenceSelected),
        p.connect(st => SequenceStepsTableContainer(p.site, st)).when(sequenceSelected)
      )
    }
    .build

    def apply(site: SeqexecSite, p: ModelProxy[InstrumentTabContentFocus]): Unmounted[Props, Unit, Unit] = component(Props(site, p))
}

/**
 * Contains the area with tabs and the sequence body
 */
object SequenceTabsBody {
  final case class Props(site: SeqexecSite) {
    protected[sequence] val instrumentConnects = site.instruments.list.map(i => SeqexecCircuit.connect(SeqexecCircuit.instrumentTabContentReader(i)))
  }

  private val component = ScalaComponent.builder[Props]("SequenceTabsBody")
    .stateless
    .render_P(p =>
      <.div(
        InstrumentsTabs(p.site),
        p.instrumentConnects.map(c => c(s => SequenceTabContent(p.site, s))).toList.toTagMod
      )
    ).build

  def apply(site: SeqexecSite): Unmounted[Props, Unit, Unit] = component(Props(site))
}

/**
 * Top level container of the sequence area
 */
object SequenceArea {

  private val component = ScalaComponent.builder[SeqexecSite]("QueueTableSection")
    .stateless
    .render_P( p =>
      <.div(
        ^.cls := "ui sixteen wide column",
        SequenceTabsBody(p)
      )
    ).build

  def apply(site: SeqexecSite): Unmounted[SeqexecSite, Unit, Unit] = component(site)
}
