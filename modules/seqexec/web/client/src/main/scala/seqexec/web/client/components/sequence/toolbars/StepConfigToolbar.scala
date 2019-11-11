// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components.sequence.toolbars

import cats.implicits._
import diode.react.ReactConnectProxy
import gem.Observation
import japgolly.scalajs.react.extra.router.RouterCtl
import japgolly.scalajs.react.ScalaComponent
import japgolly.scalajs.react.React
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.component.Scala.Unmounted
import react.common.style._
import react.common.implicits._
import seqexec.model.enum.Instrument
import seqexec.model.RunningStep
import seqexec.web.client.model.Pages._
import seqexec.web.client.circuit.SeqexecCircuit
import seqexec.web.client.circuit.SequenceInfoFocus
import seqexec.web.client.components.SeqexecStyles
import seqexec.web.client.semanticui.elements.button.ButtonGroup
import seqexec.web.client.semanticui.elements.button.Button
import seqexec.web.client.semanticui.elements.button.Button._
import seqexec.web.client.semanticui.elements.icon.Icon.IconChevronLeft
import seqexec.web.client.semanticui.elements.icon.Icon.IconChevronRight
import seqexec.web.client.semanticui.elements.label.Label
import seqexec.web.client.semanticui.Size
import mouse.boolean._

/**
  * Toolbar when displaying a step configuration
  */
object StepConfigToolbar {
  final case class Props(router:     RouterCtl[SeqexecPages],
                         instrument: Instrument,
                         id:         Observation.Id,
                         step:       Int,
                         total:      Int,
                         isPreview:  Boolean) {
    val sequenceConnect: ReactConnectProxy[Option[SequenceInfoFocus]] =
      SeqexecCircuit.connect(SeqexecCircuit.sequenceObserverReader(id))
  }

  private val component = ScalaComponent
    .builder[Props]("StepConfigToolbar")
    .stateless
    .render_P { p =>
      val sequencePage = if (p.isPreview) {
        PreviewPage(p.instrument, p.id, StepIdDisplayed(p.step))
      } else {
        SequencePage(p.instrument, p.id, StepIdDisplayed(p.step))
      }
      val nextStepPage = if (p.isPreview) {
        PreviewConfigPage(p.instrument, p.id, p.step + 2)
      } else {
        SequenceConfigPage(p.instrument, p.id, p.step + 2)
      }
      val prevStepPage = if (p.isPreview) {
        PreviewConfigPage(p.instrument, p.id, p.step)
      } else {
        SequenceConfigPage(p.instrument, p.id, p.step)
      }

      <.div(
        ^.cls := "ui grid",
        <.div(
          ^.cls := "ui row three column",
          SeqexecStyles.shorterRow,
          <.div(
            ^.cls := "ui left floated two wide column",
            SeqexecStyles.shorterFields,
            // Back to sequence button
            p.router.link(sequencePage)(
              Button(icon    = Some(IconChevronLeft),
                     labeled = LeftLabeled,
                     onClick = p.router.setUrlAndDispatchCB(sequencePage))("Back")
            )
          ),
          <.div(
            ^.cls := "left floated six wide column bottom aligned computer only",
            p.sequenceConnect(_() match {
              case Some(p) => SequenceInfo(SequenceInfo.Props(p))
              case _       => React.Fragment()
            })
          ),
          <.div(
            ^.cls := "ui right floated eight wide column",
            SeqexecStyles.shorterFields,
            ButtonGroup(
              ButtonGroup.Props(List(Css("right floated"))),
              // Previous step button
              (p.step > 0).option(
                p.router.link(prevStepPage)(
                  Button(icon    = Some(IconChevronLeft),
                         labeled = LeftLabeled,
                         onClick = p.router.setUrlAndDispatchCB(prevStepPage))("Prev")
                )
              ),
              Label(
                Label.Props(RunningStep.fromInt(p.step, p.total).getOrElse(RunningStep.Zero).show,
                            size        = Size.Large,
                            extraStyles = List(SeqexecStyles.labelAsButton))
              ),
              // Next step button
              (p.step < p.total - 1).option(
                p.router.link(nextStepPage)(
                  Button(icon    = Some(IconChevronRight),
                         labeled = RightLabeled,
                         onClick = p.router.setUrlAndDispatchCB(nextStepPage))("Next")
                )
              )
            )
          )
        )
      )
    }
    .build

  def apply(p: Props): Unmounted[Props, Unit, Unit] = component(p)
}
