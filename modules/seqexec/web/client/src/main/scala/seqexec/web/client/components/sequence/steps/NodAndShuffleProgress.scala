// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components.sequence.steps

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import seqexec.web.client.model.StepItems.StepStateSnapshot
import web.client.ReactProps
import japgolly.scalajs.react.internal.CatsReactExt
import seqexec.web.client.components.{DividedProgress, SeqexecStyles}
import cats.syntax.option._
import seqexec.web.client.semanticui.elements.button.Button
import seqexec.web.client.semanticui.elements.icon.Icon.IconPause
import seqexec.web.client.semanticui.elements.popup.Popup

final case class NodAndShuffleCycleProgress(state: StepStateSnapshot) extends ReactProps {
  @inline def render: VdomElement = NodAndShuffleCycleProgress.component(this)
}

object NodAndShuffleCycleProgress extends CatsReactExt {
  type Props = NodAndShuffleCycleProgress

  implicit val stepReuse: Reusability[StepStateSnapshot] = Reusability.byEq
  implicit val propsReuse: Reusability[Props] = Reusability.derive[Props]

  protected val component = ScalaComponent
    .builder[Props]("NodAndShuffleCycleProgress")
    .stateless
    .render_P { p =>
      val isInError = !p.state.isNSRunning && p.state.isNSInError
      val msg = if (isInError) "Error" else "Running..."

      <.span(

        ^.display.flex,
        ^.justifyContent.spaceBetween,

        DividedProgress(
          s"Nod and Shuffle Cycle: $msg",
          total = 100, //enum.all.length.toLong - 1,
          value = 33, //max(0, s.counter.toLong),
          color = if (isInError) "red".some else "green".some,
          progressCls = List(SeqexecStyles.observationProgressBar),
          barCls = List(SeqexecStyles.observationBar),
          labelCls = List(SeqexecStyles.observationLabel)
          ),
        <.span(

          ^.marginLeft := "14px",

          Popup(
            Popup.Props("button", "Pause the current cycle"),
            Button(
              Button.Props(icon = Some(IconPause),
                           color = Some("teal"),
                           //                         onClick = requestObsPause(p.id, p.stepId),
                           //                         disabled = p.requestInFlight || p.isObservePaused
                           )
              )
            )

          )
        )
    }
    .configure(Reusability.shouldComponentUpdate)
    .build
}

final case class NodAndShuffleNodProgress(state  : StepStateSnapshot) extends ReactProps {
  @inline def render: VdomElement = NodAndShuffleNodProgress.component(this)
}

object NodAndShuffleNodProgress extends CatsReactExt {
  type Props = NodAndShuffleNodProgress

  implicit val stepReuse: Reusability[StepStateSnapshot] = Reusability.byEq
  implicit val propsReuse: Reusability[Props] = Reusability.derive[Props]

  protected val component = ScalaComponent
    .builder[Props]("NodAndShuffleNodProgress")
    .stateless
    .render_P { p =>
      val isInError = !p.state.isNSRunning && p.state.isNSInError
      val msg = if (isInError) "Error" else "Running..."

      <.span(

        ^.display.flex,
        ^.justifyContent.spaceBetween,

      DividedProgress(
        s"Nod and Shuffle Nod: $msg",
        total = 100, //enum.all.length.toLong - 1,
        value = 33, //max(0, s.counter.toLong),
        color = if (isInError) "red".some else "green".some,
        progressCls = List(SeqexecStyles.observationProgressBar),
        barCls = List(SeqexecStyles.observationBar),
        labelCls = List(SeqexecStyles.observationLabel)
        ),
      <.span(

        ^.marginLeft := "14px",

          Popup(
          Popup.Props("button", "Pause the current nod"),
          Button(
            Button.Props(icon = Some(IconPause),
                         color = Some("teal"),
                         //                         onClick = requestObsPause(p.id, p.stepId),
                         //                         disabled = p.requestInFlight || p.isObservePaused
                         )
            )
          )
        )
      )
    }
    .configure(Reusability.shouldComponentUpdate)
    .build
}
