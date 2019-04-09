// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components.sequence.steps

import cats.implicits._
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.extra.Reusability
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.ScalaComponent
import gem.Observation
import seqexec.web.client.model.StartFromOperation
import seqexec.web.client.actions.RequestRunFrom
import seqexec.web.client.circuit.SeqexecCircuit
import seqexec.web.client.components.SeqexecStyles
import seqexec.web.client.semanticui.elements.button.Button
import seqexec.web.client.semanticui.elements.popup.Popup
import seqexec.web.client.semanticui.elements.icon.Icon.IconPlay
import seqexec.web.client.reusability._
import web.client.style._

/**
  * Contains the control to start a step from an arbitrary point
  */
object RunFromStep {
  final case class Props(id:      Observation.Id,
                         stepId:  Int,
                         runFrom: StartFromOperation)

  implicit val propsReuse: Reusability[Props] = Reusability.derive[Props]

  def requestRunFrom(id: Observation.Id, stepId: Int): Callback =
    SeqexecCircuit.dispatchCB(RequestRunFrom(id, stepId))

  private val component = ScalaComponent
    .builder[Props]("RunFromStep")
    .render_P { p =>
      <.div(
        SeqexecStyles.runFrom,
        SeqexecStyles.notInMobile,
        Popup(
          Popup.Props("button", s"Run from step ${p.stepId + 1}"),
          Button(
            Button.Props(
              icon     = Some(IconPlay),
              color    = Some("blue"),
              onClick  = requestRunFrom(p.id, p.stepId),
              disabled = p.runFrom === StartFromOperation.StartFromInFlight)
          )
        )
      )
    }
    .configure(Reusability.shouldComponentUpdate)
    .build

  def apply(p: Props): Unmounted[Props, Unit, Unit] = component(p)
}
