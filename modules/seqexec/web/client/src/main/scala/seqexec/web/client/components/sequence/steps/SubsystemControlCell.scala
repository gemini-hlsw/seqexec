// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components.sequence.steps

import java.util.logging.Logger

import cats.implicits._
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.ScalaComponent
import japgolly.scalajs.react.ReactEvent
import japgolly.scalajs.react.extra.Reusability
import japgolly.scalajs.react.extra.Reusability._
import gem.Observation

import scala.collection.immutable.SortedMap
import seqexec.model.enum._
import seqexec.model.StepId
import seqexec.web.client.actions.RequestResourceRun
import seqexec.web.client.circuit.SeqexecCircuit
import seqexec.web.client.components.SeqexecStyles
import seqexec.web.client.model.ResourceRunOperation
import seqexec.web.client.semanticui.elements.button.Button
import seqexec.web.client.semanticui.elements.popup.Popup
import seqexec.web.client.semanticui.elements.icon.Icon.IconCircleNotched
import seqexec.web.client.semanticui.Size
import seqexec.web.client.reusability._
import web.client.style._

/**
  * Contains the control buttons for each subsystem
  */
object SubsystemControlCell {
  private val logger = Logger.getLogger(this.getClass.getName)

  final case class Props(
    id:             Observation.Id,
    stepId:         Int,
    resources:      List[Resource],
    resourcesCalls: SortedMap[Resource, ResourceRunOperation])

  implicit val propsReuse: Reusability[Props] = Reusability.derive[Props]

  def requestResourceCall(
    id:     Observation.Id,
    stepId: StepId,
    r:      Resource
  )(e:      ReactEvent): Callback = {
    if (r === Instrument.Gpi) logger.info(s"*** SubsystemControlCell requestResourceCall: $r")
    e.preventDefaultCB *> e.stopPropagationCB *>
      SeqexecCircuit.dispatchCB(RequestResourceRun(id, stepId, r))
  }

  private val RunningIcon = IconCircleNotched.copyIcon(
    fitted      = true,
    loading     = true,
    extraStyles = List(SeqexecStyles.runningIcon))

  private val component = ScalaComponent
    .builder[Props]("SubsystemControl")
    .render_P { p =>
      <.div(
        SeqexecStyles.notInMobile,
        p.resources.map { r =>
          val inExecution = {
            if (r === Instrument.Gpi) logger.info(s"*** SubsystemControl r: $r //// p.resourcesCalls: ${p.resourcesCalls}")
            p.resourcesCalls
              .get(r)
              .map(t => t === {
                if (r === Instrument.Gpi) logger.info("*** SubsystemControl map in_executon = ResourceRunInFlight: " +
                  s"${t === ResourceRunOperation.ResourceRunInFlight}")
                ResourceRunOperation.ResourceRunInFlight
              })
              .getOrElse({
                if (r === Instrument.Gpi) logger.info("*** SubsystemControl in_execution = getOrElse false")
                false
              })
          }
          Popup(
            Popup.Props("button", s"Configure ${r.show}"),
            Button(
              Button.Props(
                size     = Size.Small,
                color    = Some("blue"),
                disabled = inExecution,
                labeled =
                  if (inExecution) {
                    if (r === Instrument.Gpi) logger.info("*** SubsystemControl in_execution: Button.LeftLabeled")
                    Button.LeftLabeled
                  } else {
                    if (r === Instrument.Gpi) logger.info("*** SubsystemControl not_in_execution: Button.NotLabeled")
                    Button.NotLabeled
                  },
                icon = p.resourcesCalls
                  .get(r)
                  .filter(t => t === {
                    if (r === Instrument.Gpi) logger.info("*** SubsystemControl filter = ResourceRunInFlight: " +
                      s"${t === ResourceRunOperation.ResourceRunInFlight}"
                    )
                    ResourceRunOperation.ResourceRunInFlight
                  })
                  .as(RunningIcon),
                onClickE = requestResourceCall(p.id, p.stepId, r) _
              ),
              r.show
            )
          )
        }.toTagMod
      )
    }
    .configure(Reusability.shouldComponentUpdate)
    .build

  def apply(p: Props): Unmounted[Props, Unit, Unit] = component(p)
}
