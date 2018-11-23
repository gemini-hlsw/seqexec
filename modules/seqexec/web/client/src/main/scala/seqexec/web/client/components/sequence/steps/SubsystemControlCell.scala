// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components.sequence.steps

import cats.implicits._
import japgolly.scalajs.react.CatsReact._
import japgolly.scalajs.react.CatsReact
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.CallbackTo
import japgolly.scalajs.react.ScalaComponent
import gem.Observation
import monocle.Optional
import monocle.macros.Lenses
import monocle.function.At.at
import monocle.function.At.atSortedMap
import monocle.std
import scala.collection.immutable.SortedMap
import seqexec.model.enum._
import seqexec.model.StepId
import seqexec.web.client.actions.RequestResourceRun
import seqexec.web.client.circuit.SeqexecCircuit
import seqexec.web.client.components.SeqexecStyles
import seqexec.web.client.semanticui.elements.button.Button
import seqexec.web.client.semanticui.elements.popup.Popup
import seqexec.web.client.semanticui.Size
import web.client.style._

/**
  * Contains the control buttons for each subsystem
  */
object SubsystemControlCell {
  final case class Props(id:        Observation.Id,
                         stepId:    Int,
                         resources: List[Resource])

  @Lenses
  final case class State(resources: SortedMap[Resource, Boolean])

  @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
  object State {
    def resource(r: Resource): Optional[State, Boolean] =
      State.resources ^|->
        at(r) ^<-?
        std.option.some
  }

  private val ST = ReactS.Fix[State]

  def requestResourceCall(id:     Observation.Id,
                          stepId: StepId,
                          r:      Resource): Callback =
    SeqexecCircuit.dispatchCB(RequestResourceRun(id, stepId, r))

  def handleResourceCall(
    id:     Observation.Id,
    stepId: StepId,
    r:      Resource): CatsReact.ReactST[CallbackTo, State, Unit] =
    ST.retM(requestResourceCall(id, stepId, r)) >> ST
      .mod(State.resource(r).set(true))
      .liftCB

  private val component = ScalaComponent
    .builder[Props]("SubsystemControl")
    .initialStateFromProps(p =>
      State(SortedMap(p.resources.fproduct(_ => false): _*)))
    .renderPS { ($, p, s) =>
      <.div(
        SeqexecStyles.notInMobile,
        p.resources.map { r =>
          Popup(
            Popup.Props("button", s"Configure ${r.show}"),
            Button(Button.Props(size  = Size.Small,
                                color = Some("blue"),
                                onClick = $.runState(
                                  handleResourceCall(p.id, p.stepId, r))),
                   r.show)
          )
        }.toTagMod
      )
    }
    .build

  def apply(p: Props): Unmounted[Props, State, Unit] = component(p)
}
