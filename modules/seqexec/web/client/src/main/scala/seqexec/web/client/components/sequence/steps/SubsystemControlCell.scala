// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components.sequence.steps

import cats.implicits._
import japgolly.scalajs.react.CatsReact._
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.vdom.html_<^._
// import japgolly.scalajs.react.Callback
// import japgolly.scalajs.react.CallbackTo
import japgolly.scalajs.react.ScalaComponent
import gem.Observation
// import seqexec.model._
import seqexec.model.enum._
import seqexec.web.client.components.SeqexecStyles
import seqexec.web.client.semanticui.elements.button.Button
import seqexec.web.client.semanticui.elements.popup.Popup
// import seqexec.web.client.semanticui.elements.icon.Icon.IconPause
// import seqexec.web.client.semanticui.elements.icon.Icon.IconPlay
// import seqexec.web.client.semanticui.elements.icon.Icon.IconStop
// import seqexec.web.client.semanticui.elements.icon.Icon.IconTrash
import web.client.style._

/**
  * Contains the control buttons for each subsystem
  */
object SubsystemControlCell {
  final case class Props(id:         Observation.Id,
                         instrument: Instrument,
                         stepId:     Int,
                         resources:  List[Resource])
  final case class State()

  private val ST = ReactS.Fix[State]
  println(ST)

  private val component = ScalaComponent
    .builder[Props]("SubsystemControl")
    .initialState(State())
    .renderPS { ($, p, s) =>
      <.div(
        SeqexecStyles.notInMobile,
        p.resources.map { r =>
          Popup(
            Popup.Props("button", s"Configure ${r.show}"),
            Button(Button.Props(color = Some("green")), r.show)
            // onClick =
            //   $.runState(handleObsPause(p.id, p.stepId)))
          )
        }.toTagMod
        // .observationOperations(p.isObservePaused)
        // .map {
        //   case PauseObservation =>
        //     Popup(
        //       Popup.Props("button", "Pause the current exposure"),
        //       Button(
        //         Button.Props(icon  = Some(IconPause),
        //                      color = Some("teal"),
        //                      onClick =
        //                        $.runState(handleObsPause(p.id, p.stepId)),
        //                      disabled = !s.canPause || p.isObservePaused))
        //     )
        //   case StopObservation =>
        //     Popup(
        //       Popup.Props("button", "Stop the current exposure early"),
        //       Button(
        //         Button.Props(icon     = Some(IconStop),
        //                      color    = Some("orange"),
        //                      onClick  = $.runState(handleStop(p.id, p.stepId)),
        //                      disabled = !s.canStop))
        //     )
        //   case AbortObservation =>
        //     Popup(
        //       Popup.Props("button", "Abort the current exposure"),
        //       Button(
        //         Button.Props(
        //           icon     = Some(IconTrash),
        //           color    = Some("red"),
        //           onClick  = $.runState(handleAbort(p.id, p.stepId)),
        //           disabled = !s.canAbort))
        //     )
        //   case ResumeObservation =>
        //     Popup(
        //       Popup.Props("button", "Resume the current exposure"),
        //       Button(
        //         Button.Props(icon  = Some(IconPlay),
        //                      color = Some("blue"),
        //                      onClick =
        //                        $.runState(handleObsResume(p.id, p.stepId)),
        //                      disabled = !s.canResume || !p.isObservePaused))
        //     )
        //   // Hamamatsu operations
        //   case PauseImmediatelyObservation =>
        //     Popup(
        //       Popup.Props("button", "Pause the current exposure immediately"),
        //       Button(
        //         Button.Props(icon = Some(IconPause), color = Some("teal"))))
        //   case PauseGracefullyObservation =>
        //     Popup(Popup.Props("button",
        //                       "Pause the current exposure gracefully"),
        //           Button(
        //             Button.Props(icon  = Some(IconPause),
        //                          color = Some("teal"),
        //                          basic = true)))
        //   case StopImmediatelyObservation =>
        //     Popup(
        //       Popup.Props("button", "Stop the current exposure immediately"),
        //       Button(
        //         Button.Props(icon = Some(IconStop), color = Some("orange"))))
        //   case StopGracefullyObservation =>
        //     Popup(Popup.Props("button",
        //                       "Stop the current exposure gracefully"),
        //           Button(
        //             Button.Props(icon  = Some(IconStop),
        //                          color = Some("orange"),
        //                          basic = true)))
        // }
        // .toTagMod
      )
    }
    // .componentWillReceiveProps { f =>
    //   f.runState(if (f.nextProps.isObservePaused) {
    //     ST.set(NoneRequested)
    //   } else {
    //     ST.nop
    //   })
    // }
    .build

  def apply(p: Props): Unmounted[Props, State, Unit] = component(p)
}
