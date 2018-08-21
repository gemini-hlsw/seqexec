// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components.sequence.toolbars

import diode.react.ModelProxy
import cats.implicits._
import gem.Observation
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{BackendScope, Callback, ScalaComponent}
import japgolly.scalajs.react.extra.{StateSnapshot, TimerSupport}
import japgolly.scalajs.react.component.Scala.Unmounted
import org.scalajs.dom.html.Div
import seqexec.model.Observer
import seqexec.web.client.actions.UpdateObserver
import seqexec.web.client.circuit.StatusAndObserverFocus
import seqexec.web.client.semanticui.elements.label.FormLabel
import seqexec.web.client.semanticui.elements.input.InputEV
import seqexec.web.client.components.SeqexecStyles
import scala.concurrent.duration._
import web.client.style._

/**
  * Encapsulates the field to change the observer name
  */
object SequenceObserverField {
  final case class Props(p: ModelProxy[StatusAndObserverFocus])

  final case class State(currentText: Option[String])

  class Backend(val $: BackendScope[Props, State]) extends TimerSupport {
    def updateObserver(id: Observation.Id, name: String): Callback =
      $.props >>= { p => Callback.when(p.p().isLogged)(p.p.dispatchCB(UpdateObserver(id, Observer(name)))) }

    def updateState(value: Option[String], cb: Callback): Callback =
      {$.state >>= { (s: State) => Callback.when(!s.currentText.contains(value.getOrElse("")))($.modState(_.copy(currentText = value))) }} >> cb

    def submitIfChanged: Callback =
      ($.state zip $.props) >>= {
        case (s, p) => Callback.when(p.p().isLogged && p.p().observer.map(_.value) =!= s.currentText)(updateObserver(p.p().id, s.currentText.orEmpty))
      }

    def setupTimer: Callback =
      // Every 2 seconds check if the field has changed and submit
      setInterval(submitIfChanged, 2.second)

    def render(p: Props, s: State): VdomTagOf[Div] = {
      val observerEV = StateSnapshot(s.currentText.orEmpty)(updateState)
      val StatusAndObserverFocus(_, instrument, _, _, _, _) = p.p()
      <.div(
        ^.cls := "ui form",
        <.div(
          ^.cls := "ui inline fields",
          SeqexecStyles.shorterFields,
          <.div(
            ^.cls := "field four wide required",
            FormLabel(FormLabel.Props("Observer", None))
          ),
          <.div(
            ^.cls := "field fourteen wide",
            InputEV(InputEV.Props(
              s"$instrument.observer",
              s"$instrument.observer",
              observerEV,
              placeholder = "Observer...",
              onBlur = _ => submitIfChanged))
          )
        )
      )
    }
  }

  private val component = ScalaComponent.builder[Props]("SequenceObserverField")
    .initialState(State(None))
    .renderBackend[Backend]
    .configure(TimerSupport.install)
    .componentWillMount(f => f.backend.$.props >>= {p => Callback.when(p.p().observer.isDefined)(f.backend.updateState(p.p().observer.map(_.value), Callback.empty))})
    .componentDidMount(_.backend.setupTimer)
    .componentWillReceiveProps { f =>
      val observer = f.nextProps.p().observer
      // Update the observer field
      Callback.when((observer.map(_.value) =!= f.state.currentText) && observer.nonEmpty)(f.backend.updateState(observer.map(_.value), Callback.empty))
    }
    .shouldComponentUpdatePure { f =>
      val observer = f.nextProps.p().observer
      observer.map(_.value) =!= f.currentState.currentText
    }
    .build

  def apply(p: ModelProxy[StatusAndObserverFocus]): Unmounted[Props, State, Backend] = component(Props(p))
}
