// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components.sequence

import diode.react.ModelProxy
import seqexec.model.enum.{ CloudCover, ImageQuality, SkyBackground, WaterVapor }
import seqexec.model.Operator
import seqexec.web.client.semanticui.elements.dropdown.DropdownMenu
import seqexec.web.client.semanticui.elements.label.FormLabel
import seqexec.web.client.semanticui.elements.input.InputEV
import seqexec.web.client.circuit.{HeaderSideBarFocus, SeqexecCircuit}
import seqexec.web.client.actions.{UpdateCloudCover, UpdateImageQuality, UpdateOperator, UpdateSkyBackground, UpdateWaterVapor}
import seqexec.web.client.components.SeqexecStyles
import web.client.style._
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.{BackendScope, Callback, ScalaComponent}
import japgolly.scalajs.react.extra.{StateSnapshot, TimerSupport}
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.html.Div
import cats.implicits._
import cats.Eq
import scala.concurrent.duration._

/**
  * Display to show headers per sequence
  */
object HeadersSideBar {
  final case class Props(model: ModelProxy[HeaderSideBarFocus]) {
    def isLogged: Boolean = model().status.isLogged
  }

  final case class State(currentText: Option[String])

  object State {
    implicit val equals: Eq[State] = Eq.fromUniversalEquals
  }

  class Backend(val $: BackendScope[Props, State]) extends TimerSupport {
    def updateOperator(name: String): Callback =
      $.props >>= { p => Callback.when(p.isLogged)(p.model.dispatchCB(UpdateOperator(Operator(name)))) }

    def updateState(value: Option[String], cb: Callback): Callback =
      $.modState(_.copy(currentText = value)) >> cb

    def setupTimer: Callback =
      // Every 2 seconds check if the field has changed and submit
      setInterval(submitIfChanged, 2.second)

    def submitIfChanged: Callback =
      ($.state zip $.props) >>= {
        case (s, p) => Callback.when(p.model().operator =!= s.currentText.map(Operator.apply))(updateOperator(s.currentText.getOrElse("")))
      }

    def iqChanged(iq: ImageQuality): Callback =
      SeqexecCircuit.dispatchCB(UpdateImageQuality(iq))

    def ccChanged(i: CloudCover): Callback =
      SeqexecCircuit.dispatchCB(UpdateCloudCover(i))

    def sbChanged(sb: SkyBackground): Callback =
      SeqexecCircuit.dispatchCB(UpdateSkyBackground(sb))

    def wvChanged(wv: WaterVapor): Callback =
      SeqexecCircuit.dispatchCB(UpdateWaterVapor(wv))

    def render(p: Props, s: State): VdomTagOf[Div] = {
      val enabled = p.model().status.canOperate
      val operatorEV = StateSnapshot(s.currentText.getOrElse(""))(updateState)
      <.div(
        ^.cls := "ui secondary segment",
        SeqexecStyles.headerSideBarStyle,
        <.div(
          ^.cls := "ui form",
          <.div(
            ^.cls := "fields",
            SeqexecStyles.fieldsNoBottom,
            <.div(
              ^.cls := "sixteen wide field",
              FormLabel(FormLabel.Props("Operator", Some("operator"))),
              InputEV(
                InputEV.Props("operator", "operator",
                  operatorEV,
                  placeholder = "Operator...",
                  disabled = !enabled,
                  onBlur = _ => submitIfChanged
                )
              )
            )
          ),
          <.div(
            ^.cls := "two fields",
            SeqexecStyles.fieldsNoBottom,
            DropdownMenu(DropdownMenu.Props("Image Quality", p.model().conditions.iq.some, "Select", ImageQuality.all, disabled = !enabled, iqChanged)),
            DropdownMenu(DropdownMenu.Props("Cloud Cover", p.model().conditions.cc.some, "Select", CloudCover.all, disabled = !enabled, ccChanged))
          ),
          <.div(
            ^.cls := "two fields",
            SeqexecStyles.fieldsNoBottom,
            DropdownMenu(DropdownMenu.Props("Water Vapor", p.model().conditions.wv.some, "Select", WaterVapor.all, disabled = !enabled, wvChanged)),
            DropdownMenu(DropdownMenu.Props("Sky Background", p.model().conditions.sb.some, "Select", SkyBackground.all, disabled = !enabled, sbChanged))
          )
        )
      )
    }
  }

  private val component = ScalaComponent.builder[Props]("HeadersSideBar")
    .initialState(State(None))
    .renderBackend[Backend]
    .configure(TimerSupport.install)
    .componentWillMount(f => f.backend.$.props >>= {p => Callback.when(p.model().operator.isDefined)(f.backend.updateState(p.model().operator.map(_.value), Callback.empty))})
    .componentDidMount(_.backend.setupTimer)
    .componentWillReceiveProps { f =>
      val operator = f.nextProps.model().operator
      // Update the operator field
      Callback.when((operator =!= f.state.currentText.map(Operator.apply)) && operator.nonEmpty)(f.modState(_.copy(currentText = operator.map(_.value))))
    }
    .build

  def apply(model: ModelProxy[HeaderSideBarFocus]): Unmounted[Props, State, Backend] =
    component(Props(model))
}
