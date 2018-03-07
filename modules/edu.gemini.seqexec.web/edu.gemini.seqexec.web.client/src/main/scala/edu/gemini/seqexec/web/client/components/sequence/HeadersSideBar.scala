// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.web.client.components.sequence

import diode.react.ModelProxy
import edu.gemini.seqexec.model.Model.{CloudCover, ImageQuality, SkyBackground, WaterVapor, Operator}
import edu.gemini.seqexec.web.client.semanticui.elements.dropdown.DropdownMenu
import edu.gemini.seqexec.web.client.semanticui.elements.label.FormLabel
import edu.gemini.seqexec.web.client.semanticui.elements.input.InputEV
import edu.gemini.seqexec.web.client.circuit.{HeaderSideBarFocus, SeqexecCircuit}
import edu.gemini.seqexec.web.client.actions.{UpdateOperator, UpdateCloudCover, UpdateWaterVapor, UpdateImageQuality, UpdateSkyBackground}
import edu.gemini.seqexec.web.client.components.SeqexecStyles
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.{BackendScope, Callback, ScalaComponent}
import japgolly.scalajs.react.extra.{StateSnapshot, TimerSupport}
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.html.Div
import scalacss.ScalaCssReact._

import scalaz.syntax.equal._
import scalaz.std.string._
import scalaz.std.option._
import scalaz.syntax.std.option._
import scalaz.Equal
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
    implicit val equals: Equal[State] = Equal.equalA[State]
  }

  class Backend(val $: BackendScope[Props, State]) extends TimerSupport {
    def updateOperator(name: String): Callback =
      $.props >>= { p => Callback.when(p.isLogged)(p.model.dispatchCB(UpdateOperator(Operator(name)))) }

    def updateState(value: String): Callback =
      $.modState(_.copy(currentText = value.some))

    def setupTimer: Callback =
      // Every 2 seconds check if the field has changed and submit
      setInterval(submitIfChanged, 2.second)

    def submitIfChanged: Callback =
      ($.state zip $.props) >>= {
        case (s, p) => Callback.when(p.model().operator =/= s.currentText.map(Operator.apply))(updateOperator(~s.currentText))
      }

    def iqChanged(iq: ImageQuality): Callback =
      Callback(SeqexecCircuit.dispatch(UpdateImageQuality(iq)))

    def ccChanged(i: CloudCover): Callback =
      Callback(SeqexecCircuit.dispatch(UpdateCloudCover(i)))

    def sbChanged(sb: SkyBackground): Callback =
      Callback(SeqexecCircuit.dispatch(UpdateSkyBackground(sb)))

    def wvChanged(wv: WaterVapor): Callback =
      Callback(SeqexecCircuit.dispatch(UpdateWaterVapor(wv)))

    def render(p: Props, s: State): VdomTagOf[Div] = {
      val enabled = p.model().status.isLogged && p.model().status.anySelected

      val operatorEV = StateSnapshot(~s.currentText)(updateState)
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
    .componentWillMount(f => f.backend.$.props >>= {p => Callback.when(p.model().operator.isDefined)(f.backend.updateState(p.model().operator.getOrElse(Operator.Zero).value))})
    .componentDidMount(_.backend.setupTimer)
    .componentWillReceiveProps { f =>
      val operator = f.nextProps.model().operator
      // Update the operator field
      Callback.when((operator =/= f.state.currentText.map(Operator.apply)) && operator.nonEmpty)(f.modState(_.copy(currentText = operator.map(_.value))))
    }
    .build

  def apply(model: ModelProxy[HeaderSideBarFocus]): Unmounted[Props, State, Backend] =
    component(Props(model))
}
