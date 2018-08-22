// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components.sequence

import diode.react.ModelProxy
import seqexec.model.enum.{ CloudCover, ImageQuality, SkyBackground, WaterVapor }
import seqexec.model.{ Observer, Operator }
import seqexec.web.client.semanticui.elements.dropdown.DropdownMenu
import seqexec.web.client.semanticui.elements.label.FormLabel
import seqexec.web.client.semanticui.elements.input.InputEV
import seqexec.web.client.circuit.{ HeaderSideBarFocus, SeqexecCircuit, SequenceObserverFocus }
import seqexec.web.client.actions.{UpdateCloudCover, UpdateImageQuality, UpdateOperator, UpdateObserver, UpdateSkyBackground, UpdateWaterVapor}
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
    def canOperate: Boolean = model().status.canOperate
    def sequenceObserver: Option[SequenceObserverFocus] = model().observer
  }

  final case class State(operatorText: Option[String], observerText: Option[String])

  object State {
    implicit val equals: Eq[State] = Eq.fromUniversalEquals
  }

  class Backend(val $: BackendScope[Props, State]) extends TimerSupport {
    def updateOperator(name: String): Callback =
      $.props >>= { p => Callback.when(p.canOperate)(p.model.dispatchCB(UpdateOperator(Operator(name)))) }

    def updateObserver(name: String): Callback =
      $.props >>= { p => Callback.when(p.canOperate)(p.sequenceObserver.map(a => p.model.dispatchCB(UpdateObserver(a.obsId, Observer(name)))).getOrEmpty) }

    def updateStateOp(value: Option[String], cb: Callback): Callback =
      $.modState(_.copy(operatorText = value)) >> cb

    def updateStateOb(value: Option[String], cb: Callback): Callback =
      $.modState(_.copy(observerText = value)) >> cb

    def setupTimer: Callback =
      // Every 2 seconds check if the field has changed and submit
      setInterval(submitIfChangedOp *> submitIfChangedOb, 2.second)

    def submitIfChangedOp: Callback =
      ($.state zip $.props) >>= {
        case (s, p) => Callback.when(p.model().operator =!= s.operatorText.map(Operator.apply))(updateOperator(s.operatorText.getOrElse("")))
      }

    def submitIfChangedOb: Callback =
      ($.state zip $.props) >>= {
        case (s, p) => Callback.when(p.model().observer.flatMap(_.observer).exists(_.some =!= s.observerText.map(Observer.apply)))(updateObserver(s.observerText.getOrElse("")))
      }

    def iqChanged(iq: ImageQuality): Callback =
      SeqexecCircuit.dispatchCB(UpdateImageQuality(iq))

    def ccChanged(i: CloudCover): Callback =
      SeqexecCircuit.dispatchCB(UpdateCloudCover(i))

    def sbChanged(sb: SkyBackground): Callback =
      SeqexecCircuit.dispatchCB(UpdateSkyBackground(sb))

    def wvChanged(wv: WaterVapor): Callback =
      SeqexecCircuit.dispatchCB(UpdateWaterVapor(wv))

    // scalastyle:off
    def render(p: Props, s: State): VdomTagOf[Div] = {
      val enabled = p.model().status.canOperate
      val operatorEV = StateSnapshot(s.operatorText.getOrElse(""))(updateStateOp)
      val observerEV = StateSnapshot(s.observerText.getOrElse(""))(updateStateOb)
      val instrument = p.sequenceObserver.map(_.instrument).foldMap(i => s" - ${i.show}")
      val instrumentEmpty = p.sequenceObserver.isEmpty
      val observerField = s"Observer$instrument"
      <.div(
        ^.cls := "ui secondary segment",
        SeqexecStyles.headerSideBarStyle,
        <.div(
          ^.cls := "ui form",
          <.div(
            ^.cls := "two fields",
            SeqexecStyles.fieldsNoBottom,
            <.div(
              ^.cls := "eight wide field",
              FormLabel(FormLabel.Props("Operator", Some("operator"))),
              InputEV(
                InputEV.Props("operator", "operator",
                  operatorEV,
                  placeholder = "Operator...",
                  disabled = !enabled,
                  onBlur = _ => submitIfChangedOp
                )
              )
            ),
            <.div(
              ^.cls := "eight wide field",
              FormLabel(FormLabel.Props(observerField, Some("observer"))),
              InputEV(
                InputEV.Props("observer", "observer",
                  observerEV,
                  placeholder = "Observer...",
                  disabled = !enabled || instrumentEmpty,
                  onBlur = _ => submitIfChangedOb
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
    // scalastyle:on
  }

  private val component = ScalaComponent.builder[Props]("HeadersSideBar")
    .initialState(State(None, None))
    .renderBackend[Backend]
    .configure(TimerSupport.install)
    .componentWillMount(f => f.backend.$.props >>= { p =>
      p.model().operator.map(op => f.backend.updateStateOp(op.value.some, Callback.empty)).getOrEmpty *>
      p.model().observer.map(ob => f.backend.updateStateOp(ob.observer.map(_.value), Callback.empty)).getOrEmpty
    })
    .componentDidMount(_.backend.setupTimer)
    .componentWillReceiveProps { f =>
      val operator = f.nextProps.model().operator
      val observer = f.nextProps.model().observer.flatMap(_.observer)
      // Update the operator field
      Callback.when((operator =!= f.state.operatorText.map(Operator.apply)) && operator.nonEmpty)(f.modState(_.copy(operatorText = operator.map(_.value)))) *>
      Callback.when((observer =!= f.state.observerText.map(Observer.apply)) && observer.nonEmpty)(f.modState(_.copy(observerText = observer.map(_.value))))
    }
    .build

  def apply(model: ModelProxy[HeaderSideBarFocus]): Unmounted[Props, State, Backend] =
    component(Props(model))
}
