// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components

import diode.react.ModelProxy
import cats.implicits._
import cats.Eq
import cats.Show
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.BackendScope
import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.ScalaComponent
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.extra.TimerSupport
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.html.Div
import scala.concurrent.duration._
import react.common.implicits._
import seqexec.model.enum.CloudCover
import seqexec.model.enum.ImageQuality
import seqexec.model.enum.SkyBackground
import seqexec.model.enum.WaterVapor
import seqexec.model.Observer
import seqexec.model.Operator
import seqexec.web.client.semanticui.elements.dropdown.DropdownMenu
import seqexec.web.client.semanticui.elements.label.FormLabel
import seqexec.web.client.semanticui.elements.input.InputEV
import seqexec.web.client.circuit.HeaderSideBarFocus
import seqexec.web.client.circuit.SeqexecCircuit
import seqexec.web.client.circuit.SequenceObserverFocus
import seqexec.web.client.circuit.DayCalObserverFocus
import seqexec.web.client.actions.UpdateCloudCover
import seqexec.web.client.actions.UpdateCalTabObserver
import seqexec.web.client.actions.UpdateImageQuality
import seqexec.web.client.actions.UpdateOperator
import seqexec.web.client.actions.UpdateDefaultObserver
import seqexec.web.client.actions.UpdateObserver
import seqexec.web.client.actions.UpdateSkyBackground
import seqexec.web.client.actions.UpdateWaterVapor

/**
  * Display to show headers per sequence
  */
object HeadersSideBar {

  implicit val showSkyBackground: Show[SkyBackground] =
    Show.show(_.label)

  implicit val showWaterVapor: Show[WaterVapor] =
    Show.show(_.label)

  implicit val showCloudCover: Show[CloudCover] =
    Show.show(_.label)

  implicit val showImageQuality: Show[ImageQuality] =
    Show.show(_.label)

  final case class Props(model: ModelProxy[HeaderSideBarFocus]) {
    def canOperate: Boolean = model().status.canOperate
    def selectedObserver
      : Either[Observer, Either[DayCalObserverFocus, SequenceObserverFocus]] =
      model().observer
  }

  final case class State(operatorText: Option[String],
                         observerText: Option[String])

  object State {
    implicit val equals: Eq[State] = Eq.fromUniversalEquals
  }

  class Backend(val $ : BackendScope[Props, State]) extends TimerSupport {
    def updateOperator(name: String): Callback =
      $.props >>= { p =>
        Callback.when(p.canOperate)(
          p.model.dispatchCB(UpdateOperator(Operator(name))))
      }

    def updateObserver(name: String): Callback =
      $.props >>= { p =>
        Callback.when(p.canOperate) {
          p.selectedObserver match {
            case Right(Right(a)) =>
              p.model.dispatchCB(UpdateObserver(a.obsId, Observer(name)))
            case Right(Left(_)) =>
              p.model.dispatchCB(UpdateCalTabObserver(Observer(name)))
            case Left(_) =>
              p.model.dispatchCB(UpdateDefaultObserver(Observer(name)))
          }
        }
      }

    def updateStateOp(value: Option[String], cb: Callback): Callback =
      $.modState(_.copy(operatorText = value)) >> cb

    def updateStateOb(value: Option[String], cb: Callback): Callback =
      $.modState(_.copy(observerText = value)) >> cb

    def setupTimer: Callback =
      // Every 2 seconds check if the field has changed and submit
      setInterval(submitIfChangedOp *> submitIfChangedOb, 2.second)

    def submitIfChangedOp: Callback =
      ($.state.zip($.props)) >>= {
        case (s, p) =>
          Callback.when(
            p.model().operator =!= s.operatorText.map(Operator.apply))(
            updateOperator(s.operatorText.getOrElse("")))
      }

    def submitIfChangedOb: Callback =
      ($.state.zip($.props)) >>= {
        case (s, p) =>
          p.selectedObserver match {
            case Right(Right(a)) =>
              Callback.when(
                a.observer.forall(
                  _.some =!= s.observerText.map(Observer.apply)))(
                updateObserver(s.observerText.getOrElse("")))
            case Right(Left(a)) =>
              Callback.when(
                a.observer.forall(
                  _.some =!= s.observerText.map(Observer.apply)))(
                updateObserver(s.observerText.getOrElse("")))
            case Left(o) =>
              Callback.when(o.some =!= s.observerText.map(Observer.apply))(
                updateObserver(s.observerText.getOrElse("")))
          }
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
      val operatorEV =
        StateSnapshot(s.operatorText.getOrElse(""))(updateStateOp)
      val observerEV =
        StateSnapshot(s.observerText.getOrElse(""))(updateStateOb)
      val instrument = p.selectedObserver
        .map(i => i.fold(_ => "Daycal", _.instrument.show))
        .getOrElse("Default")
      val obsCompleted =
        p.selectedObserver.map(_.fold(_ => false, _.completed)).getOrElse(false)
      val observerField = s"Observer - $instrument"
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
                InputEV.Props("operator",
                              "operator",
                              operatorEV,
                              placeholder = "Operator...",
                              disabled    = !enabled,
                              onBlur      = _ => submitIfChangedOp)
              )
            ),
            <.div(
              ^.cls := "eight wide field",
              FormLabel(FormLabel.Props(observerField, Some("observer"))),
              InputEV(
                InputEV.Props("observer",
                              "observer",
                              observerEV,
                              placeholder = "Observer...",
                              disabled    = !enabled || obsCompleted,
                              onBlur      = _ => submitIfChangedOb)
              )
            )
          ),
          <.div(
            ^.cls := "two fields",
            SeqexecStyles.fieldsNoBottom,
            DropdownMenu(
              DropdownMenu.Props("Image Quality",
                                 p.model().conditions.iq.some,
                                 "Select",
                                 ImageQuality.ImageQualityEnumerated.all,
                                 disabled = !enabled,
                                 iqChanged)),
            DropdownMenu(
              DropdownMenu.Props("Cloud Cover",
                                 p.model().conditions.cc.some,
                                 "Select",
                                 CloudCover.CloudCoverEnumerated.all,
                                 disabled = !enabled,
                                 ccChanged))
          ),
          <.div(
            ^.cls := "two fields",
            SeqexecStyles.fieldsNoBottom,
            DropdownMenu(
              DropdownMenu.Props("Water Vapor",
                                 p.model().conditions.wv.some,
                                 "Select",
                                 WaterVapor.WaterVaporEnumerated.all,
                                 disabled = !enabled,
                                 wvChanged)),
            DropdownMenu(
              DropdownMenu.Props("Sky Background",
                                 p.model().conditions.sb.some,
                                 "Select",
                                 SkyBackground.SkyBackgroundEnumerated.all,
                                 disabled = !enabled,
                                 sbChanged))
          )
        )
      )
    }
  }

  private val component = ScalaComponent
    .builder[Props]("HeadersSideBar")
    .initialState(State(None, None))
    .renderBackend[Backend]
    .configure(TimerSupport.install)
    .componentWillMount(f =>
      f.backend.$.props >>= { p =>
        p.model()
          .operator
          .map(op => f.backend.updateStateOp(op.value.some, Callback.empty))
          .getOrEmpty *>
          (p.selectedObserver match {
            case Right(Right(a)) =>
              f.backend.updateStateOb(a.observer.map(_.value), Callback.empty)
            case Right(Left(a)) =>
              f.backend.updateStateOb(a.observer.map(_.value), Callback.empty)
            case Left(o) =>
              f.backend.updateStateOb(o.value.some, Callback.empty)
          })
    })
    .componentDidMount(_.backend.setupTimer)
    .componentWillReceiveProps { f =>
      val operator = f.nextProps.model().operator
      val observer = f.nextProps.selectedObserver match {
        case Right(Right(a)) => a.observer
        case Right(Left(a))  => a.observer
        case Left(o)         => o.some
      }
      // Update the operator and observator fields
      Callback.when((operator =!= f.state.operatorText
        .map(Operator.apply)) && operator.nonEmpty)(
        f.modState(_.copy(operatorText = operator.map(_.value)))) *>
        Callback.when((observer =!= f.state.observerText
          .map(Observer.apply)) && observer.nonEmpty)(
          f.modState(_.copy(observerText = observer.map(_.value))))
    }
    .build

  def apply(
    model: ModelProxy[HeaderSideBarFocus]): Unmounted[Props, State, Backend] =
    component(Props(model))
}
