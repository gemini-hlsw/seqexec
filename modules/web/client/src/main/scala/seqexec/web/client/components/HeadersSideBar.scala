// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components

import scala.concurrent.duration._

import cats.Eq
import cats.syntax.all._
import japgolly.scalajs.react.ReactCats._
import japgolly.scalajs.react.ReactMonocle._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.extra.TimerSupport
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.util.Display
import lucuma.ui.forms._
import lucuma.ui.input.InputFormat
import monocle.macros.Lenses
import react.common._
import react.semanticui.collections.form._
import react.semanticui.elements.segment.Segment
import react.semanticui.widths._
import seqexec.model.Operator
import seqexec.model.enum.CloudCover
import seqexec.model.enum.ImageQuality
import seqexec.model.enum.SkyBackground
import seqexec.model.enum.WaterVapor
import seqexec.web.client.actions._
import seqexec.web.client.circuit._
import seqexec.web.client.components.forms.FormLabel
import seqexec.web.client.reusability._

/**
 * Container for a table with the steps
 */
final case class HeadersSideBar(model: HeaderSideBarFocus)
    extends ReactProps[HeadersSideBar](HeadersSideBar.component) {

  def canOperate: Boolean = model.status.canOperate

}

/**
 * Display to show headers per sequence
 */
object HeadersSideBar {
  implicit val eqHeadersSideBar: Eq[HeadersSideBar]    = Eq.by(_.model)
  implicit val propsReuse: Reusability[HeadersSideBar] = Reusability.byEq

  private def conditionIntToString(v: Int): String = if (v === 100) "Any" else v.toString

  implicit val showSkyBackground: Display[SkyBackground] =
    Display.by(_.toInt.map(conditionIntToString).getOrElse("Unknown"), _.label)

  implicit val displayWaterVapor: Display[WaterVapor] =
    Display.by(_.toInt.map(conditionIntToString).getOrElse("Unknown"), _.label)

  implicit val showCloudCover: Display[CloudCover] =
    Display.by(_.toInt.map(conditionIntToString).getOrElse("Unknown"), _.label)

  implicit val showImageQuality: Display[ImageQuality] =
    Display.by(_.toInt.map(conditionIntToString).getOrElse("Unknown"), _.label)

  @Lenses
  final case class State(
    operator:        Option[Operator],
    prevOperator:    Option[Operator],
    displayName:     Option[String],
    prevDisplayName: Option[String]
  )

  object State {
    def apply(operator: Option[Operator], displayName: Option[String]): State =
      State(operator, operator, displayName, displayName)

    implicit val stateEquals: Eq[State] = Eq.by(s => (s.operator, s.displayName))

    implicit val stateReuse: Reusability[State] = Reusability.byEq
  }

  class Backend(val $ : BackendScope[HeadersSideBar, State]) extends TimerSupport {
    private def updateOperator(name: Operator): Callback =
      $.props >>= { p => SeqexecCircuit.dispatchCB(UpdateOperator(name)).when_(p.canOperate) }

    private def updateDisplayName(dn: String): Callback =
      $.props >>= { p =>
        SeqexecCircuit
          .dispatchCB(UpdateDisplayName(p.model.status.user.foldMap(_.username), dn))
          .when_(p.canOperate)
      }

    def updateStateOp(value: Option[Operator], cb: Callback = Callback.empty): Callback =
      $.setStateL(State.operator)(value) >> cb

    def updateStateDN(value: Option[String], cb: Callback = Callback.empty): Callback =
      $.setStateL(State.displayName)(value) >> cb

    def setupTimer: Callback =
      // Every 2 seconds check if the field has changed and submit
      setInterval(submitIfChangedOp *> submitIfChangedDN, 2.second)

    def submitIfChangedDN: Callback =
      ($.state.zip($.props)) >>= { case (s, p) =>
        s.displayName
          .map(updateDisplayName)
          .getOrEmpty
          .when_(p.model.status.displayName =!= s.displayName)
      }

    def submitIfChangedOp: Callback =
      ($.state.zip($.props)) >>= { case (s, p) =>
        s.operator
          .map(updateOperator)
          .getOrEmpty
          .when_(p.model.operator =!= s.operator)
      }

    def iqChanged(iq: ImageQuality): Callback =
      SeqexecCircuit.dispatchCB(UpdateImageQuality(iq))

    def ccChanged(i: CloudCover): Callback =
      SeqexecCircuit.dispatchCB(UpdateCloudCover(i))

    def sbChanged(sb: SkyBackground): Callback =
      SeqexecCircuit.dispatchCB(UpdateSkyBackground(sb))

    def wvChanged(wv: WaterVapor): Callback =
      SeqexecCircuit.dispatchCB(UpdateWaterVapor(wv))

    def render(p: HeadersSideBar, s: State): VdomNode = {
      val enabled    = p.model.status.canOperate
      val operatorEV =
        StateSnapshot[Operator](s.operator.getOrElse(Operator.Zero))(updateStateOp)

      val displayNameEV =
        StateSnapshot[String](s.displayName.orEmpty)(updateStateDN)

      Segment(secondary = true, clazz = SeqexecStyles.headerSideBarStyle)(
        Form()(
          FormGroup(widths = Two, clazz = SeqexecStyles.fieldsNoBottom)(
            <.div(
              ^.cls := "sixteen wide field",
              FormLabel("My observer name", Some("displayName")),
              InputEV[StateSnapshot, String](
                "displayName",
                "displayName",
                displayNameEV,
                placeholder = "Display name...",
                disabled = !enabled,
                onBlur = _ => submitIfChangedDN
              )
            )
          ),
          FormGroup(widths = Two, clazz = SeqexecStyles.fieldsNoBottom)(
            <.div(
              ^.cls := "sixteen wide field",
              FormLabel("Operator", Some("operator")),
              InputEV[StateSnapshot, Operator](
                "operator",
                "operator",
                operatorEV,
                format = InputFormat.fromIso(Operator.valueI.reverse),
                placeholder = "Operator...",
                disabled = !enabled,
                onBlur = _ => submitIfChangedOp
              )
            )
          ),
          FormGroup(widths = Two, clazz = SeqexecStyles.fieldsNoBottom)(
            EnumSelect[ImageQuality](p.model.conditions.iq.some,
                                     "Image Quality",
                                     "Select",
                                     disabled = !enabled,
                                     iqChanged
            ),
            EnumSelect[CloudCover](p.model.conditions.cc.some,
                                   "Cloud Cover",
                                   "Select",
                                   disabled = !enabled,
                                   ccChanged
            )
          ),
          FormGroup(widths = Two, clazz = SeqexecStyles.fieldsNoBottom)(
            EnumSelect[WaterVapor](p.model.conditions.wv.some,
                                   "Water Vapor",
                                   "Select",
                                   disabled = !enabled,
                                   wvChanged
            ),
            EnumSelect[SkyBackground](p.model.conditions.sb.some,
                                      "Sky Background",
                                      "Select",
                                      disabled = !enabled,
                                      sbChanged
            )
          )
        )
      )
    }
  }

  private val component = ScalaComponent
    .builder[HeadersSideBar]
    .getDerivedStateFromPropsAndState[State] { (p, sOpt) =>
      val operator    = p.model.operator
      val displayName = p.model.status.displayName

      sOpt.fold(State(operator, displayName)) { s =>
        Function.chain(
          List(
            State.operator.replace(operator),
            State.prevOperator.replace(operator)
          ).some
            .filter(_ => (operator =!= s.prevOperator) && operator.nonEmpty)
            .orEmpty :::
            List(
              State.displayName.replace(displayName),
              State.prevDisplayName.replace(displayName)
            ).some
              .filter(_ => (displayName =!= s.prevDisplayName) && displayName.nonEmpty)
              .orEmpty
        )(s)
      }
    }
    .renderBackend[Backend]
    .configure(TimerSupport.install)
    .componentDidMount(_.backend.setupTimer)
    .configure(Reusability.shouldComponentUpdate)
    .build

}
