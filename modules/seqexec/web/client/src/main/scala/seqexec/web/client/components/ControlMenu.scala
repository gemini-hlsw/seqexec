// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components

import seqexec.web.client.actions.{Logout, OpenLoginBox, UpdateDefaultObserver}
import seqexec.web.client.model.ClientStatus
import seqexec.web.client.circuit.SeqexecCircuit
import seqexec.web.client.semanticui.Size
import seqexec.web.client.semanticui.elements.button.Button
import seqexec.web.client.semanticui.elements.icon.Icon.IconSignOut
import react.common.implicits._
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.ScalaComponent
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.vdom.html_<^._
import seqexec.model.Observer
import seqexec.web.client.semanticui.elements.input.InputEV
import seqexec.web.client.semanticui.elements.label.FormLabel

/**
  * Menu with options
  */
object ControlMenu {

  final case class Props(status: ClientStatus)

  private val soundConnect =
    SeqexecCircuit.connect(SeqexecCircuit.soundSettingReader)

  private val openLogin: Callback =
    SeqexecCircuit.dispatchCB(OpenLoginBox)
  private val logout: Callback =
    SeqexecCircuit.dispatchCB(Logout)

  private def loginButton(enabled: Boolean) =
    Button(size     = Size.Medium,
           onClick  = openLogin,
           disabled = !enabled,
           inverted = true)("Login")

  private def logoutButton(text: String, enabled: Boolean) =
    Button(size     = Size.Medium,
           onClick  = logout,
           icon     = Some(IconSignOut),
           disabled = !enabled,
           inverted = true)(text)


  // Ideally we'd do this with css text-overflow but it is not
  // working properly inside a header item, let's abbreviate in code
  private def overflowText(text: String): String =
    text
      .split("\\s")
      .headOption
      .map(_.substring(0, 10) + "...")
      .getOrElse[String]("")

  private val component = ScalaComponent
    .builder[Props]("SeqexecTopMenu")
    .stateless
    .render_P { p =>
      val status = p.status

      val enabled = p.status.canOperate
      def updateStateOb(value: Option[String], cb: Callback): Callback =
        SeqexecCircuit.dispatchCB(UpdateDefaultObserver(value.fold(Observer.Zero)(Observer.apply))) >> cb
      val observerEV =
        StateSnapshot(p.status.defaultObserver.value)(updateStateOb)

      <.div(
        ^.cls := "ui secondary right menu",
        status.userDetails.fold(
          <.div(
            ^.cls := "ui item",
            soundConnect(x => SoundControl(SoundControl.Props(x()))),
            loginButton(status.isConnected)
          )
        )(
          u =>
            <.div(
              ^.cls := "ui secondary right menu",
              <.div(
                ^.cls := "ui form item",
                SeqexecStyles.notInMobile,
                FormLabel(FormLabel.Props("Observer:", Some("observer"))),
                <.div(
                  ^.cls := "ui input",
                  SeqexecStyles.defaultObserverEditField,
                  InputEV(
                    InputEV.Props("observer",
                                  "observer",
                                  observerEV,
                                  placeholder = "Observer...",
                                  disabled    = !enabled)
                  )
                )
              ),
              <.div(
                ^.cls := "ui item",
                SeqexecStyles.onlyMobile,
                s"Observer: ${overflowText(status.defaultObserver.value)}"
              ),
              <.div(
                ^.cls := "ui header item",
                SeqexecStyles.notInMobile,
                s"User: ${u.displayName}"
              ),
              <.div(
                ^.cls := "ui header item",
                SeqexecStyles.onlyMobile,
                s"User: ${overflowText(u.displayName)}"
              ),
              <.div(
                ^.cls := "ui item",
                SeqexecStyles.notInMobile,
                soundConnect(x => SoundControl(SoundControl.Props(x()))),
                logoutButton("Logout", status.isConnected)
              ),
              <.div(
                ^.cls := "ui item",
                SeqexecStyles.onlyMobile,
                logoutButton("", status.isConnected)
              )
            )
        )
      )
    }
    .build

  def apply(u: ClientStatus): Unmounted[Props, Unit, Unit] =
    component(Props(u))
}
