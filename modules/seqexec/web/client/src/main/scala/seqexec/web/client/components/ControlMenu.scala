// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components

import seqexec.web.client.actions.Logout
import seqexec.web.client.actions.OpenLoginBox
import seqexec.web.client.model.ClientStatus
import seqexec.web.client.circuit.SeqexecCircuit
import seqexec.web.client.semanticui.Size
import seqexec.web.client.semanticui.elements.button.Button
import seqexec.web.client.semanticui.elements.icon.Icon.IconSignOut
import react.common.implicits._
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.ScalaComponent
import japgolly.scalajs.react.vdom.html_<^._

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

  private val component = ScalaComponent
    .builder[Props]("SeqexecTopMenu")
    .stateless
    .render_P { p =>
      val status = p.status
      <.div(
        ^.cls := "ui secondary right menu",
        status.u.fold(
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
                ^.cls := "ui header item",
                SeqexecStyles.notInMobile,
                u.displayName
              ),
              <.div(
                ^.cls := "ui header item",
                SeqexecStyles.onlyMobile,
                // Ideally we'd do this with css text-overflow but it is not
                // working properly inside a header item, let's abbreviate in code
                u.displayName
                  .split("\\s")
                  .headOption
                  .map(_.substring(0, 10) + "...")
                  .getOrElse[String]("")
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
