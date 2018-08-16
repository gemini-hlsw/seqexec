// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components

import diode.react.ModelProxy
import seqexec.web.client.actions.{Logout, OpenLoginBox}
import seqexec.web.client.model.ClientStatus
import seqexec.web.client.semanticui.Size
import seqexec.web.client.semanticui.elements.button.Button
import seqexec.web.client.semanticui.elements.icon.Icon.IconSignOut
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.{Callback, ScalaComponent}
import japgolly.scalajs.react.vdom.html_<^._
import web.client.style._

/**
  * Menu with options
  */
object ControlMenu {

  final case class Props(status: ModelProxy[ClientStatus])

  def openLogin[A](proxy: ModelProxy[A]): Callback = proxy.dispatchCB(OpenLoginBox)
  def logout[A](proxy: ModelProxy[A]): Callback = proxy.dispatchCB(Logout)

  private def loginButton[A](proxy: ModelProxy[A], enabled: Boolean) =
    Button(Button.Props(size = Size.Medium, onClick = openLogin(proxy), disabled = !enabled, inverted = true), "Login")

  private def logoutButton[A](proxy: ModelProxy[A], text: String, enabled: Boolean) =
    Button(Button.Props(size = Size.Medium, onClick = logout(proxy), icon = Some(IconSignOut), disabled = !enabled, inverted = true), text)

  private val component = ScalaComponent.builder[Props]("SeqexecTopMenu")
    .stateless
    .render_P { p =>
      val status = p.status()
      <.div(
        ^.cls := "ui secondary right menu",
        SeqexecStyles.notInMobile,
        status.u.fold(
          <.div(
            ^.cls := "ui item",
            loginButton(p.status, status.isConnected)
          )
        )(u =>
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
              u.displayName.split("\\s").headOption.map(_.substring(0, 10) + "...").getOrElse[String]("")
            ),
            <.div(
              ^.cls := "ui item",
              SeqexecStyles.notInMobile,
              logoutButton(p.status, "Logout", status.isConnected)
            ),
            <.div(
              ^.cls := "ui item",
              SeqexecStyles.onlyMobile,
              logoutButton(p.status, "", status.isConnected)
            )
          )
        )
      )
    }
    .build

  def apply(u: ModelProxy[ClientStatus]): Unmounted[Props, Unit, Unit] = component(Props(u))
}
