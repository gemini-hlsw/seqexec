// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components

import cats.syntax.all._
import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.Reusability
import japgolly.scalajs.react.ScalaComponent
import japgolly.scalajs.react.vdom.html_<^._
import react.common._
import react.common.implicits._
import react.semanticui.collections.menu._
import react.semanticui.elements.button.Button
import react.semanticui.elements.icon.Icon
import react.semanticui.sizes._
import seqexec.web.client.actions.Logout
import seqexec.web.client.actions.OpenLoginBox
import seqexec.web.client.circuit.SeqexecCircuit
import seqexec.web.client.model.ClientStatus
import seqexec.web.client.reusability._

final case class ControlMenu(status: ClientStatus) extends ReactProps[ControlMenu](ControlMenu.component)

/**
  * Menu with options
  */
object ControlMenu {
  implicit val cmReuse: Reusability[ControlMenu] = Reusability.derive

  private val soundConnect =
    SeqexecCircuit.connect(SeqexecCircuit.soundSettingReader)

  private val openLogin: Callback =
    SeqexecCircuit.dispatchCB(OpenLoginBox)
  private val logout: Callback =
    SeqexecCircuit.dispatchCB(Logout)

  private def loginButton(enabled: Boolean) =
    Button(size = Medium, onClick = openLogin, disabled = !enabled, inverted = true)("Login")

  private def logoutButton(text: String, enabled: Boolean) =
    Button(size = Medium, onClick = logout, icon = true, disabled = !enabled, inverted = true)(
      Icon("sign out"),
      text
    )

  val component = ScalaComponent
    .builder[ControlMenu]("ControlMenu")
    .stateless
    .render_P { p =>
      val status = p.status
      <.div(
        ^.cls := "ui secondary right menu",
        status.u match {
          case Some(u) =>
            Menu(secondary = true, floated = MenuFloated.Right)(
              MenuHeader(clazz =
                SeqexecStyles.notInMobile |+| SeqexecStyles.ui |+| SeqexecStyles.item
              )(
                u.displayName
              ),
              MenuHeader(clazz =
                SeqexecStyles.onlyMobile |+| SeqexecStyles.ui |+| SeqexecStyles.item
              )(
                // Ideally we'd do this with css text-overflow but it is not
                // working properly inside a header item, let's abbreviate in code
                u.displayName
                  .split("\\s")
                  .headOption
                  .map(_.substring(0, 10) + "...")
                  .getOrElse[String]("")
              ),
              MenuItem(clazz = SeqexecStyles.notInMobile)(
                soundConnect(x => SoundControl(x())),
                logoutButton("Logout", status.isConnected)
              ),
              MenuItem(clazz = SeqexecStyles.onlyMobile)(
                logoutButton("", status.isConnected)
              )
            )
          case None =>
            MenuItem()(
              soundConnect(x => SoundControl(x())),
              loginButton(status.isConnected)
            )
        }
      )
    }
    .configure(Reusability.shouldComponentUpdate)
    .build

}
