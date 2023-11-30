// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components

import cats.syntax.all._
import diode.react.ReactPot._
import japgolly.scalajs.react.React
import japgolly.scalajs.react.Reusability
import japgolly.scalajs.react.ScalaComponent
import japgolly.scalajs.react.extra.router._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.enums.Site
import react.common._
import react.common.implicits._
import react.semanticui.elements.divider.Divider
import seqexec.web.client.circuit.SeqexecCircuit
import seqexec.web.client.components.tabs.TabsArea
import seqexec.web.client.model.Pages._
import seqexec.web.client.model.WebSocketConnection
import seqexec.web.client.reusability._

final case class AppTitle(site: Site, ws: WebSocketConnection)
    extends ReactProps[AppTitle](AppTitle.component)

object AppTitle {
  type Props = AppTitle

  implicit val propsReuse: Reusability[Props] = Reusability.derive[Props]

  private val component = ScalaComponent
    .builder[Props]
    .stateless
    .render_P(p =>
      Divider(as = "h4",
              horizontal = true,
              clazz = SeqexecStyles.titleRow |+| SeqexecStyles.notInMobile |+| SeqexecStyles.header
      )(
        s"Seqexec ${p.site.shortName}",
        // TODO: Remove me
        <.div(^.cls := "ghost-only", "GHOST ONLY!"),
        p.ws.ws.renderPending(_ =>
          <.div(
            SeqexecStyles.errorText,
            SeqexecStyles.blinking,
            "Connection lost"
          )
        )
      )
    )
    .configure(Reusability.shouldComponentUpdate)
    .build

}

final case class SeqexecMain(site: Site, ctl: RouterCtl[SeqexecPages])
    extends ReactProps[SeqexecMain](SeqexecMain.component)

object SeqexecMain {
  type Props = SeqexecMain

  implicit val propsReuse: Reusability[Props] = Reusability.by(_.site)

  private val lbConnect               = SeqexecCircuit.connect(_.uiModel.loginBox)
  private val userNotificationConnect = SeqexecCircuit.connect(_.uiModel.notification)
  private val userPromptConnect       = SeqexecCircuit.connect(_.uiModel.userPrompt)

  private val headerSideBarConnect = SeqexecCircuit.connect(SeqexecCircuit.headerSideBarReader)
  private val logConnect           = SeqexecCircuit.connect(_.uiModel.globalLog)
  private val wsConnect            = SeqexecCircuit.connect(_.ws)

  private val component = ScalaComponent
    .builder[Props]
    .stateless
    .render_P(p =>
      React.Fragment(
        <.div(SeqexecStyles.MainUI)(
          wsConnect(ws => AppTitle(p.site, ws())),
          <.div(SeqexecStyles.queueAreaRow)(
            <.div(SeqexecStyles.queueArea)(
              SessionQueueTableSection(p.ctl)
            ),
            <.div(SeqexecStyles.headerSideBarArea)(
              headerSideBarConnect(x => HeadersSideBar(x()))
            )
          ),
          TabsArea(p.ctl, p.site),
          <.div(SeqexecStyles.logArea)(
            logConnect(l => LogArea(p.site, l()))
          ),
          Footer(p.ctl, p.site)
        ),
        lbConnect(p => LoginBox(p())),
        userNotificationConnect(p => UserNotificationBox(p())),
        userPromptConnect(p => UserPromptBox(p()))
      )
    )
    .configure(Reusability.shouldComponentUpdate)
    .build

}
