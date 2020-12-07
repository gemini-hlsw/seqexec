// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components

import cats.syntax.all._
import diode.react.ReactPot._
import japgolly.scalajs.react.React
import japgolly.scalajs.react.Reusability
import japgolly.scalajs.react.ScalaComponent
import japgolly.scalajs.react.extra.router._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.enum.Site
import react.common._
import react.common.implicits._
import react.semanticui.collections.grid._
import react.semanticui.elements.divider.Divider
import react.semanticui.toasts._
import react.semanticui.widths._
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
    .builder[Props]("SeqexecTitle")
    .stateless
    .render_P(p =>
      Divider(as = "h4",
              horizontal = true,
              clazz =
                SeqexecStyles.titleRow |+| SeqexecStyles.notInMobile |+| SeqexecStyles.header
      )(
        s"Seqexec ${p.site.shortName}",
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
  private val logConnect              = SeqexecCircuit.connect(_.uiModel.globalLog)
  private val userNotificationConnect = SeqexecCircuit.connect(_.uiModel.notification)
  private val userPromptConnect       = SeqexecCircuit.connect(_.uiModel.userPrompt)
  private val headerSideBarConnect    = SeqexecCircuit.connect(SeqexecCircuit.headerSideBarReader)
  private val wsConnect               = SeqexecCircuit.connect(_.ws)

  private val component = ScalaComponent
    .builder[Props]("SeqexecUI")
    .stateless
    .render_P(p =>
      React.Fragment(
        SemanticToastContainer(position = ContainerPosition.BottomRight,
                               animation = SemanticAnimation.FadeUp,
                               clazz = SeqexecStyles.Toast
        ),
        Grid(padded = GridPadded.Horizontally)(
          GridRow(clazz = SeqexecStyles.shorterRow),
          wsConnect(ws => AppTitle(p.site, ws())),
          GridRow(clazz = SeqexecStyles.shorterRow |+| SeqexecStyles.queueAreaRow)(
            GridColumn(mobile = Sixteen,
                       tablet = Ten,
                       computer = Ten,
                       clazz = SeqexecStyles.queueArea
            )(
              SessionQueueTableSection(p.ctl)
            ),
            GridColumn(tablet = Six, computer = Six, clazz = SeqexecStyles.headerSideBarArea)(
              headerSideBarConnect(x => HeadersSideBar(x()))
            )
          ),
          GridRow(clazz = SeqexecStyles.shorterRow)(
            TabsArea(p.ctl, p.site)
          ),
          GridRow(clazz = SeqexecStyles.logArea)(
            logConnect(l => LogArea(p.site, l()))
          )
        ),
        lbConnect(p => LoginBox(p())),
        userNotificationConnect(p => UserNotificationBox(p())),
        userPromptConnect(p => UserPromptBox(p())),
        Footer(p.ctl, p.site)
      )
    )
    .configure(Reusability.shouldComponentUpdate)
    .build

}
