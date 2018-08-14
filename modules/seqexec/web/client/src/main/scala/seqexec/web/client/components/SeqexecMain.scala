// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components

import diode.react.ModelProxy
import diode.react.ReactPot._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.extra.router._
import japgolly.scalajs.react.ScalaComponent
import japgolly.scalajs.react.component.Scala.Unmounted
import gem.enum.Site
import seqexec.web.client.circuit.SeqexecCircuit
import seqexec.web.client.model.Pages._
import seqexec.web.client.components.sequence.{HeadersSideBar, SequenceArea}
import seqexec.web.client.model.WebSocketConnection
import web.client.style._

object AppTitle {
  final case class Props(site: Site, ws: ModelProxy[WebSocketConnection])

  private val component = ScalaComponent.builder[Props]("SeqexecUI")
    .stateless
    .render_P(p =>
      <.div(
        ^.cls := "ui row",
        SeqexecStyles.shorterRow,
        SeqexecStyles.notInMobile,
        <.h4(
          ^.cls := "ui horizontal divider header",
          s"Seqexec ${p.site.shortName}",
          p.ws().ws.renderPending(_ =>
            <.div(
              SeqexecStyles.errorText,
              SeqexecStyles.blinking,
              "Connection lost"
            )
          )
        )
      )
    ).build

  def apply(site: Site, ws: ModelProxy[WebSocketConnection]): Unmounted[Props, Unit, Unit] = component(Props(site, ws))
}

object SeqexecMain {
  final case class Props(site: Site, ctl: RouterCtl[SeqexecPages])

  private val lbConnect = SeqexecCircuit.connect(_.uiModel.loginBox)
  private val logConnect = SeqexecCircuit.connect(_.uiModel.globalLog)
  private val resourcesBusyConnect = SeqexecCircuit.connect(_.uiModel.resourceConflict)
  private val headerSideBarConnect = SeqexecCircuit.connect(SeqexecCircuit.headerSideBarReader)
  private val wsConnect = SeqexecCircuit.connect(_.ws)

  private val component = ScalaComponent.builder[Props]("SeqexecUI")
    .stateless
    .render_P(p =>
      <.div(
        <.div(
          ^.cls := "ui horizontally padded grid",
          <.div(
            ^.cls := "ui row",
            SeqexecStyles.shorterRow
          ),
          wsConnect(ws => AppTitle(p.site, ws)),
          <.div(
            ^.cls := "ui row",
            SeqexecStyles.shorterRow,
            SeqexecStyles.queueAreaRow,
            <.div(
              ^.cls := "sixteen wide mobile ten wide tablet ten wide computer column",
              SeqexecStyles.queueArea,
              QueueTableSection(p.ctl)
            ),
            <.div(
              ^.cls := "six wide column tablet computer only",
              SeqexecStyles.headerSideBarArea,
              headerSideBarConnect(HeadersSideBar.apply)
            )
          ),
          <.div(
            ^.cls := "ui row",
            SeqexecStyles.shorterRow,
            SequenceArea(SequenceArea.Props(p.ctl, p.site))
          ),
          <.div(
            ^.cls := "ui row",
            // Add margin to avoid covering the footer
            SeqexecStyles.logArea,
            logConnect(l => LogArea(p.site, l))
          )
        ),
        lbConnect(LoginBox.apply),
        resourcesBusyConnect(ResourcesBox.apply),
        Footer(p.site)
      )
    ).build

  def apply(site: Site, ctl: RouterCtl[SeqexecPages]): Unmounted[Props, Unit, Unit] = component(Props(site, ctl))
}
