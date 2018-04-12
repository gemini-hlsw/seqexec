// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.web.client.components

import edu.gemini.seqexec.web.client.circuit.SeqexecCircuit
import edu.gemini.seqexec.web.client.actions.WSConnect
import edu.gemini.seqexec.web.client.model.Pages._
import edu.gemini.seqexec.web.client.actions.{NavigateSilentTo, RequestSoundEcho}
import edu.gemini.seqexec.web.client.components.sequence.{HeadersSideBar, SequenceArea}
import edu.gemini.seqexec.model.Model.SeqexecSite
import edu.gemini.seqexec.web.client.model.WebSocketConnection
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.extra.router._
import japgolly.scalajs.react.{Callback, ScalaComponent}
import diode.ModelRO
import diode.react.ModelProxy
import diode.react.ReactPot._
import japgolly.scalajs.react.component.Scala.Unmounted
import scalacss.ScalaCssReact._
import cats.implicits._
import cats.effect.IO
import scala.scalajs.js.timers.SetTimeoutHandle

object AppTitle {
  final case class Props(site: SeqexecSite, ws: ModelProxy[WebSocketConnection])

  private val component = ScalaComponent.builder[Props]("SeqexecUI")
    .stateless
    .render_P(p =>
      <.div(
        ^.cls := "ui row",
        SeqexecStyles.shorterRow,
        SeqexecStyles.notInMobile,
        <.h4(
          ^.cls := "ui horizontal divider header",
          s"Seqexec ${p.site.show}",
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

  def apply(site: SeqexecSite, ws: ModelProxy[WebSocketConnection]): Unmounted[Props, Unit, Unit] = component(Props(site, ws))
}

object SeqexecMain {
  final case class Props(site: SeqexecSite, ctl: RouterCtl[SeqexecPages])

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
            SeqexecStyles.sequenceArea,
            SequenceArea(p.ctl, p.site)
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

  def apply(site: SeqexecSite, ctl: RouterCtl[SeqexecPages]): Unmounted[Props, Unit, Unit] = component(Props(site, ctl))
}

/**
  * Top level UI component
  */
object SeqexecUI {
  final case class RouterProps(page: InstrumentPage, router: RouterCtl[InstrumentPage])

  def pageTitle(site: SeqexecSite)(p: SeqexecPages): String = p match {
    case SequenceConfigPage(_, id, _) => s"Seqexec - $id"
    case SequencePage(_, id, _)       => s"Seqexec - $id"
    case InstrumentPage(i)            => s"Seqexec - ${i.show}"
    case _                            => s"Seqexec - ${site.show}"
  }

  @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
  def router(site: SeqexecSite): IO[Router[SeqexecPages]] = {
    val instrumentNames = site.instruments.map(i => (i.show, i)).toList.toMap

    val routerConfig = RouterConfigDsl[SeqexecPages].buildConfig { dsl =>
      import dsl._

      (emptyRule
      | staticRoute(root, Root) ~> renderR(r => SeqexecMain(site, r))
      | staticRoute("/soundtest", SoundTest) ~> renderR(r => SeqexecMain(site, r))
      | dynamicRoute(("/" ~ string("[a-zA-Z0-9-]+") ~ "/" ~ string("[a-zA-Z0-9-]+") ~ "/configuration/" ~ int)
        .pmap {
          case (i, s, step) => instrumentNames.get(i).map(SequenceConfigPage(_, s, step))
        }(p => (p.instrument.show, p.obsId, p.step))) {
          case x @ SequenceConfigPage(i, _, _) if site.instruments.toList.contains(i) => x
        } ~> dynRenderR((p, r) => SeqexecMain(site, r))
      | dynamicRoute(("/" ~ string("[a-zA-Z0-9-]+") ~ "/" ~ string("[a-zA-Z0-9-]+"))
        .pmap {
          case (i, s) => instrumentNames.get(i).map(SequencePage(_, s, 0))
        }(p => (p.instrument.show, p.obsId))) {
          case x @ SequencePage(i, _, _) if site.instruments.toList.contains(i) => x
        } ~> dynRenderR((p, r) => SeqexecMain(site, r))
      | dynamicRoute(("/" ~ string("[a-zA-Z0-9-]+"))
        .pmap(i => instrumentNames.get(i).map(InstrumentPage))(p => p.instrument.show)) {
          case x @ InstrumentPage(i) if site.instruments.toList.contains(i) => x
        } ~> dynRenderR((p, r) => SeqexecMain(site, r))
      )
        .notFound(redirectToPage(Root)(Redirect.Push))
        // Runtime verification that all pages are routed
        .verify(Root, SoundTest :: site.instruments.toList.map(i => InstrumentPage(i)): _*)
        .onPostRender((_, next) =>
          Callback.when(next === SoundTest)(SeqexecCircuit.dispatchCB(RequestSoundEcho)) >>
          Callback.when(next =!= SeqexecCircuit.zoom(_.uiModel.navLocation).value)(SeqexecCircuit.dispatchCB(NavigateSilentTo(next))))
        .renderWith { case (_, r) => <.div(r.render()).render}
        .setTitle(pageTitle(site))
        .logToConsole
    }

    def navigated(routerLogic: RouterLogic[SeqexecPages], page: ModelRO[SeqexecPages]): SetTimeoutHandle = {
      scalajs.js.timers.setTimeout(0)(routerLogic.ctl.set(page.value).runNow())
    }

    for {
      r                     <- IO(Router.componentAndLogic(BaseUrl.fromWindowOrigin, routerConfig))
      (router, routerLogic) = r
      // subscribe to navigation changes
      _                     <- IO(SeqexecCircuit.subscribe(SeqexecCircuit.zoom(_.uiModel.navLocation))(x => {navigated(routerLogic, x);()}))
        // Initiate the WebSocket connection
      _                     <- IO(SeqexecCircuit.dispatch(WSConnect(0)))
    } yield router
  }

}
