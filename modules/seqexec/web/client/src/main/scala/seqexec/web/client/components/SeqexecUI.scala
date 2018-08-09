// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components

import diode.ModelRO
import diode.react.ModelProxy
import diode.react.ReactPot._
import cats.implicits._
import cats.effect.IO
import monocle.Prism
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.extra.router._
import japgolly.scalajs.react.{Callback, ScalaComponent}
import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react.component.Scala.Unmounted
import gem.Observation
import gem.enum.Site
import scala.scalajs.js.timers.SetTimeoutHandle
import seqexec.web.client.circuit.SeqexecCircuit
import seqexec.web.client.actions.WSConnect
import seqexec.web.client.model.Pages._
import seqexec.web.client.ModelOps._
import seqexec.web.client.actions.{NavigateSilentTo, RequestSoundEcho}
import seqexec.web.client.components.sequence.{HeadersSideBar, SequenceArea}
import seqexec.model.enum.Instrument
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

  def apply(site: Site, ctl: RouterCtl[SeqexecPages]): Unmounted[Props, Unit, Unit] = component(Props(site, ctl))
}

/**
  * Top level UI component
  */
object SeqexecUI {
  final case class RouterProps(page: InstrumentPage, router: RouterCtl[InstrumentPage])

  def pageTitle(site: Site)(p: SeqexecPages): String = p match {
    case SequenceConfigPage(_, id, _) => s"Seqexec - ${id.format}"
    case SequencePage(_, id, _)       => s"Seqexec - ${id.format}"
    case InstrumentPage(i)            => s"Seqexec - ${i.show}"
    case _                            => s"Seqexec - ${site.shortName}"
  }

  // Prism from url params to config page
  def configPageP(instrumentNames: Map[String, Instrument]): Prism[(String, String, Int), SequenceConfigPage] = Prism[(String, String, Int), SequenceConfigPage] {
    case (i, s, step) => (instrumentNames.get(i), Observation.Id.fromString(s)).mapN(SequenceConfigPage(_, _, step))
  } {
    p => (p.instrument.show, p.obsId.format, p.step)
  }
  // Prism from url params to sequence page
  def sequencePageP(instrumentNames: Map[String, Instrument]): Prism[(String, String), SequencePage] = Prism[(String, String), SequencePage] {
    case (i, s) => (instrumentNames.get(i), Observation.Id.fromString(s)).mapN(SequencePage(_, _, 0))
  } {
    p => (p.instrument.show, p.obsId.format)
  }

  // Prism from url params to the preview page
  def previewPageP(instrumentNames: Map[String, Instrument]): Prism[(String, String), PreviewPage] = Prism[(String, String), PreviewPage] {
    case (i, s) => (instrumentNames.get(i), Observation.Id.fromString(s)).mapN(PreviewPage(_, _, 0))
  } {
    p => (p.instrument.show, p.obsId.format)
  }

  // Prism from url params to the preview page with config
  def previewConfigPageP(instrumentNames: Map[String, Instrument]): Prism[(String, String, Int), PreviewConfigPage] = Prism[(String, String, Int), PreviewConfigPage] {
    case (i, s, step) => (instrumentNames.get(i), Observation.Id.fromString(s)).mapN(PreviewConfigPage(_, _, step))
  } {
    p => (p.instrument.show, p.obsId.format, p.step)
  }

  @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
  def router(site: Site): IO[Router[SeqexecPages]] = {
    val instrumentNames = site.instruments.map(i => (i.show, i)).toList.toMap

    val routerConfig = RouterConfigDsl[SeqexecPages].buildConfig { dsl =>
      import dsl._

      (emptyRule
      | staticRoute(root, Root) ~> renderR(r => SeqexecMain(site, r))
      | staticRoute("/soundtest", SoundTest) ~> renderR(r => SeqexecMain(site, r))
      | dynamicRouteCT(("/" ~ string("[a-zA-Z0-9-]+") ~ "/" ~ string("[a-zA-Z0-9-]+") ~ "/configuration/" ~ int)
        .pmapL(configPageP(instrumentNames))) ~> dynRenderR((_: SequenceConfigPage, r) => SeqexecMain(site, r))
      | dynamicRouteCT(("/" ~ string("[a-zA-Z0-9-]+") ~ "/" ~ string("[a-zA-Z0-9-]+"))
        .pmapL(sequencePageP(instrumentNames))) ~> dynRenderR((_: SequencePage, r) => SeqexecMain(site, r))
      | dynamicRouteCT(("/preview/" ~ string("[a-zA-Z0-9-]+") ~ "/" ~ string("[a-zA-Z0-9-]+"))
        .pmapL(previewPageP(instrumentNames))) ~> dynRenderR((_: PreviewPage, r) => SeqexecMain(site, r))
      | dynamicRouteCT(("/preview/" ~ string("[a-zA-Z0-9-]+") ~ "/" ~ string("[a-zA-Z0-9-]+") ~ "/configuration/" ~ int)
        .pmapL(previewConfigPageP(instrumentNames))) ~> dynRenderR((_: PreviewConfigPage, r) => SeqexecMain(site, r))
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
