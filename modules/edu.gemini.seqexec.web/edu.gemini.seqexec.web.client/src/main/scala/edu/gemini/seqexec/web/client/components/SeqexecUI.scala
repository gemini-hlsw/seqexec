package edu.gemini.seqexec.web.client.components

import edu.gemini.seqexec.web.client.model.{SeqexecCircuit, WSConnect}
import edu.gemini.seqexec.web.client.model.Pages._
import edu.gemini.seqexec.web.client.model.NavigateSilentTo
import edu.gemini.seqexec.web.client.components.sequence.SequenceArea
import edu.gemini.seqexec.model.Model.SeqexecSite
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.extra.router._
import japgolly.scalajs.react.{Callback, ScalaComponent}
import diode.ModelRO

import scala.scalajs.js.timers.SetTimeoutHandle

import scalaz._
import Scalaz._

object SeqexecMain {
  case class Props(site: SeqexecSite, ctl: RouterCtl[SeqexecPages])

  private val lbConnect = SeqexecCircuit.connect(_.uiModel.loginBox)
  private val logConnect = SeqexecCircuit.connect(_.uiModel.globalLog)

  private val component = ScalaComponent.builder[Props]("SeqexecUI")
    .stateless
    .render_P(p =>
      <.div(
        NavBar(p.site),
        QueueArea(p.ctl),
        SequenceArea(p.site),
        logConnect(LogArea.apply),
        lbConnect(LoginBox.apply)
      )
    ).build

  def apply(site: SeqexecSite, ctl: RouterCtl[SeqexecPages]) = component(Props(site, ctl))
}

/**
  * Top level UI component
  */
object SeqexecUI {
  case class RouterProps(page: InstrumentPage, router: RouterCtl[InstrumentPage])

  def router(site: SeqexecSite): Router[SeqexecPages] = {
    val instrumentNames = site.instruments.map(i => (i.shows, i)).list.toList.toMap

    val routerConfig = RouterConfigDsl[SeqexecPages].buildConfig { dsl =>
      import dsl._

      (emptyRule
      | staticRoute(root, Root) ~> renderR(r => SeqexecMain(site, r))
      | dynamicRoute(("/" ~ string("[a-zA-Z0-9-]+") ~ "/" ~ string("[a-zA-Z0-9-]+").option)
        .pmap {
          case (i, Some(s)) => instrumentNames.get(i).map(InstrumentPage(_, s.some))
          case (i, None)    => instrumentNames.get(i).map(InstrumentPage(_, none))
        }(p => (p.instrument.shows, p.obsId))) {
          case x @ InstrumentPage(i, _) if site.instruments.list.toList.contains(i) => x
        } ~> dynRenderR((p, r) => SeqexecMain(site, r))
      | dynamicRoute(("/" ~ string("[a-zA-Z0-9-]+")).pmap(i => instrumentNames.get(i).map(InstrumentPage(_, None)))(p => p.instrument.shows)) {
          case x @ InstrumentPage(i, _) if site.instruments.list.toList.contains(i) => x
        } ~> dynRenderR((p, r) => SeqexecMain(site, r))
      )
        .notFound(redirectToPage(Root)(Redirect.Push))
        // Runtime verification that all pages are routed
        .verify(Root, site.instruments.list.toList.map(i => InstrumentPage(i, None)): _*)
        .onPostRender((_, next) =>
          Callback.when(next != SeqexecCircuit.zoom(_.uiModel.navLocation).value)(Callback(SeqexecCircuit.dispatch(NavigateSilentTo(next)))))
        .renderWith { case (_, r) => <.div(r.render()).render}
        .logToConsole
    }

    val (router, routerLogic) = Router.componentAndLogic(BaseUrl.fromWindowOrigin, routerConfig)

    def navigated(page: ModelRO[SeqexecPages]): SetTimeoutHandle = {
      scalajs.js.timers.setTimeout(0)(routerLogic.ctl.set(page.value).runNow())
    }

    // subscribe to navigation changes
    SeqexecCircuit.subscribe(SeqexecCircuit.zoom(_.uiModel.navLocation))(x => {navigated(x); ()})

    // Initiate the WebSocket connection
    SeqexecCircuit.dispatch(WSConnect(0))

    router
  }

}
