package edu.gemini.seqexec.web.client.components

import edu.gemini.seqexec.web.client.model.{SeqexecCircuit, WSConnect}
import edu.gemini.seqexec.web.client.model.Pages._
import edu.gemini.seqexec.web.client.model.NavigateSilentTo
import edu.gemini.seqexec.web.client.components.sequence.SequenceArea
import edu.gemini.seqexec.model.Model.Instrument
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.extra.router._
import japgolly.scalajs.react.{Callback, ScalaComponent}
import diode.ModelRO

import scalaz._
import Scalaz._

object SeqexecMain {
  private val lbConnect = SeqexecCircuit.connect(_.uiModel.loginBox)
  private val logConnect = SeqexecCircuit.connect(_.uiModel.globalLog)

  private val component = ScalaComponent.builder[RouterCtl[SeqexecPages]]("SeqexecUI")
    .stateless
    .render_P(p =>
      <.div(
        NavBar(),
        QueueArea(p),
        SequenceArea(),
        logConnect(LogArea.apply),
        lbConnect(LoginBox.apply)
      )
    ).build

  def apply(ctl: RouterCtl[SeqexecPages]) = component(ctl)
}

/**
  * Top level UI component
  */
object SeqexecUI {
  case class RouterProps(page: InstrumentPage, router: RouterCtl[InstrumentPage])

  private val siteInstruments = Instrument.gsInstruments
  private val instrumentNames = siteInstruments.map(i => (i.shows, i)).toIList.toList.toMap

  def router: Router[SeqexecPages] = {
    val routerConfig = RouterConfigDsl[SeqexecPages].buildConfig { dsl =>
      import dsl._

      def layout(c: RouterCtl[SeqexecPages], r: Resolution[SeqexecPages]) =
        <.div(r.render()).render

      (emptyRule
      | staticRoute(root, Root) ~> renderR(r => SeqexecMain(r))
      | dynamicRoute(("/" ~ string("[a-zA-Z0-9-]+") ~ "/" ~ string("[a-zA-Z0-9-]+").option)
        .pmap {
          case (i, Some(s)) => instrumentNames.get(i).map(InstrumentPage(_, s.some))
          case (i, None)    => instrumentNames.get(i).map(InstrumentPage(_, none))
        }(p => (p.instrument.shows, p.obsId))) {
          case x @ InstrumentPage(i, _) if siteInstruments.toIList.toList.contains(i) => x
        } ~> dynRenderR((p, r) => SeqexecMain(r))
      | dynamicRoute(("/" ~ string("[a-zA-Z0-9-]+")).pmap(i => instrumentNames.get(i).map(InstrumentPage(_, None)))(p => p.instrument.shows)) {
          case x @ InstrumentPage(i, _) if siteInstruments.toIList.toList.contains(i) => x
        } ~> dynRenderR((p, r) => SeqexecMain(r))
      )
        .notFound(redirectToPage(Root)(Redirect.Push))
        // Runtime verification that all pages are routed
        .verify(Root, siteInstruments.list.toList.map(i => InstrumentPage(i, None)): _*)
        .onPostRender((_, next) =>
          Callback.when(next != SeqexecCircuit.zoom(_.uiModel.navLocation).value)(Callback.log("silent " + next) >> Callback(SeqexecCircuit.dispatch(NavigateSilentTo(next)))))
        .renderWith(layout)
        .logToConsole
    }

    val (router, routerLogic) = Router.componentAndLogic(BaseUrl.fromWindowOrigin, routerConfig)

    def navigated(page: ModelRO[SeqexecPages]): Unit = {
      scalajs.js.timers.setTimeout(0)(routerLogic.ctl.set(page.value).runNow())
    }

    // subscribe to navigation changes
    SeqexecCircuit.subscribe(SeqexecCircuit.zoom(_.uiModel.navLocation))(navigated _)

    // Initiate the WebSocket connection
    SeqexecCircuit.dispatch(WSConnect(0))

    router
  }

}
