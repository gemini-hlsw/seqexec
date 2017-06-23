package edu.gemini.seqexec.web.client.components

import edu.gemini.seqexec.web.client.components.sequence.SequenceArea
import edu.gemini.seqexec.web.client.model.{SeqexecCircuit, WSConnect}
import edu.gemini.seqexec.web.client.model.InstrumentNames
import edu.gemini.seqexec.web.client.model.Pages._
import edu.gemini.seqexec.web.client.model.NavigateSilentTo
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.extra.router._
import japgolly.scalajs.react.Callback
import diode.ModelRO

/**
  * Top level UI component
  */
object SeqexecUI {
  private val lbConnect = SeqexecCircuit.connect(_.loginBox)
  private val wsConsoleConnect = SeqexecCircuit.connect(m => (m.devConsoleState, m.webSocketLog))
  private val navLocationConnect = SeqexecCircuit.connect(_.navLocation)

  case class RouterProps(page: InstrumentPage, router: RouterCtl[InstrumentPage])

  def router: Router[SeqexecPages] = {
    val routerConfig = RouterConfigDsl[SeqexecPages].buildConfig { dsl =>
      import dsl._

      def layout(c: RouterCtl[SeqexecPages], r: Resolution[SeqexecPages]) =
        <.div(
          NavBar(),
          wsConsoleConnect(u => WebSocketsConsole(u()._1, u()._2)),
          navLocationConnect(_ => QueueArea()),
          r.render(),
          lbConnect(LoginBox.apply)
        )

      (emptyRule
      | staticRoute(root, Root) ~> renderR(r => SequenceArea())
      | dynamicRoute(("/" ~ string("[a-zA-Z0-9-]+") ~ "/" ~ string("[a-zA-Z0-9-]+").option).caseClass[InstrumentPage]) {
          case x @ InstrumentPage(i, _) if InstrumentNames.instruments.list.toList.contains(i) => x
        } ~> dynRenderR((p, r) => SequenceArea())
      )
        .notFound(redirectToPage(Root)(Redirect.Push))
        // Runtime verification that all pages are routed
        .verify(Root, InstrumentNames.instruments.list.toList.map(i => InstrumentPage(i, None)): _*)
        .onPostRender((_, next) =>
          Callback.when(next != SeqexecCircuit.zoom(_.navLocation).value)(Callback.log("post render" + next.toString) >> Callback(SeqexecCircuit.dispatch(NavigateSilentTo(next)))))
        .renderWith(layout)
        .logToConsole
    }

    val (router, routerLogic) = Router.componentAndLogic(BaseUrl.fromWindowOrigin, routerConfig)

    def navigated(page: ModelRO[SeqexecPages]): Unit = {
      println("navigated " + page)
      scalajs.js.timers.setTimeout(0)(routerLogic.ctl.set(page.value).runNow())
    }

    // subscribe to navigation changes
    SeqexecCircuit.subscribe(SeqexecCircuit.zoom(_.navLocation))(navigated _)

    // Initiate the WebSocket connection
    SeqexecCircuit.dispatch(WSConnect(0))

    router
  }

}
