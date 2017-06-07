package edu.gemini.seqexec.web.client.components

import edu.gemini.seqexec.web.client.components.sequence.SequenceArea
import edu.gemini.seqexec.web.client.model.SeqexecCircuit
import edu.gemini.seqexec.web.client.model.InstrumentNames
import edu.gemini.seqexec.model.Model.Instrument
import japgolly.scalajs.react.ScalaComponent
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.extra.router._

/**
  * Top level UI component
  */
object SeqexecUI {
  private val lbConnect = SeqexecCircuit.connect(_.loginBox)
  private val wsConsoleConnect = SeqexecCircuit.connect(m => (m.devConsoleState, m.webSocketLog))
  sealed trait SeqexecPages

  case object Root extends SeqexecPages
  case class InstrumentPage(i: Instrument) extends SeqexecPages

  val routerConfig = RouterConfigDsl[SeqexecPages].buildConfig { dsl =>
    import dsl._

    (trimSlashes
    | staticRoute(root,     Root)  ~> render(SequenceArea(InstrumentPage("Flamingos2")))
    | dynamicRoute(string("[a-zA-Z0-9]+").caseClass[InstrumentPage]) {
      case x @ InstrumentPage(i) if InstrumentNames.instruments.list.toList.contains(i) => x
    } ~> dynRender(SequenceArea(_))

    )
      .notFound(redirectToPage(Root)(Redirect.Replace)).logToConsole
      .renderWith(layout)
  }

  val router = Router(BaseUrl.fromWindowOrigin, routerConfig)

  def layout(c: RouterCtl[SeqexecPages], r: Resolution[SeqexecPages]) =
    <.div(
      NavBar(),
      wsConsoleConnect(u => WebSocketsConsole(u()._1, u()._2)),
      QueueArea(),
      r.render(),
      lbConnect(LoginBox.apply)
    )

}
