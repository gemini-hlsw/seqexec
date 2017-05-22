package edu.gemini.seqexec.web.client.components

import edu.gemini.seqexec.web.client.components.sequence.SequenceArea
import edu.gemini.seqexec.web.client.model.SeqexecCircuit
import japgolly.scalajs.react.ScalaComponent
import japgolly.scalajs.react.vdom.html_<^._

/**
  * Top level UI component
  */
object SeqexecUI {
  private val lbConnect = SeqexecCircuit.connect(_.loginBox)
  private val wsConsoleConnect = SeqexecCircuit.connect(m => (m.devConsoleState, m.webSocketLog))

  val component = ScalaComponent.builder[Unit]("Seqexec")
    .stateless
    .render(_ =>
      <.div(
        NavBar(),
        wsConsoleConnect(u => WebSocketsConsole(u()._1, u()._2)),
        QueueArea(),
        SequenceArea(),
        lbConnect(LoginBox.apply)
      )
    )
    .build

  def apply() = component()
}

