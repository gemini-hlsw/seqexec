package edu.gemini.seqexec.web.client.components

import diode.react.ModelProxy
import edu.gemini.seqexec.web.client.components.sequence.SequenceArea
import edu.gemini.seqexec.web.client.model.{SectionVisibilityState, SeqexecCircuit}
import japgolly.scalajs.react.ReactComponentB
import japgolly.scalajs.react.vdom.prefix_<^._

/**
  * Top level UI component
  */
object SeqexecUI {
  val lbConnect = SeqexecCircuit.connect(_.loginBox)
  val qaConnect = SeqexecCircuit.connect(_.searchAreaState)
  val wsConsoleConnect = SeqexecCircuit.connect(m => (m.devConsoleState, m.webSocketLog))

  val component = ReactComponentB[Unit]("Seqexec")
    .stateless
    .render(_ =>
      <.div(
        NavBar(),
        wsConsoleConnect(u => WebSocketsConsole(u()._1, u()._2)),
        qaConnect(QueueArea.apply),
        SequenceArea(),
        lbConnect(LoginBox.apply)
      )
    )
    .build

  def apply() = component()
}

