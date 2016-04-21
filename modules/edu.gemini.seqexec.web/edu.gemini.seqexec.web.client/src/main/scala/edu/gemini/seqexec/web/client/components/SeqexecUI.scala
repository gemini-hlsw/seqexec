package edu.gemini.seqexec.web.client.components

import edu.gemini.seqexec.web.client.components.sequence.SequenceArea
import edu.gemini.seqexec.web.client.model.SeqexecCircuit
import japgolly.scalajs.react.ReactComponentB
import japgolly.scalajs.react.vdom.prefix_<^._

/**
  * Top level UI component
  */
object SeqexecUI {

  val component = ReactComponentB[Unit]("Seqexec")
    .stateless
    .render(_ =>
      <.div(
        NavBar(),
        WebSocketsConsole(),
        SeqexecCircuit.connect(_.searchAreaState)(QueueArea(_)),
        SequenceArea()
      )
    )
    .build

  def apply() = component()
}
