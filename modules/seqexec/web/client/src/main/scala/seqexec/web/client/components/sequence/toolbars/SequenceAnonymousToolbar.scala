// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components.sequence.toolbars

import gem.Observation
import seqexec.web.client.circuit.SeqexecCircuit
import seqexec.web.client.components.SeqexecStyles
import web.client.style._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.ScalaComponent
import japgolly.scalajs.react.component.Scala.Unmounted

/**
  * Toolbar for anonymous users
  */
object SequenceAnonymousToolbar {
  final case class Props(id: Observation.Id)

  private def component = ScalaComponent.builder[Props]("SequencesDefaultToolbar")
    .stateless
    .render_P ( p =>
      <.div(
        ^.cls := "ui grid",
        <.div(
          ^.cls := "ui row",
          <.div(
            ^.cls := "ui left column bottom aligned sixteen wide",
            SeqexecStyles.shorterRow,
            SeqexecCircuit.connect(SeqexecCircuit.sequenceObserverReader(p.id))(SequenceInfo.apply)
          )
        )
      )
    ).build

  def apply(p: Props): Unmounted[Props, Unit, Unit] = component(p)
}
