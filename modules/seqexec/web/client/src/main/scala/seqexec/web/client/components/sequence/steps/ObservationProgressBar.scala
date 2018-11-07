// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components.sequence.steps

import japgolly.scalajs.react._
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.vdom.html_<^._
import gem.Observation
import seqexec.model.dhs.ImageFileId
import seqexec.web.client.components.SeqexecStyles
import seqexec.web.client.circuit.SeqexecCircuit
import web.client.style._

/**
  * Component to wrap the progress bar
  */
object ObservationProgressBar {
  final case class Props(obsId: Observation.Id, fileId: ImageFileId) {
    protected[steps] val connect =
      SeqexecCircuit.connect(SeqexecCircuit.obsProgressReader(obsId))
  }

  private val component = ScalaComponent
    .builder[Props]("ObservationProgressBar")
    .stateless
    .render_P(p =>
      <.div(
        SeqexecStyles.observationProgressRow,
        <.div(
          ^.cls := "ui progress",
          SeqexecStyles.observationProgressBar,
          <.div(
            ^.cls := "bar",
            SeqexecStyles.observationBar,
            <.div(^.cls := "progress")
          )
        ),
        <.div(
          ^.cls := "label",
          SeqexecStyles.observationLabel,
          p.connect(x => <.div(s"${x()}")),
          p.fileId
        )
    ))
    .build

  def apply(p: Props): Unmounted[Props, Unit, Unit] = component(p)
}
