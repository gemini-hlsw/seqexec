// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components.sequence.steps

import japgolly.scalajs.react._
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.extra.Reusability
import gem.Observation
import seqexec.model.dhs.ImageFileId
import seqexec.model.ObservationProgress
import seqexec.web.client.components.SeqexecStyles
import seqexec.web.client.circuit.SeqexecCircuit
import seqexec.web.client.reusability._
import seqexec.web.client.semanticui.elements.progress.Progress
import web.client.style._

/**
  * Component to wrap the progress bar
  */
object ObservationProgressBar {
  final case class Props(obsId: Observation.Id, fileId: ImageFileId) {
    protected[steps] val connect =
      SeqexecCircuit.connect(SeqexecCircuit.obsProgressReader(obsId))
  }

  implicit val propsReuse: Reusability[Props] = Reusability.derive[Props]

  private val component = ScalaComponent
    .builder[Props]("ObservationProgressBar")
    .stateless
    .render_P(p =>
      <.div(
        SeqexecStyles.observationProgressRow,
        p.connect(x =>
          x() match {
            case Some(ObservationProgress(_, _, r, t)) =>
              val label =
                if (t.millis > 0) p.fileId else s"${p.fileId} - Completing..."
              Progress(Progress.Props(
                label,
                total       = r.millis,
                value       = r.millis - scala.math.max(0, t.millis),
                indicating  = t.millis <= 0,
                progress    = true,
                progressCls = List(SeqexecStyles.observationProgressBar),
                barCls      = List(SeqexecStyles.observationBar),
                labelCls    = List(SeqexecStyles.observationLabel)
              ))
            case _ =>
              <.div(s"Start observing ${p.fileId}")
        })
    ))
    .configure(Reusability.shouldComponentUpdate)
    .build

  def apply(p: Props): Unmounted[Props, Unit, Unit] = component(p)
}
