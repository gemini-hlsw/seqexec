// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components.sequence.steps

import seqexec.model.dhs.ImageFileId
import seqexec.web.client.components.SeqexecStyles
import web.client.style._
import japgolly.scalajs.react._
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.vdom.html_<^._

/**
 * Component to wrap the progress bar
 */
object ObservationProgressBar {
  private val component = ScalaComponent.builder[ImageFileId]("ObservationProgressBar")
    .stateless
    .render_P(fileId =>
      <.div(
        SeqexecStyles.observationProgressRow,
        <.div(
          ^.cls := "ui progress",
          SeqexecStyles.observationProgressBar,
          <.div(
            ^.cls := "bar",
            SeqexecStyles.observationBar,
            <.div(
              ^.cls := "progress")
          )
        ),
        <.div(
          ^.cls := "label",
          SeqexecStyles.observationLabel,
          fileId
        )
      )
    )
    .build

  def apply(p: ImageFileId): Unmounted[ImageFileId, Unit, Unit] = component(p)
}
