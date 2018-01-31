// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.web.client.components.sequence.steps

import edu.gemini.seqexec.model.dhs.ImageFileId
import edu.gemini.seqexec.web.client.components.SeqexecStyles
import japgolly.scalajs.react._
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.vdom.html_<^._
import scalacss.ScalaCssReact._

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
