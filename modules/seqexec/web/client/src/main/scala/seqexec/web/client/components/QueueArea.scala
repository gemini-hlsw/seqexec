// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components

import seqexec.web.client.circuit._
import seqexec.web.client.model.Pages._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react._
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.extra.router.RouterCtl
import japgolly.scalajs.react.extra.Reusability
import web.client.style._

/**
  * Container for the queue table
  */
object QueueTableSection {
  private val sequencesConnect = SeqexecCircuit.connect(SeqexecCircuit.statusAndLoadedSequencesReader)

  private val component = ScalaComponent.builder[RouterCtl[SeqexecPages]]("QueueTableSection")
    .stateless
    .render_P(p =>
      <.div(
        SeqexecStyles.queueListPane,
        sequencesConnect(c => QueueTableBody(p, c))
      )
    )
    .configure(Reusability.shouldComponentUpdate)
    .build

  def apply(ctl: RouterCtl[SeqexecPages]): Unmounted[RouterCtl[SeqexecPages], Unit, Unit] = component(ctl)

}

/**
  * Displays the elements on the queue
  */
object QueueArea {

  private val component = ScalaComponent.builder[RouterCtl[SeqexecPages]]("QueueArea")
    .stateless
    .render_P(p =>
      <.div(
        ^.cls := "twelve wide column",
        QueueTableSection(p)
      )
    )
    .configure(Reusability.shouldComponentUpdate)
    .build

  def apply(ctl: RouterCtl[SeqexecPages]): Unmounted[RouterCtl[SeqexecPages], Unit, Unit] = component(ctl)

}
