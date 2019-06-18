// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components

import seqexec.web.client.circuit._
import seqexec.web.client.model.Pages._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react._
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.extra.router.RouterCtl
import japgolly.scalajs.react.Reusability
import react.common.implicits._

/**
  * Container for the queue table
  */
object SessionQueueTableSection {
  private val sequencesConnect =
    SeqexecCircuit.connect(SeqexecCircuit.statusAndLoadedSequencesReader)

  private val component = ScalaComponent
    .builder[RouterCtl[SeqexecPages]]("SessionQueueTableSection")
    .stateless
    .render_P(
      p =>
        React.Fragment(
          <.div(
            SeqexecStyles.queueListPane,
            sequencesConnect(c => SessionQueueTable(p, c()))
          ),
          SessionQueueTableFilter()
      ))
    .configure(Reusability.shouldComponentUpdate)
    .build

  def apply(
    ctl: RouterCtl[SeqexecPages]
  ): Unmounted[RouterCtl[SeqexecPages], Unit, Unit] =
    component(ctl)

}
