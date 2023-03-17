// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components

import japgolly.scalajs.react.Reusability
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.router.RouterCtl
import japgolly.scalajs.react.vdom.html_<^._
import react.common._
import seqexec.web.client.circuit._
import seqexec.web.client.model.Pages._

/**
 * Container for the queue table
 */
final case class SessionQueueTableSection(pages: RouterCtl[SeqexecPages])
    extends ReactProps[SessionQueueTableSection](SessionQueueTableSection.component)

object SessionQueueTableSection {
  type Props = SessionQueueTableSection

  implicit val propsReuse: Reusability[Props] = Reusability.by(_.pages)

  private val sequencesConnect =
    SeqexecCircuit.connect(SeqexecCircuit.statusAndLoadedSequencesReader)

  private val component = ScalaComponent
    .builder[Props]
    .stateless
    .render_P(p =>
      React.Fragment(
        <.div(
          SeqexecStyles.queueListPane,
          sequencesConnect(c => SessionQueueTable(p.pages, c()))
        ),
        SessionQueueTableFilter()
      )
    )
    .configure(Reusability.shouldComponentUpdate)
    .build

}
