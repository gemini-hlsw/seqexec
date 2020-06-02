// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components.sequence.toolbars

import diode.react.ReactConnectProxy
import gem.Observation
import japgolly.scalajs.react.ScalaComponent
import japgolly.scalajs.react.vdom.html_<^._
import react.common._
import react.semanticui.collections.grid._
import react.semanticui.floats._
import react.semanticui.widths._
import seqexec.web.client.circuit._
import seqexec.web.client.components.SeqexecStyles

final case class SequenceDefaultToolbar(id: Observation.Id) extends ReactProps[SequenceDefaultToolbar](SequenceDefaultToolbar.component) {

  val observerReader: ReactConnectProxy[Option[SequenceInfoFocus]] =
    SeqexecCircuit.connect(SeqexecCircuit.sequenceObserverReader(id))
  val controlReader: ReactConnectProxy[Option[SequenceControlFocus]] =
    SeqexecCircuit.connect(SeqexecCircuit.sequenceControlReader(id))
}

/**
  * Toolbar for logged in users
  */
object SequenceDefaultToolbar {

  type Props = SequenceDefaultToolbar

  private val component = ScalaComponent
    .builder[Props]("SequenceDefaultToolbar")
    .stateless
    .render_P(p =>
      Grid(
        GridRow(columns = Two, clazz = SeqexecStyles.shorterRow)(
          GridColumn(floated  = Left,
                     computer = Eight,
                     tablet   = Eight,
                     only     = GridOnly.Computer,
                     clazz    = SeqexecStyles.infoOnControl)(
            p.controlReader(_() match {
              case Some(c) => SequenceControl(c)
              case _       => <.div()
            })
          ),
          GridColumn(floated = Right, clazz = SeqexecStyles.infoOnControl)(
            p.observerReader(_() match {
              case Some(p) => SequenceInfo(p)
              case _       => <.div()
            })
          )
        )
      )
    )
    .build

}
