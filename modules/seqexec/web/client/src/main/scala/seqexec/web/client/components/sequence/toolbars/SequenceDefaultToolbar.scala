// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components.sequence.toolbars

import diode.react.ReactConnectProxy
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.ScalaComponent
import japgolly.scalajs.react.component.Scala.Unmounted
import gem.Observation
import react.common.implicits._
import seqexec.web.client.circuit._
import seqexec.web.client.components.SeqexecStyles

/**
  * Toolbar for logged in users
  */
object SequenceDefaultToolbar {
  final case class Props(id: Observation.Id) {
    val observerReader: ReactConnectProxy[Option[SequenceInfoFocus]] =
      SeqexecCircuit.connect(SeqexecCircuit.sequenceObserverReader(id))
    val controlReader: ReactConnectProxy[Option[SequenceControlFocus]] =
      SeqexecCircuit.connect(SeqexecCircuit.sequenceControlReader(id))
  }

  private val component = ScalaComponent
    .builder[Props]("SequenceDefaultToolbar")
    .stateless
    .render_P(p =>
      <.div(
        ^.cls := "ui grid",
        <.div(
          ^.cls := "two column row",
          SeqexecStyles.shorterRow,
          <.div(
            ^.cls := "ui left floated column eight wide computer eight wide tablet only",
            p.controlReader(_() match {
              case Some(c) => SequenceControl(SequenceControl.Props(c))
              case _       => <.div()
            })
          ),
          <.div(
            ^.cls := "ui right floated column",
            SeqexecStyles.infoOnControl,
            p.observerReader(_() match {
              case Some(p) => SequenceInfo(SequenceInfo.Props(p))
              case _       => <.div()
            })
          )
        )
    ))
    .build

  def apply(p: Props): Unmounted[Props, Unit, Unit] = component(p)
}
