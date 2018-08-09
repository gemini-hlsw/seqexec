// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components.sequence.toolbars

import gem.enum.Site
import seqexec.model.enum.Instrument
import seqexec.web.client.circuit.SeqexecCircuit
import seqexec.web.client.components.SeqexecStyles
import seqexec.web.client.ModelOps._
import web.client.style._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.ScalaComponent
import japgolly.scalajs.react.component.Scala.Unmounted

/**
  * Toolbar for anonymous users
  */
object SequenceAnonymousToolbar {
  final case class Props(site: Site, instrument: Instrument) {
    protected[sequence] val instrumentConnects =
     site.instruments.toList.map(i => (i, SeqexecCircuit.connect(SeqexecCircuit.sequenceObserverReader(i)))).toMap
  }

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
            p.instrumentConnects.get(p.instrument).whenDefined(_(SequenceInfo.apply))
          )
        )
      )
    ).build

  def apply(site: Site, i: Instrument): Unmounted[Props, Unit, Unit] = component(Props(site, i))
}
