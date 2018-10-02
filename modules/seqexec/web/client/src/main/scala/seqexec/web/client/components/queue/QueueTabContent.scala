// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components.queue

import cats.implicits._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.ScalaComponent
import japgolly.scalajs.react.component.Scala.Unmounted
import seqexec.web.client.semanticui._
import seqexec.web.client.semanticui.elements.message.IconMessage
import seqexec.web.client.semanticui.elements.icon.Icon.IconInbox
import seqexec.web.client.model.SectionClosed
import seqexec.web.client.model.SectionOpen
import seqexec.web.client.model.SectionVisibilityState
import seqexec.web.client.model.TabSelected
import seqexec.web.client.components.SeqexecStyles
import web.client.style._

/**
  * Content of the queue tab
  */
object QueueTabContent {
  final case class Props(active:       TabSelected,
                         logDisplayed: SectionVisibilityState)

  private val defaultContent = IconMessage(
    IconMessage
      .Props(IconInbox, Some("Work in progress"), IconMessage.Style.Warning))

  private val component = ScalaComponent
    .builder[Props]("QueueTabContent")
    .stateless
    .render_P { p =>
      ReactFragment(
        // <.div(
        //   ^.height := "100%",
        <.div(
          ^.cls := "ui attached secondary segment tab",
          ^.classSet(
            "active" -> (p.active === TabSelected.Selected)
          ),
          dataTab := "daycal",
          SeqexecStyles.emptyInstrumentTab,
          SeqexecStyles.emptyInstrumentTabLogShown
            .when(p.logDisplayed =!= SectionOpen),
          SeqexecStyles.emptyInstrumentTabLogHidden
            .when(p.logDisplayed =!= SectionClosed),
          <.div(
            QueueToolbar.Props().cmp,
            defaultContent
          )
        )
      )
    }
    .build

  def apply(p: Props): Unmounted[Props, Unit, Unit] =
    component(p)
}
