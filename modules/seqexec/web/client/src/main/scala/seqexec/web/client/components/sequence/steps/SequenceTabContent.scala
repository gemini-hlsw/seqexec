// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components.sequence.steps

import cats.implicits._
import diode.react.ReactConnectProxy
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.ScalaComponent
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.extra.Reusability
import japgolly.scalajs.react.extra.router.RouterCtl
import seqexec.web.client.circuit._
import seqexec.web.client.model.Pages.SeqexecPages
import seqexec.web.client.model.SectionClosed
import seqexec.web.client.model.SectionOpen
import seqexec.web.client.model.TabSelected
import seqexec.web.client.semanticui._
import seqexec.web.client.semanticui.elements.message.IconMessage
import seqexec.web.client.semanticui.elements.icon.Icon.IconInbox
import seqexec.web.client.components.SeqexecStyles
import seqexec.web.client.reusability._
import web.client.style._

/**
  * Content of a single tab with a sequence
  */
object SequenceTabContent {
  private val defaultContent = IconMessage(
    IconMessage
      .Props(IconInbox, Some("No sequence loaded"), IconMessage.Style.Warning))

  final case class Props(router: RouterCtl[SeqexecPages],
                         p:      SequenceTabContentFocus) {
    val tableTypeConnect: ReactConnectProxy[Option[StepsTableTypeSelection]] =
      SeqexecCircuit.connect(SeqexecCircuit.stepsTableType(p.id))
  }

  implicit val stcfReuse: Reusability[SequenceTabContentFocus] =
    Reusability.derive[SequenceTabContentFocus]
  implicit val propsReuse: Reusability[Props] = Reusability.by(_.p)

  private val component = ScalaComponent
    .builder[Props]("SequenceTabContent")
    .stateless
    .render_P { p =>
      val SequenceTabContentFocus(isLogged,
                                  instrument,
                                  _,
                                  // _,
                                  active,
                                  logDisplayed) = p.p
      val content = p.tableTypeConnect { st =>
        st()
          .map(s =>
            StepsTableContainer(StepsTableContainer.Props(p.router, s)): VdomElement)
          .getOrElse(defaultContent)
      }

      <.div(
        ^.cls := "ui attached secondary segment tab",
        ^.classSet(
          "active" -> (active === TabSelected.Selected)
        ),
        dataTab := instrument.show,
        SeqexecStyles.tabSegment.when(isLogged),
        SeqexecStyles.tabSegmentLogShown
          .when(isLogged && logDisplayed === SectionOpen),
        SeqexecStyles.tabSegmentLogHidden
          .when(isLogged && logDisplayed === SectionClosed),
        SeqexecStyles.tabSegmentUnauth.when(!isLogged),
        SeqexecStyles.tabSegmentLogShownUnauth
          .when(!isLogged && logDisplayed === SectionOpen),
        SeqexecStyles.tabSegmentLogHiddenUnauth
          .when(!isLogged && logDisplayed === SectionClosed),
        content
      )
    }
    .configure(Reusability.shouldComponentUpdate)
    .build

  def apply(p: Props): Unmounted[Props, Unit, Unit] =
    component(p)
}
