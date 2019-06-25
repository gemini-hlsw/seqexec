// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components

import gem.enum.Site
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.router.RouterCtl
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.Reusability
import react.common.implicits._
import seqexec.web.client.actions.SelectCalibrationQueue
import seqexec.web.client.circuit.SeqexecCircuit
import seqexec.web.client.model.Pages._
import seqexec.web.client.OcsBuildInfo
import seqexec.web.client.semanticui.elements.menu.HeaderItem
import seqexec.web.client.reusability._

/**
  * Component for the bar at the top of the page
  */
object Footer {
  final case class Props(router: RouterCtl[SeqexecPages], site: Site)

  implicit val propsReuse: Reusability[Props] = Reusability.by(_.site)

  private val userConnect = SeqexecCircuit.connect(SeqexecCircuit.statusReader)

  private def goHome(p: Props)(e: ReactEvent): Callback =
    e.preventDefaultCB *>
      p.router.dispatchAndSetUrlCB(SelectCalibrationQueue)

  private val component = ScalaComponent
    .builder[Props]("SeqexecAppBar")
    .stateless
    .render_P(
      p =>
        <.div(
          ^.cls := "ui footer inverted menu",
          <.a(
            ^.cls := "header item",
            ^.onClick ==> goHome(p),
            SeqexecStyles.notInMobile,
            s"Seqexec - ${p.site.shortName}"
          ),
          <.a(
            ^.cls := "header item",
            ^.onClick ==> goHome(p),
            SeqexecStyles.onlyMobile,
            p.site.shortName
          ),
          HeaderItem(
            HeaderItem.Props(OcsBuildInfo.version,
                             sub         = true,
                             extraStyles = List(SeqexecStyles.notInMobile))
          ),
          userConnect(FooterStatus.apply)
        )
    )
    .componentDidMount(
      ctx =>
        Callback {
          // Mount the Semantic component using jQuery
          import org.querki.jquery.$
          import web.client.facades.semanticui.SemanticUIVisibility._

          // Pick the top bar and make it stay visible regardless of scrolling
          val dom = ctx.getDOMNode.asMounted().asElement()
          $(dom)
            .visibility(JsVisiblityOptions.visibilityType("fixed").offset(0))
        }
    )
    .configure(Reusability.shouldComponentUpdate)
    .build

  def apply(p: Props): Unmounted[Props, Unit, Unit] = component(p)
}
