// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components

import gem.enum.Site
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.router.RouterCtl
import japgolly.scalajs.react.Reusability
import japgolly.scalajs.react.vdom.html_<^._
import react.common._
import react.semanticui.collections.menu._
import seqexec.web.client.actions.SelectCalibrationQueue
import seqexec.web.client.circuit.SeqexecCircuit
import seqexec.web.client.model.Pages._
import seqexec.web.client.OcsBuildInfo
import seqexec.web.client.reusability._

final case class Footer(router: RouterCtl[SeqexecPages], site: Site) extends ReactProps[Footer](Footer.component)

/**
  * Component for the bar at the top of the page
  */
object Footer {
  type Props = Footer

  implicit val propsReuse: Reusability[Props] = Reusability.by(_.site)

  private val userConnect = SeqexecCircuit.connect(SeqexecCircuit.statusReader)

  private def goHome(p: Props)(e: ReactEvent): Callback =
    e.preventDefaultCB *>
      p.router.dispatchAndSetUrlCB(SelectCalibrationQueue)

  private val component = ScalaComponent
    .builder[Props]("SeqexecAppBar")
    .stateless
    .render_P(p =>
      Menu(
        clazz    = Css("footer"),
        inverted = true
      )(
        MenuItem(
          as       = "a",
          header   = true,
          clazz    = SeqexecStyles.notInMobile,
          onClickE = goHome(p) _
        )(s"Seqexec - ${p.site.shortName}"),
        MenuItem(
          as       = "a",
          header   = true,
          clazz    = SeqexecStyles.onlyMobile,
          onClickE = goHome(p) _
        )(p.site.shortName),
        MenuMenu(
          MenuItem(
            header = true,
            clazz  = SeqexecStyles.notInMobile
          )(OcsBuildInfo.version)
        ),
        userConnect(x => FooterStatus(x()))
      )
    )
    .configure(Reusability.shouldComponentUpdate)
    .build

}
