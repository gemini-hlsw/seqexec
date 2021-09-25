// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components

import scala.concurrent.duration._

import japgolly.scalajs.react.Reusability
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.router.RouterCtl
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.enum.Site
import react.clipboard.CopyToClipboard
import react.common._
import react.semanticui.collections.menu._
import react.semanticui.elements.icon.Icon
import react.semanticui.modules.popup.Popup
import react.semanticui.modules.popup.PopupPosition
import react.semanticui.sizes._
import react.semanticui.toasts._
import seqexec.web.client.OcsBuildInfo
import seqexec.web.client.actions.SelectCalibrationQueue
import seqexec.web.client.circuit.SeqexecCircuit
import seqexec.web.client.model.Pages._
import seqexec.web.client.reusability._

final case class Footer(router: RouterCtl[SeqexecPages], site: Site)
    extends ReactProps[Footer](Footer.component)

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

  val onVersionCopy     = (_: String, _: Boolean) =>
    toastCB(
      ToastOptions(title = "Copied...",
                   icon = Icon("clipboard"),
                   size = Small,
                   tpe = ToastType.Success,
                   time = Dismissal.On(500.millisecond)
      )
    )

  private val component = ScalaComponent
    .builder[Props]
    .stateless
    .render_P(p =>
      Menu(
        clazz = SeqexecStyles.Footer,
        inverted = true
      )(
        MenuItem(
          as = "a",
          header = true,
          clazz = SeqexecStyles.notInMobile,
          onClickE = goHome(p) _
        )(s"Seqexec - ${p.site.shortName}"),
        Popup(
          position = PopupPosition.TopCenter,
          size = Tiny,
          trigger = MenuItem(as = <.a, header = true, clazz = SeqexecStyles.notInMobile)(
            CopyToClipboard(text = OcsBuildInfo.version, onCopy = onVersionCopy)(
              OcsBuildInfo.version
            )
          )
        )(
          "Copy to clipboard"
        ),
        userConnect(x => FooterStatus(x()))
      )
    )
    .configure(Reusability.shouldComponentUpdate)
    .build

}
