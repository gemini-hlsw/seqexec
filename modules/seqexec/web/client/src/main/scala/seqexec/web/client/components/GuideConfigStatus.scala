// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components

import cats.Show
import cats.implicits._
import gem.syntax.all._
import diode.react.ModelProxy
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.Reusability
import react.common.implicits._
import seqexec.model.TelescopeGuideConfig
import seqexec.model.enum.ComaOption
import seqexec.model.enum.MountGuideOption
import seqexec.model.M1GuideConfig
import seqexec.model.M2GuideConfig
import seqexec.web.client.reusability._

/**
  * Alert message when the connection disappears
  */
object GuideConfigStatus {
  final case class Props(config: TelescopeGuideConfig)

  implicit val mountGuideShow = Show.show[MountGuideOption] {
    case MountGuideOption.MountGuideOn  => "On"
    case MountGuideOption.MountGuideOff => "Off"
  }

  implicit val comaOptionShow = Show.show[ComaOption] {
    case ComaOption.ComaOn  => "On"
    case ComaOption.ComaOff => "Off"
  }

  implicit val m1GuideShow = Show.show[M1GuideConfig] {
    case M1GuideConfig.M1GuideOn(s) => s.tag
    case M1GuideConfig.M1GuideOff   => "Off"
  }

  implicit val propsReuse: Reusability[Props] = Reusability.derive[Props]

  private val component = ScalaComponent
    .builder[Props]("GuideConfigStatus")
    .stateless
    .render_P { p =>
      React.Fragment(
        <.span(
          ^.cls := "header item",
          SeqexecStyles.activeGuide
            .when(p.config.mountGuide === MountGuideOption.MountGuideOn),
          s"Mount: ${p.config.mountGuide.show}"
        ),
        <.span(
          ^.cls := "header item",
          SeqexecStyles.activeGuide
            .when(p.config.m1Guide =!= M1GuideConfig.M1GuideOff),
          s"M1: ${p.config.m1Guide.show}"
        ),
        p.config.m2Guide match {
          case M2GuideConfig.M2GuideOn(c, s) =>
            React.Fragment(
              <.span(
                ^.cls := "header item",
                SeqexecStyles.activeGuide.when(s.nonEmpty),
                s"Tip/Tilt: ${s.map(_.tag).mkString("+")}".when(s.nonEmpty),
                s"Tip/Tilt: Off".when(s.isEmpty)
              ),
              <.span(
                ^.cls := "header item",
                SeqexecStyles.activeGuide.when(c === ComaOption.ComaOn),
                s"Coma: ${c.show}"
              )
            )
          case M2GuideConfig.M2GuideOff =>
            React.Fragment(
              <.span(
                ^.cls := "header item",
                "Tip/Tilt: Off"
              ),
              <.span(
                ^.cls := "header item",
                "Coma: Off"
              )
            )
        }
      )
    }
    .configure(Reusability.shouldComponentUpdate)
    .build

  def apply(u: ModelProxy[TelescopeGuideConfig]): Unmounted[Props, Unit, Unit] =
    component(Props(u()))
}
