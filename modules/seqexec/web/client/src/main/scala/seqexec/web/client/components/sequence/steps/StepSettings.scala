// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components.sequence.steps

import cats.implicits._
import japgolly.scalajs.react._
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.extra.Reusability
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.extra.router.RouterCtl
import gem.Observation
import seqexec.model.enum.Instrument
import seqexec.model.enum.StepType
import seqexec.model.Step
import seqexec.model.StepState
import seqexec.web.client.components.SeqexecStyles
import seqexec.web.client.model.Pages
import seqexec.web.client.model.lenses._
import seqexec.web.client.model.StepItems._
import seqexec.web.client.semanticui.elements.label.Label
import seqexec.web.client.semanticui.elements.icon.Icon._
import seqexec.web.client.semanticui.Size
import seqexec.web.client.reusability._
import web.client.style._

/**
  * Component to display an item of a sequence
  */
object StepItemCell {
  final case class Props(value: Option[String])

  implicit val propsReuse: Reusability[Props] = Reusability.derive[Props]

  private val component = ScalaComponent
    .builder[Props]("StepItemCell")
    .stateless
    .render_P { p =>
      <.div(
        SeqexecStyles.componentLabel,
        p.value.getOrElse("Unknown"): String
      )
    }
    .configure(Reusability.shouldComponentUpdate)
    .build

  def apply(p: Props): Unmounted[Props, Unit, Unit] = component(p)
}

/**
  * Component to display the exposure time and coadds
  */
object ExposureTimeCell {
  final case class Props(s: Step, i: Instrument)

  implicit val propsReuse: Reusability[Props] = Reusability.derive[Props]

  private val component = ScalaComponent
    .builder[Props]("ExposureTimeCell")
    .stateless
    .render_P { p =>
      val exposureTime = p.s.exposureTimeS(p.i)
      val coadds       = p.s.coAdds

      // TODO Find a better way to output math-style text
      val seconds = List(
        <.span(^.display := "inline-block", ^.marginLeft := 5.px, "["),
        <.span(^.display := "inline-block",
               ^.verticalAlign := "none",
               ^.fontStyle := "italic",
               "s"),
        <.span(^.display := "inline-block", "]")
      )

      val displayedText: TagMod = (coadds, exposureTime) match {
        case (c, Some(e)) if c.exists(_ > 1) =>
          (List(
            <.span(^.display := "inline-block", s"${c.foldMap(_.show)} "),
            <.span(^.display := "inline-block",
                   ^.verticalAlign := "none",
                   "\u2A2F"),
            <.span(^.display := "inline-block", s"$e")
          ) ::: seconds).toTagMod
        case (_, Some(e)) =>
          ((s"$e": VdomNode) :: seconds).toTagMod
        case _ => EmptyVdom
      }

      <.div(
        displayedText
      )
    }
    .configure(Reusability.shouldComponentUpdate)
    .build

  def apply(p: Props): Unmounted[Props, Unit, Unit] = component(p)
}

/**
  * Component to display the step id
  */
object StepIdCell {
  private val component = ScalaComponent
    .builder[Int]("StepIdCell")
    .stateless
    .render_P(p => <.div(s"${p + 1}"))
    .configure(Reusability.shouldComponentUpdate)
    .build

  def apply(i: Int): Unmounted[Int, Unit, Unit] = component(i)
}

/**
  * Component to link to the settings
  */
object SettingsCell {
  final case class Props(ctl:        RouterCtl[Pages.SeqexecPages],
                         instrument: Instrument,
                         obsId:      Observation.Id,
                         index:      Int,
                         isPreview:  Boolean)

  implicit val propsReuse: Reusability[Props] = Reusability.derive[Props]

  private val component = ScalaComponent
    .builder[Props]("SettingsCell")
    .stateless
    .render_P { p =>
      val page = if (p.isPreview) {
        Pages.PreviewConfigPage(p.instrument, p.obsId, p.index + 1)
      } else {
        Pages.SequenceConfigPage(p.instrument, p.obsId, p.index + 1)
      }
      <.div(
        SeqexecStyles.settingsCell,
        p.ctl.link(page)(
          IconCaretRight.copyIcon(color   = "black".some,
                                  onClick = p.ctl.setUrlAndDispatchCB(page))
        )
      )
    }
    .configure(Reusability.shouldComponentUpdate)
    .build

  def apply(i: Props): Unmounted[Props, Unit, Unit] = component(i)
}

/**
  * Component to display the object type
  */
object ObjectTypeCell {
  final case class Props(step: Step, size: Size)

  implicit val propsReuse: Reusability[Props] = Reusability.derive[Props]

  private val component = ScalaComponent
    .builder[Props]("ObjectTypeCell")
    .stateless
    .render_P { p =>
      <.div( // Column object type
        stepTypeO
          .getOption(p.step)
          .map { st =>
            val stepTypeColor = st match {
              case _ if p.step.status === StepState.Completed => "light gray"
              case StepType.Object                            => "green"
              case StepType.Arc                               => "violet"
              case StepType.Flat                              => "grey"
              case StepType.Bias                              => "teal"
              case StepType.Dark                              => "black"
              case StepType.Calibration                       => "blue"
            }
            Label(
              Label.Props(st.show, color = stepTypeColor.some, size = p.size))
          }
          .whenDefined)
    }
    .configure(Reusability.shouldComponentUpdate)
    .build

  def apply(i: Props): Unmounted[Props, Unit, Unit] = component(i)
}
