// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components.sequence.steps

import cats.syntax.all._
import japgolly.scalajs.react.Reusability
import japgolly.scalajs.react._
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.extra.router.RouterCtl
import japgolly.scalajs.react.vdom.html_<^._
import react.common._
import react.semanticui.SemanticSize
import react.semanticui.colors._
import react.semanticui.elements.label.Label
import seqexec.model.Observation
import seqexec.model.Step
import seqexec.model.StepState
import seqexec.model.enum.Instrument
import seqexec.model.enum.StepType
import seqexec.web.client.components.SeqexecStyles
import seqexec.web.client.icons._
import seqexec.web.client.model.Pages
import seqexec.web.client.model.StepItems._
import seqexec.web.client.reusability._

/**
  * Component to display an item of a sequence
  */
final case class StepItemCell(value: Option[String]) extends ReactProps[StepItemCell](StepItemCell.component)

object StepItemCell {
  type Props = StepItemCell

  implicit val propsReuse: Reusability[Props] = Reusability.derive[Props]

  protected val component = ScalaComponent
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
}

/**
  * Component to display the exposure time and coadds
  */
final case class ExposureTimeCell(s: Step, i: Instrument) extends ReactProps[ExposureTimeCell](ExposureTimeCell.component)

object ExposureTimeCell {
  type Props = ExposureTimeCell

  implicit val propsReuse: Reusability[Props] =
    Reusability.by(p => (p.s.config, p.i))

  protected val component = ScalaComponent
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
            <.span(^.display := "inline-block", ^.verticalAlign := "none", "\u2A2F"),
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
final case class SettingsCell(
  ctl:        RouterCtl[Pages.SeqexecPages],
  instrument: Instrument,
  obsId:      Observation.Id,
  index:      Int,
  isPreview:  Boolean
) extends ReactProps[SettingsCell](SettingsCell.component)

object SettingsCell {
  type Props = SettingsCell

  implicit val propsReuse: Reusability[Props] = Reusability.derive[Props]

  protected val component = ScalaComponent
    .builder[Props]("SettingsCell")
    .stateless
    .render_P { p =>
      val page = if (p.isPreview) {
        Pages.PreviewConfigPage(p.instrument, p.obsId, p.index + 1)
      } else {
        Pages.SequenceConfigPage(p.instrument, p.obsId, p.index + 1)
      }
      <.div(SeqexecStyles.settingsCell,
            p.ctl.link(page)(
              IconCaretRight.color(Black)(^.onClick --> p.ctl.setUrlAndDispatchCB(page))
            ))
    }
    .configure(Reusability.shouldComponentUpdate)
    .build
}

/**
  * Component to display the object type
  */
final case class ObjectTypeCell(
  instrument: Instrument,
  step:       Step,
  size:       SemanticSize
) extends ReactProps[ObjectTypeCell](ObjectTypeCell.component)

object ObjectTypeCell {
  type Props = ObjectTypeCell

  implicit val propsReuse: Reusability[Props] =
    Reusability.by(p => (p.instrument, p.step.config, p.step.status, p.size))

  protected val component = ScalaComponent
    .builder[Props]("ObjectTypeCell")
    .stateless
    .render_P { p =>
      <.div( // Column object type
        p.step
          .stepType(p.instrument)
          .map { st =>
            val stepTypeColor = st match {
              case _ if p.step.status === StepState.Completed => Grey
              case StepType.Object                            => Green
              case StepType.Arc                               => Violet
              case StepType.Flat                              => Grey
              case StepType.Bias                              => Teal
              case StepType.Dark                              => Black
              case StepType.Calibration                       => Blue
              case StepType.AlignAndCalib                     => Brown
              case StepType.NodAndShuffle                     => Olive
              case StepType.NodAndShuffleDark                 => Black
            }
            Label(color = stepTypeColor, size = p.size)(st.show)
          }
          .whenDefined
      )
    }
    .configure(Reusability.shouldComponentUpdate)
    .build
}
