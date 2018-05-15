// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.web.client.components.sequence.steps

import edu.gemini.seqexec.model.Model.{FPUMode, Instrument, SequenceId, Step, StepType, StepState}
import edu.gemini.seqexec.web.client.actions.{NavigateSilentTo, FlipSkipStep, FlipBreakpointStep}
import edu.gemini.seqexec.model.enumerations
import edu.gemini.seqexec.web.client.circuit.{ClientStatus, SeqexecCircuit, StepsTableFocus}
import edu.gemini.seqexec.web.client.components.SeqexecStyles
import edu.gemini.seqexec.web.client.model.Pages
import edu.gemini.seqexec.web.client.lenses._
import edu.gemini.seqexec.web.client.semanticui.elements.label.Label
import edu.gemini.seqexec.web.client.semanticui.elements.icon.Icon
import edu.gemini.seqexec.web.client.semanticui.elements.icon.Icon._
import edu.gemini.seqexec.web.client.semanticui.Size
import edu.gemini.seqexec.web.client.services.HtmlConstants.iconEmpty
import japgolly.scalajs.react._
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.extra.router.RouterCtl
import cats.implicits._
import edu.gemini.web.client.style._

/**
 * Component to display an icon for the state
 */
object StepToolsCell {
  final case class Props(clientStatus: ClientStatus, focus: StepsTableFocus, step: Step, rowHeight: Int, heightChangeCB: Int => Callback)

  private val component = ScalaComponent.builder[Props]("StepIconCell")
    .stateless
    .render_P { p =>
      <.div(
        SeqexecStyles.controlCell,
        StepBreakStopCell(StepBreakStopCell.Props(p.clientStatus, p.focus, p.step, p.rowHeight, p.heightChangeCB)).when(p.clientStatus.isLogged),
        StepIconCell(p)
      )
    }
    .build

  def apply(p: Props): Unmounted[Props, Unit, Unit] = component(p)
}

/**
 * Component to display an icon for the state
 */
object StepBreakStopCell {
  final case class Props(clientStatus: ClientStatus, focus: StepsTableFocus, step: Step, rowHeight: Int, heightChangeCB: Int => Callback) {
    val steps: List[Step] = focus.steps
  }

  // Request a to flip the breakpoint
  def flipBreakpoint(p: Props): Callback =
    Callback.when(p.clientStatus.isLogged)(SeqexecCircuit.dispatchCB(FlipBreakpointStep(p.focus.id, p.step)) >> p.heightChangeCB(p.step.id))

  // Request a to flip the skip
  def flipSkipped(p: Props): Callback =
    Callback.when(p.clientStatus.isLogged)(SeqexecCircuit.dispatchCB(FlipSkipStep(p.focus.id, p.step)))

  private def firstRunnableIndex(l: List[Step]): Int = l.zipWithIndex.find(!_._1.isFinished).map(_._2).getOrElse(l.length)

  private val component = ScalaComponent.builder[Props]("StepIconCell")
    .stateless
    .render_P { p =>
      val canSetBreakpoint = p.clientStatus.isLogged && p.step.canSetBreakpoint(p.step.id, firstRunnableIndex(p.steps))
      val canSetSkipMark = p.clientStatus.isLogged && p.step.canSetSkipmark
      <.div(
        SeqexecStyles.gutterCell,
        ^.height := p.rowHeight.px,
        <.div(
          SeqexecStyles.breakPointHandle,
          ^.onClick --> flipBreakpoint(p),
          Icon.IconRemove.copyIcon(color = Some("grey"), fitted = true, extraStyles = List(SeqexecStyles.breakPointOffIcon)).when(p.step.breakpoint),
          Icon.IconCaretDown.copyIcon(color = Some("grey"), fitted = true, extraStyles = List(SeqexecStyles.breakPointOnIcon)).unless(p.step.breakpoint)
        ).when(canSetBreakpoint),
        <.div(
          SeqexecStyles.skipHandle,
          ^.top := (p.rowHeight / 2 - SeqexecStyles.skipHandleHeight + 2).px,
          IconPlusSquareOutline.copyIcon(link = true, onClick = flipSkipped(p)).when(p.step.skip),
          IconMinusCircle.copyIcon(link = true, color = Some("orange"), onClick = flipSkipped(p)).unless(p.step.skip)
        ).when(canSetSkipMark)
      )
    }
    .build

  def apply(p: Props): Unmounted[Props, Unit, Unit] = component(p)
}

/**
 * Component to display an icon for the state
 */
object StepIconCell {
  private def stepIcon(p: StepToolsCell.Props): VdomNode =
    p.step.status match {
      case StepState.Completed                                => IconCheckmark
      case StepState.Running                                  => IconCircleNotched.copyIcon(loading = true)
      case StepState.Failed(_)                                => IconAttention
      case StepState.Skipped                                  => IconReply.copyIcon(fitted = true, rotated = Icon.Rotated.CounterClockwise)
      case _ if p.step.skip                                   => IconReply.copyIcon(fitted = true, rotated = Icon.Rotated.CounterClockwise)
      case _ if p.focus.nextStepToRun.forall(_ === p.step.id) => IconChevronRight
      case _                                                  => iconEmpty
    }

  private def stepStyle(p: StepToolsCell.Props): GStyle =
    p.step.status match {
      case StepState.Running   => SeqexecStyles.runningIconCell
      case StepState.Skipped   => SeqexecStyles.skippedIconCell
      case StepState.Failed(_) => SeqexecStyles.errorCell
      case _ if p.step.skip    => SeqexecStyles.skippedIconCell
      case _                   => SeqexecStyles.iconCell
    }

  private val component = ScalaComponent.builder[StepToolsCell.Props]("StepIconCell")
    .stateless
    .render_P { p =>
      <.div(
        stepStyle(p),
        stepIcon(p)
      )
    }
    .build

  def apply(p: StepToolsCell.Props): Unmounted[StepToolsCell.Props, Unit, Unit] = component(p)
}

/**
 * Component to display the FPU
 */
object FPUCell {
  final case class Props(s: Step, i: Instrument)

  private val component = ScalaComponent.builder[Props]("FPUCell")
    .stateless
    .render_P { p =>

      val nameMapper: Map[String, String] = p.i match {
        case Instrument.GmosS => enumerations.fpu.GmosSFPU
        case Instrument.GmosN => enumerations.fpu.GmosNFPU
        case Instrument.F2    => enumerations.fpu.Flamingos2
        case _                => Map.empty
      }

      val fpuValue = for {
        mode <- instrumentFPUModeO.getOption(p.s).orElse(FPUMode.BuiltIn.some) // If the instrument has no fpu mode default to built in
        fpuL = if (mode === FPUMode.BuiltIn) instrumentFPUO else instrumentFPUCustomMaskO
        fpu  <- fpuL.getOption(p.s)
      } yield nameMapper.getOrElse(fpu, fpu)

      <.div(
        SeqexecStyles.componentLabel,
        fpuValue.getOrElse("Unknown"): String
      )
    }
    .build

  def apply(p: Props): Unmounted[Props, Unit, Unit] = component(p)
}

/**
 * Component to display the Filter
 */
object FilterCell {
  final case class Props(s: Step, i: Instrument)

  private val component = ScalaComponent.builder[Props]("FilterCell")
    .stateless
    .render_P { p =>

      val nameMapper: Map[String, String] = p.i match {
        case Instrument.GmosS => enumerations.filter.GmosSFilter
        case Instrument.GmosN => enumerations.filter.GmosNFilter
        case Instrument.F2    => enumerations.filter.F2Filter
        case _                => Map.empty
      }

      val filter = for {
        filter  <- instrumentFilterO.getOption(p.s)
      } yield nameMapper.getOrElse(filter, filter)

      <.div(
        SeqexecStyles.componentLabel,
        filter.getOrElse("Unknown"): String
      )
    }
    .build

  def apply(p: Props): Unmounted[Props, Unit, Unit] = component(p)
}

/**
 * Component to display the disperser and wavelength
 */
object DisperserCell {
  final case class Props(s: Step, i: Instrument)

  private val component = ScalaComponent.builder[Props]("DisperserCell")
    .stateless
    .render_P { p =>

      val nameMapper: Map[String, String] = p.i match {
        case Instrument.GmosS => enumerations.disperser.GmosSDisperser
        case Instrument.GmosN => enumerations.disperser.GmosNDisperser
        case _                => Map.empty
      }

      val disperser = for {
        disperser <- instrumentDisperserO.getOption(p.s)
      } yield nameMapper.getOrElse(disperser, disperser)
      val centralWavelength = instrumentDisperserLambdaO.getOption(p.s)

      // Formatter
      val displayedText = (disperser, centralWavelength) match {
        case (Some(d), Some(w)) => f"$d @ $w%.0f nm"
        case (Some(d), None)    => d
        case _                  => "Unknown"
      }
      <.div(
        SeqexecStyles.componentLabel,
        displayedText
      )
    }
    .build

  def apply(p: Props): Unmounted[Props, Unit, Unit] = component(p)
}

/**
 * Component to display the exposure time and coadds
 */
object ExposureTimeCell {
  final case class Props(s: Step, i: Instrument)

  private val component = ScalaComponent.builder[Props]("ExposureTimeCell")
    .stateless
    .render_P { p =>
      def formatExposureTime(e: Double): String = p.i match {
        case Instrument.GmosN | Instrument.GmosS                => f"$e%.0f"
        case _                                                  => f"$e%.2f"
      }

      val exposureTime = observeExposureTimeO.getOption(p.s)
      val coadds = observeCoaddsO.getOption(p.s)

      // TODO Find a better way to output math-style text
      val seconds = List(<.span(^.display := "inline-block", ^.marginLeft := 5.px, "["), <.span(^.display := "inline-block", ^.verticalAlign := "none", ^.fontStyle := "italic", "s"), <.span(^.display := "inline-block", "]"))

      val displayedText: TagMod = (coadds, exposureTime) match {
        case (c, Some(e)) if c.exists(_ > 1) => (List(<.span(^.display := "inline-block", s"${c.foldMap(_.show)} "), <.span(^.display := "inline-block", ^.verticalAlign := "none", "\u2A2F"), <.span(^.display := "inline-block", s"${formatExposureTime(e)}")) ::: seconds).toTagMod
        case (_, Some(e))                    => ((s"${formatExposureTime(e)}": VdomNode) :: seconds).toTagMod
        case _                               => EmptyVdom
      }

      <.div(
        displayedText
      )
    }
    .build

  def apply(p: Props): Unmounted[Props, Unit, Unit] = component(p)
}

/**
  * Component to display the step id
  */
object StepIdCell {
  private val component = ScalaComponent.builder[Int]("StepIdCell")
    .stateless
    .render_P { p =>
      <.div(
        s"${p + 1}")
    }.build

  def apply(i: Int): Unmounted[Int, Unit, Unit] = component(i)
}

/**
  * Component to link to the settings
  */
object SettingsCell {
  final case class Props(ctl: RouterCtl[Pages.SeqexecPages], instrument: Instrument, obsId: SequenceId, index: Int)
  private val component = ScalaComponent.builder[Props]("SettingsCell")
    .stateless
    .render_P { p =>
      val page = Pages.SequenceConfigPage(p.instrument, p.obsId, p.index + 1)
      <.div(
        SeqexecStyles.settingsCell,
        p.ctl.link(page)(
          IconCaretRight.copyIcon(color = "black".some, onClick = SeqexecCircuit.dispatchCB(NavigateSilentTo(page)))
        )
      )
    }.build

  def apply(i: Props): Unmounted[Props, Unit, Unit] = component(i)
}

/**
  * Component to display the object type
  */
object ObjectTypeCell {
  final case class Props(step: Step, size: Size)
  private val component = ScalaComponent.builder[Props]("ObjectTypeCell")
    .stateless
    .render_P { p =>
      <.div( // Column object type
        stepTypeO.getOption(p.step).map { st =>
          val stepTypeColor = st match {
            case _ if p.step.status === StepState.Completed => "light gray"
            case StepType.Object                            => "green"
            case StepType.Arc                               => "violet"
            case StepType.Flat                              => "grey"
            case StepType.Bias                              => "teal"
            case StepType.Dark                              => "black"
            case StepType.Calibration                       => "blue"
          }
          Label(Label.Props(st.show, color = stepTypeColor.some, size = p.size))
        }.whenDefined
      )
    }.build

  def apply(i: Props): Unmounted[Props, Unit, Unit] = component(i)
}
