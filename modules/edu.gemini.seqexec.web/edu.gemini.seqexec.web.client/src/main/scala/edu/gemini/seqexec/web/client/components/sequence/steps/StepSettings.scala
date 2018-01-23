// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.web.client.components.sequence.steps

import edu.gemini.seqexec.model.Model.{FPUMode, Guiding, Instrument, OffsetAxis, Step, StepType, StepState, TelescopeOffset}
import edu.gemini.seqexec.web.client.actions.FlipBreakpointStep
import edu.gemini.seqexec.model.enumerations
import edu.gemini.seqexec.web.client.circuit.{SeqexecCircuit, ClientStatus, StepsTableFocus}
import edu.gemini.seqexec.web.client.components.SeqexecStyles
import edu.gemini.seqexec.web.client.components.sequence.steps.OffsetFns._
import edu.gemini.seqexec.web.client.lenses._
import edu.gemini.seqexec.web.client.semanticui.elements.label.Label
import edu.gemini.seqexec.web.client.semanticui.elements.icon.Icon
import edu.gemini.seqexec.web.client.semanticui.elements.icon.Icon._
import edu.gemini.seqexec.web.client.semanticui.Size
import edu.gemini.seqexec.web.client.services.HtmlConstants.iconEmpty
import edu.gemini.web.client.utils._
import japgolly.scalajs.react.ScalazReact._
import japgolly.scalajs.react._
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.CanvasRenderingContext2D
import org.scalajs.dom.html.Canvas

import scalacss.ScalaCssReact._
import scalaz.syntax.order._
import scalaz.syntax.show._
import scalaz.syntax.std.option._
import scalaz.std.anyVal._
import scalaz.std.string._

/**
  * Component to draw a grid for the offsets using canvas
  */
object OffsetGrid {
  private val Size = 33.0
  final case class Props(p: TelescopeOffset.P, q: TelescopeOffset.Q)
  final case class State(canvas: Option[Canvas])

  private val ST = ReactS.Fix[State]

  @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
  def render(props: Props, state: State): Callback = state.canvas.fold(Callback.empty) { c => Callback {
    // The canvas API is very imperative and stateful but we are inside a Callback!
    val ctx = c.getContext("2d").asInstanceOf[CanvasRenderingContext2D]
    c.width = Size.toInt
    c.height = Size.toInt
    // First quadrant
    ctx.fillStyle = if (props.p > TelescopeOffset.P.Zero && props.q > TelescopeOffset.Q.Zero) "darkgray" else "white"
    ctx.fillRect(0, 0, Size / 2, Size / 2)
    // Second quadrant
    ctx.fillStyle = if (props.p < TelescopeOffset.P.Zero && props.q > TelescopeOffset.Q.Zero) "darkgray" else "white"
    ctx.fillRect(Size / 2, 0, Size / 2, Size / 2)
    // Third quadrant
    ctx.fillStyle = if (props.p < TelescopeOffset.P.Zero && props.q < TelescopeOffset.Q.Zero) "darkgray" else "white"
    ctx.fillRect(Size / 2, Size / 2, Size / 2, Size / 2)
    // Fourth quadrant
    ctx.fillStyle = if (props.p > TelescopeOffset.P.Zero && props.q < TelescopeOffset.Q.Zero) "darkgray" else "white"
    ctx.fillRect(0, Size / 2, Size / 2, Size / 2)
    // Grid
    ctx.fillStyle = "black"
    // Outer border
    ctx.strokeRect(0, 0, Size, Size)
    // Inner borders
    ctx.strokeRect(0, 0, Size / 2, Size / 2)
    ctx.strokeRect(Size / 2, 0, Size / 2, Size / 2)
    ctx.strokeRect(0, Size / 2, Size / 2, Size / 2)
    ctx.strokeRect(Size/2, Size / 2, Size / 2, Size / 2)
  }}

  private val component = ScalaComponent.builder[Props]("OffsetGrid")
    .initialState(State(None))
    .render_P ( p =>
      <.canvas(
        SeqexecStyles.offsetGrid,
        ^.width := Size.toInt.px,
        ^.height := Size.toInt.px
      )
    ).componentWillReceiveProps { ctx =>
    render(ctx.nextProps, ctx.state)
  }.componentDidMount { ctx =>
    // Grab a copy of the canvas
    val state = State(Some(ctx.getDOMNode.domCast[Canvas]))
    ctx.runState(ST.set(state)) >> render(ctx.props, state)
  }.build

  def apply(p: Props): Unmounted[Props, State, Unit] = component(p)

}

/**
  * Component to display the offsets
  */
object OffsetsDisplayCell {
  final case class Props(offsetsDisplay: OffsetsDisplay, step: Step)

  private val component = ScalaComponent.builder[Props]("OffsetsDisplayCell")
    .stateless
    .render_P { p =>
      <.div( // Column step offset
        p.offsetsDisplay match {
          case OffsetsDisplay.DisplayOffsets(offsetWidth) =>
            OffsetBlock(OffsetBlock.Props(p.step, offsetWidth))
          case _ => EmptyVdom
        }
      )
    }.build

  def apply(i: Props): Unmounted[Props, Unit, Unit] = component(i)
}

/**
 * Component to display the offset grid and offset values
 */
object OffsetBlock {
  final case class Props(s: Step, offsetWidth: Int)
  private val component = ScalaComponent.builder[Props]("OffsetValues")
    .stateless
    .render_P { p =>
      val offsetP = telescopeOffsetPO.getOption(p.s).getOrElse(TelescopeOffset.P.Zero)
      val offsetQ = telescopeOffsetQO.getOption(p.s).getOrElse(TelescopeOffset.Q.Zero)

      <.div(
        SeqexecStyles.centeredCell,
        <.div(
          SeqexecStyles.inlineBlock,
          SeqexecStyles.offsetCellWrapper,
          ^.textAlign := "left",
          OffsetGrid(OffsetGrid.Props(offsetP, offsetQ))
        ),
        <.div(
          SeqexecStyles.inlineBlock,
          ^.textAlign := "right",
          <.div(
            <.div(
              ^.width := pLabelWidth.px,
              SeqexecStyles.inlineBlock,
              offsetAxis(OffsetAxis.AxisP)
            ),
            <.div(
              ^.width := p.offsetWidth.px,
              SeqexecStyles.inlineBlock,
              offsetValueFormat(offsetP)
            )
          ),
          <.div(
              SeqexecStyles.inlineBlock,
            <.div(
              ^.width := qLabelWidth.px,
              SeqexecStyles.inlineBlock,
              offsetAxis(OffsetAxis.AxisQ)
            ),
            <.div(
              ^.width := p.offsetWidth.px,
              SeqexecStyles.inlineBlock,
              offsetValueFormat(offsetQ)
            )
          )
        )
      )
    }
    .build

  def apply(p: Props): Unmounted[Props, Unit, Unit] = component(p)
}

/**
 * Component to display an icon for the state
 */
object StepToolsCell {
  final case class Props(clientStatus: ClientStatus, focus: StepsTableFocus, step: Step, rowHeight: Int)

  private val component = ScalaComponent.builder[Props]("StepIconCell")
    .stateless
    .render_P { p =>
      <.div(
        SeqexecStyles.controlCell,
        StepBreakStopCell(StepBreakStopCell.Props(p.clientStatus, p.focus, p.step, p.rowHeight)),
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
  final case class Props(clientStatus: ClientStatus, focus: StepsTableFocus, step: Step, rowHeight: Int) {
    val steps: List[Step] = focus.steps
  }

  def breakpointAt(p: Props, step: Step): Callback =
    Callback.when(p.clientStatus.isLogged)(Callback(SeqexecCircuit.dispatch(FlipBreakpointStep(p.focus.id, step))))

  private def firstRunnableIndex(l: List[Step]): Int = l.zipWithIndex.find(!_._1.isFinished).map(_._2).getOrElse(l.length)

  private val component = ScalaComponent.builder[Props]("StepIconCell")
    .stateless
    .render_P { p =>
      val canSetBreakpoint = p.clientStatus.isLogged && p.step.canSetBreakpoint(p.step.id, firstRunnableIndex(p.steps))
      <.div(
        SeqexecStyles.gutterCell,
        ^.height := p.rowHeight.px,
        <.div(
          SeqexecStyles.breakPointHandle,
          if (p.step.breakpoint) {
            Icon.IconMinus.copyIcon(link = true, color = Some("brown"), onClick = breakpointAt(p, p.step))
          } else {
            Icon.IconCaretDown.copyIcon(link = true, color = Some("grey"), onClick = breakpointAt(p, p.step))
          }
        ).when(canSetBreakpoint)
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
      case _ if p.focus.nextStepToRun.forall(_ === p.step.id) => IconChevronRight
      case _ if p.step.skip                                   => IconReply.copyIcon(rotated = Icon.Rotated.CounterClockwise)
      case _                                                  => iconEmpty
    }

  private val component = ScalaComponent.builder[StepToolsCell.Props]("StepIconCell")
    .stateless
    .render_P { p =>
      <.div(
        SeqexecStyles.iconCell,
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
        SeqexecStyles.centeredCell,
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
        SeqexecStyles.centeredCell,
        filter.getOrElse("Unknown"): String
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
        case (c, Some(e)) if c.exists(_ > 1) => (List(<.span(^.display := "inline-block", s"${~c.map(_.shows)} "), <.span(^.display := "inline-block", ^.verticalAlign := "none", "\u2A2F"), <.span(^.display := "inline-block", s"${formatExposureTime(e)}")) ::: seconds).toTagMod
        case (_, Some(e))                    => ((s"${formatExposureTime(e)}": VdomNode) :: seconds).toTagMod
        case _                               => EmptyVdom
      }

      <.div(
        SeqexecStyles.centeredCell,
        displayedText
      )
    }
    .build

  def apply(p: Props): Unmounted[Props, Unit, Unit] = component(p)
}

/**
 * Component to display the Guiding state of the step
 */
object GuidingCell {
  final case class Props(s: Step)
  private val guidingIcon = IconCrosshairs.copyIcon(color = "green".some, size = Size.Large)
  private val noGuidingIcon = IconBan.copyIcon(size = Size.Large)
  private val component = ScalaComponent.builder[Props]("GuidingCell")
    .stateless
    .render_P { p =>
      val guiding: Boolean = telescopeGuidingWithT.exist(_ === Guiding.Guide)(p.s)

      <.div(
        SeqexecStyles.centeredCell,
        guidingIcon.when(guiding),
        noGuidingIcon.unless(guiding)
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
  * Component to display the object type
  */
object ObjectTypeCell {
  private val component = ScalaComponent.builder[Step]("ObjectTypeCell")
    .stateless
    .render_P { p =>
      <.div( // Column object type
        SeqexecStyles.rightCell,
        stepTypeO.getOption(p).map { st =>
          val stepTypeColor = st match {
            case _ if p.status === StepState.Completed => "light gray"
            case StepType.Object                          => "green"
            case StepType.Arc                             => "violet"
            case StepType.Flat                            => "grey"
            case StepType.Bias                            => "teal"
            case StepType.Dark                            => "black"
            case StepType.Calibration                     => "blue"
          }
          Label(Label.Props(st.shows, color = stepTypeColor.some, size = Size.Small))
        }.whenDefined
      )
    }.build

  def apply(i: Step): Unmounted[Step, Unit, Unit] = component(i)
}
