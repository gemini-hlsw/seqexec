// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.web.client.components.sequence.steps

import edu.gemini.seqexec.model.Model.{Offset, OffsetAxis, Step, StepType, TelescopeOffset}
import edu.gemini.seqexec.web.client.components.SeqexecStyles
import edu.gemini.seqexec.web.client.lenses.{stepTypeO, telescopeOffsetPO, telescopeOffsetQO}
import edu.gemini.seqexec.web.client.semanticui.elements.label.Label
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

/**
  * Utility methods to display offsets and calculate their widths
  */
trait OffsetFns {
  def offsetAxis(axis: OffsetAxis): String =
    f"${axis.shows}:"

  def offsetValueFormat(off: Offset): String =
    f" ${off.value}%003.2f″"

  def tableTextWidth(text: String): Int = textWidth(text, "bold 14px sans-serif")

  def offsetText(axis: OffsetAxis)(step: Step): String =
    offsetValueFormat(axis match {
      case OffsetAxis.AxisP => telescopeOffsetPO.getOption(step).getOrElse(TelescopeOffset.P.Zero)
      case OffsetAxis.AxisQ => telescopeOffsetQO.getOption(step).getOrElse(TelescopeOffset.Q.Zero)
    })

  private val offsetPText = offsetText(OffsetAxis.AxisP) _
  private val offsetQText = offsetText(OffsetAxis.AxisQ) _

  val pLabelWidth: Int = tableTextWidth(offsetAxis(OffsetAxis.AxisP))
  val qLabelWidth: Int = tableTextWidth(offsetAxis(OffsetAxis.AxisQ))

  // Calculate the widest offset step
  def sequenceOffsetWidths(steps: List[Step]): (Int, Int) = {
    steps.map(s => (tableTextWidth(offsetPText(s)), tableTextWidth(offsetQText(s)))).foldLeft((0, 0)) {
      case ((p1, q1), (p2, q2)) => (p1.max(p2), q1.max(q2))
    }
  }
}

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
 * Component to display the offset grid and offset values
 */
object OffsetBlock extends OffsetFns {
  final case class Props(s: Step, offsetWidth: Int)
  private val component = ScalaComponent.builder[Props]("OffsetValues")
    .stateless
    .render_P { p =>
      val offsetP = telescopeOffsetPO.getOption(p.s).getOrElse(TelescopeOffset.P.Zero)
      val offsetQ = telescopeOffsetQO.getOption(p.s).getOrElse(TelescopeOffset.Q.Zero)

      <.div(
        <.div(
          SeqexecStyles.inlineBlock,
          OffsetGrid(OffsetGrid.Props(offsetP, offsetQ))
        ),
        <.div(
          SeqexecStyles.inlineBlock,
          <.div(
            ^.cls := "right aligned",
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
            ^.cls := "right aligned",
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
 * Component to display the settings of a given step
 */
object StepSettings extends OffsetFns {
  final case class Props(s: Step, offsetWidth: Int)
  private val component = ScalaComponent.builder[Props]("StepSettings")
    .stateless
    .render_P { p =>
      val stepTypeLabel = stepTypeO.getOption(p.s).map { st =>
        val stepTypeColor = st match {
          case StepType.Object      => "green"
          case StepType.Arc         => "violet"
          case StepType.Flat        => "grey"
          case StepType.Bias        => "teal"
          case StepType.Dark        => "black"
          case StepType.Calibration => "blue"
        }
        Label(Label.Props(st.shows, color = stepTypeColor.some))
      }

      <.div(
        ^.cls := "ui two column grid",
        <.div(
          ^.cls := "row",
          <.div(
            ^.cls := "middle aligned left aligned left floated column",
            OffsetBlock(OffsetBlock.Props(p.s, p.offsetWidth))
          ),
          <.div(
            ^.cls := "middle aligned right floated column",
            <.div(stepTypeLabel.whenDefined)
          )
        )
      )
    }
    .build

  def apply(p: Props): Unmounted[Props, Unit, Unit] = component(p)
}
