// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.web.client.components.sequence.steps

import edu.gemini.seqexec.model.Model.{Offset, OffsetAxis, Step, TelescopeOffset}
import edu.gemini.seqexec.web.client.lenses.{telescopeOffsetPO, telescopeOffsetQO}
import edu.gemini.seqexec.web.client.lenses._
import edu.gemini.seqexec.web.client.components.SeqexecStyles
import edu.gemini.web.client.utils._
import japgolly.scalajs.react.ScalazReact._
import japgolly.scalajs.react._
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.vdom.html_<^._
import scalacss.ScalaCssReact._
import org.scalajs.dom.CanvasRenderingContext2D
import org.scalajs.dom.html.Canvas

import scalaz.Equal
import scalaz.syntax.order._
import scalaz.syntax.show._
import scalaz.syntax.std.boolean._
import scalaz.std.anyVal._

/**
  * Utility methods to display offsets and calculate their widths
  */
object OffsetFns {
  // Used to decide if the offsets are displayed
  sealed trait OffsetsDisplay

  object OffsetsDisplay {
    case object NoDisplay extends OffsetsDisplay
    final case class DisplayOffsets(offsetsWidth: Int) extends OffsetsDisplay
    implicit val eq: Equal[OffsetsDisplay] = Equal.equalA
  }

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

  val offsetPText: Step => String = offsetText(OffsetAxis.AxisP) _
  val offsetQText: Step => String = offsetText(OffsetAxis.AxisQ) _

  val pLabelWidth: Int = tableTextWidth(offsetAxis(OffsetAxis.AxisP))
  val qLabelWidth: Int = tableTextWidth(offsetAxis(OffsetAxis.AxisQ))

  // Calculate the widest offset step
  private def sequenceOffsetWidthsF(steps: List[Step]): (Int, Int) =
    steps.map(s => (tableTextWidth(offsetPText(s)), tableTextWidth(offsetQText(s)))).foldLeft((0, 0)) {
      case ((p1, q1), (p2, q2)) => (p1.max(p2), q1.max(q2))
    }

  // Calculate if there are non-zero offsets
  private def areNonZeroOffsetsF(steps: List[Step]): Boolean = {
    steps.map(s => telescopeOffsetPO.exist(_ =/= TelescopeOffset.P.Zero)(s) || telescopeOffsetQO.exist(_ =/= TelescopeOffset.Q.Zero)(s)).fold(false)(_ || _)
  }

  implicit class OffsetFnsOps(val steps: List[Step]) extends AnyVal {
    def sequenceOffsetWidths: (Int, Int) = sequenceOffsetWidthsF(steps)
    def areNonZeroOffsets: Boolean = areNonZeroOffsetsF(steps)
    // Find out if offsets should be displayed
    def offsetsDisplay: OffsetsDisplay = steps.areNonZeroOffsets.fold( {
      val (p, q) = steps.sequenceOffsetWidths
      OffsetsDisplay.DisplayOffsets(scala.math.max(p, q))
    }, OffsetsDisplay.NoDisplay)
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
  * Component to display the offsets
  */
object OffsetsDisplayCell {
  import OffsetFns._

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
  import OffsetFns._

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
