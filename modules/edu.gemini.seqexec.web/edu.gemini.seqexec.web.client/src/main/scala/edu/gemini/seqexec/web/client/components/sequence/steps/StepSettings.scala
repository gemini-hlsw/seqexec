// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.web.client.components.sequence.steps

import edu.gemini.seqexec.model.Model.{Guiding, OffsetAxis, Step, TelescopeOffset}
import edu.gemini.seqexec.web.client.components.SeqexecStyles
import edu.gemini.seqexec.web.client.components.sequence.steps.OffsetFns._
import edu.gemini.seqexec.web.client.lenses.{observeCoaddsO, observeExposureTimeO, telescopeOffsetPO, telescopeOffsetQO, telescopeGuidingWithT}
import edu.gemini.seqexec.web.client.semanticui.elements.icon.Icon.{IconBan, IconCrosshairs}
import edu.gemini.seqexec.web.client.semanticui.Size
import edu.gemini.web.client.utils._
import japgolly.scalajs.react.ScalazReact._
import japgolly.scalajs.react._
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.CanvasRenderingContext2D
import org.scalajs.dom.html.Canvas

import scalacss.ScalaCssReact._
import scalaz.syntax.order._
import scalaz.syntax.std.option._

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
object OffsetBlock {
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
 * Component to display the exposure time and coadds
 */
object ExposureTime {
  private val component = ScalaComponent.builder[Step]("ExposureTime")
    .stateless
    .render_P { p =>
      val exposureTime = observeExposureTimeO.getOption(p)
      val coadds = observeCoaddsO.getOption(p)

      val displayedText: String = (coadds, exposureTime) match {
        case (Some(c), Some(e)) => s"$c \u2A2F $e [s]"
        case (None, Some(e)) => s"$e [s]"
        case _ => ""
      }

      <.div(
        displayedText
      )
    }
    .build

  def apply(p: Step): Unmounted[Step, Unit, Unit] = component(p)
}

/**
 * Component to display the Guiding state of the step
 */
object GuidingBlock {
  final case class Props(s: Step)
  private val guidingIcon = IconCrosshairs.copyIcon(color = "green".some, size = Size.Big)
  private val noGuidingIcon = IconBan.copyIcon(size = Size.Big)
  private val component = ScalaComponent.builder[Props]("OffsetValues")
    .stateless
    .render_P { p =>
      val guiding: Boolean = telescopeGuidingWithT.exist(_ === Guiding.Guide)(p.s)

      <.div(
        ^.cls := "center aligned",
        guidingIcon.when(guiding),
        noGuidingIcon.unless(guiding)
      )
    }
    .build

  def apply(p: Props): Unmounted[Props, Unit, Unit] = component(p)
}
