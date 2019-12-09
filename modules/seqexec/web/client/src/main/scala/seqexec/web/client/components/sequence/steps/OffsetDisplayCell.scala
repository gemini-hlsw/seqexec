// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components.sequence.steps

import cats.implicits._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.Reusability
import gsp.math.Axis
import react.common._
import react.common.implicits._
import seqexec.model.{NodAndShuffleStep, OffsetType, StandardStep, Step}
import seqexec.web.client.model.StepItems._
import seqexec.web.client.model.Formatting._
import seqexec.web.client.components.SeqexecStyles
import seqexec.web.client.semanticui.elements.icon.Icon.{IconBan, IconCrosshairs}
import seqexec.web.client.semanticui.Size
import seqexec.web.client.reusability._

/**
  * Component to display the offsets
  */
final case class OffsetsDisplayCell(
  offsetsDisplay: OffsetsDisplay,
  step: Step
) extends ReactProps {
  @inline def render: VdomElement = OffsetsDisplayCell.component(this)
}

object OffsetsDisplayCell {
  type Props = OffsetsDisplayCell

  implicit val doubleReuse: Reusability[Double] = Reusability.double(0.0001)
  implicit val ofdReuse: Reusability[OffsetsDisplay] = Reusability.derive[OffsetsDisplay]
  implicit val propsReuse: Reusability[Props] =
    Reusability.by(p => (p.offsetsDisplay, p.step.config))

  private val guidingIcon = IconCrosshairs.copyIcon(color = "green".some, size = Size.Large)
  private val noGuidingIcon = IconBan.copyIcon(size = Size.Large)

  private def standardOffsetsRender(step: StandardStep, offsetWidth: Double, axisLabelWidth: Double): VdomElement = {
    val offsetP = step.offset[OffsetType.Telescope, Axis.P]
    val offsetQ = step.offset[OffsetType.Telescope, Axis.Q]

    <.div(
      SeqexecStyles.offsetsBlock,
      <.div(
        <.div(
          SeqexecStyles.offsetComponent,
          <.div(
            ^.width := axisLabelWidth.px,
            offsetAxis[Axis.P]
          ),
          <.div(
            ^.width := offsetWidth.px,
            offsetAngle(offsetP.toAngle)
          )
        ),
        <.div(
          SeqexecStyles.offsetComponent,
          <.div(
            ^.width := axisLabelWidth.px,
            offsetAxis[Axis.Q]
          ),
          <.div(
            ^.width := offsetWidth.px,
            offsetAngle(offsetQ.toAngle)
          )
        )
      )
    )
  }

  private def nodAndShuffleOffsetsRender(step: NodAndShuffleStep, width: Double, axisLabelWidth: Double, nsNodLabelWidth: Double): VdomElement = {
    val offsetBP = step.offset[OffsetType.NSNodB, Axis.P]
    val offsetBQ = step.offset[OffsetType.NSNodB, Axis.Q]
    val offsetAP = step.offset[OffsetType.NSNodA, Axis.P]
    val offsetAQ = step.offset[OffsetType.NSNodA, Axis.Q]

    <.div(
      SeqexecStyles.offsetsBlock,
      <.div(
          ^.width := nsNodLabelWidth.px,
          SeqexecStyles.offsetsNodLabel,
          offsetNSNod[OffsetType.NSNodB]
      ),
      <.div(
        <.div(
          SeqexecStyles.offsetComponent,
          <.div(
            ^.width := axisLabelWidth.px,
            offsetAxis[Axis.P]
          ),
          <.div(
            ^.width := width.px,
            offsetAngle(offsetBP.toAngle)
          )
        ),
        <.div(
          SeqexecStyles.offsetComponent,
          <.div(
            ^.width := axisLabelWidth.px,
            offsetAxis[Axis.Q]
          ),
          <.div(
            ^.width := width.px,
            offsetAngle(offsetBQ.toAngle)
          )
        )
      ),
      <.div(
        ^.width := nsNodLabelWidth.px,
        SeqexecStyles.offsetsNodLabel,
        offsetNSNod[OffsetType.NSNodA]
      ),
      <.div(
        <.div(
          SeqexecStyles.offsetComponent,
          <.div(
            ^.width := axisLabelWidth.px,
            offsetAxis[Axis.P]
          ),
          <.div(
            ^.width := width.px,
            offsetAngle(offsetAP.toAngle)
          )
        ),
        <.div(
          SeqexecStyles.offsetComponent,
          <.div(
            ^.width := axisLabelWidth.px,
            offsetAxis[Axis.Q]
          ),
          <.div(
            ^.width := width.px,
            offsetAngle(offsetAQ.toAngle)
          )
        )
      )
    )
  }

  protected val component = ScalaComponent.builder[Props]("OffsetsDisplayCell")
    .stateless
    .render_P { p =>
      p.offsetsDisplay match {
        case OffsetsDisplay.DisplayOffsets(offsetWidth, axisLabelWidth, nsNodLabelWidth) =>
          val guiding = p.step.guiding

          <.div(
            SeqexecStyles.guidingCell,
            guidingIcon.when(guiding),
            noGuidingIcon.unless(guiding),
            p.step match {
              case s: StandardStep => standardOffsetsRender(s, offsetWidth, axisLabelWidth)
              case s: NodAndShuffleStep => nodAndShuffleOffsetsRender(s, offsetWidth, axisLabelWidth, nsNodLabelWidth)
            }
          )
        case _ => <.div()
      }
    }
    .configure(Reusability.shouldComponentUpdate)
    .build
}
