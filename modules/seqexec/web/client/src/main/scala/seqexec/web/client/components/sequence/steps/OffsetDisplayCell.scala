// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components.sequence.steps

import cats.Eq
import cats.implicits._
import japgolly.scalajs.react._
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.extra.Reusability
import seqexec.model.enum.Guiding
import seqexec.model.Step
import seqexec.model.{ Offset, OffsetAxis, TelescopeOffset }
import seqexec.web.client.lenses.{telescopeOffsetPO, telescopeOffsetQO}
import seqexec.web.client.lenses._
import seqexec.web.client.components.SeqexecStyles
import seqexec.web.client.semanticui.elements.icon.Icon.{IconBan, IconCrosshairs}
import seqexec.web.client.semanticui.Size
import seqexec.web.client.reusability._
import web.client.utils._
import web.client.style._

/**
  * Utility methods to display offsets and calculate their widths
  */
object OffsetFns {
  // Used to decide if the offsets are displayed
  sealed trait OffsetsDisplay

  object OffsetsDisplay {
    case object NoDisplay extends OffsetsDisplay
    final case class DisplayOffsets(offsetsWidth: Int) extends OffsetsDisplay
    implicit val eq: Eq[OffsetsDisplay] =
      Eq.by {
        case NoDisplay         => None
        case DisplayOffsets(v) => Some(v)
      }
  }

  def offsetAxis(axis: OffsetAxis): String =
    f"${axis.show}:"

  def offsetValueFormat(off: Offset): String =
    f" ${off.value}%03.2f″"

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
    steps.map(s => telescopeOffsetPO.exist(_ =!= TelescopeOffset.P.Zero)(s) || telescopeOffsetQO.exist(_ =!= TelescopeOffset.Q.Zero)(s)).fold(false)(_ || _)
  }

  implicit class OffsetFnsOps(val steps: List[Step]) extends AnyVal {
    def sequenceOffsetWidths: (Int, Int) = sequenceOffsetWidthsF(steps)
    def areNonZeroOffsets: Boolean = areNonZeroOffsetsF(steps)
    // Find out if offsets should be displayed
    def offsetsDisplay: OffsetsDisplay = {
      val (p, q) = steps.sequenceOffsetWidths
      OffsetsDisplay.DisplayOffsets(scala.math.max(p, q))
    }
  }
}

/**
  * Component to display the offsets
  */
object OffsetsDisplayCell {
  import OffsetFns._

  final case class Props(offsetsDisplay: OffsetsDisplay, step: Step)

  implicit val ofdReuse: Reusability[OffsetsDisplay] = Reusability.derive[OffsetsDisplay]
  implicit val propsReuse: Reusability[Props] = Reusability.derive[Props]

  private val guidingIcon = IconCrosshairs.copyIcon(color = "green".some, size = Size.Large)
  private val noGuidingIcon = IconBan.copyIcon(size = Size.Large)

  private val component = ScalaComponent.builder[Props]("OffsetsDisplayCell")
    .stateless
    .render_P { p =>
      p.offsetsDisplay match {
        case OffsetsDisplay.DisplayOffsets(offsetWidth) =>
          val offsetP = telescopeOffsetPO.getOption(p.step).getOrElse(TelescopeOffset.P.Zero)
          val offsetQ = telescopeOffsetQO.getOption(p.step).getOrElse(TelescopeOffset.Q.Zero)
          val guiding = telescopeGuidingWithT.exist(_ === Guiding.Guide)(p.step)

          <.div(
            SeqexecStyles.guidingCell,
            guidingIcon.when(guiding),
            noGuidingIcon.unless(guiding),
            <.div(
              SeqexecStyles.inlineBlock,
              SeqexecStyles.offsetsBlock,
              ^.textAlign := "right",
              <.div(
                <.div(
                  ^.width := pLabelWidth.px,
                  SeqexecStyles.inlineBlock,
                  offsetAxis(OffsetAxis.AxisP)
                ),
                <.div(
                  ^.width := offsetWidth.px,
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
                  ^.width := offsetWidth.px,
                  SeqexecStyles.inlineBlock,
                  offsetValueFormat(offsetQ)
                )
              )
            )
          )
        case _ => <.div()
      }
    }
    .configure(Reusability.shouldComponentUpdate)
    .build

  def apply(p: Props): Unmounted[Props, Unit, Unit] = component(p)
}
