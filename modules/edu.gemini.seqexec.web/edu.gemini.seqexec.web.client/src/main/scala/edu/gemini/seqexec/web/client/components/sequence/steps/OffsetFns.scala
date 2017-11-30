// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.web.client.components.sequence.steps

import edu.gemini.seqexec.model.Model.{Offset, OffsetAxis, Step, TelescopeOffset}
import edu.gemini.seqexec.web.client.lenses.{telescopeOffsetPO, telescopeOffsetQO}
import edu.gemini.web.client.utils._
import scalaz.Equal
import scalaz.syntax.show._
import scalaz.syntax.equal._
import scalaz.syntax.std.boolean._

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
