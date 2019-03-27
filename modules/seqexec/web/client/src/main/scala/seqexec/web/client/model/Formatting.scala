// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.model

import cats.Eq
import cats.implicits._
import seqexec.model.enum.Instrument
import seqexec.model.Offset
import seqexec.model.OffsetAxis
import seqexec.model.Step
import seqexec.web.client.model.StepItems._
import web.client.utils._

/**
  * Utility methods to format step items
  */
object Formatting {
  // Used to decide if the offsets are displayed
  sealed trait OffsetsDisplay

  object OffsetsDisplay {
    case object NoDisplay extends OffsetsDisplay
    final case class DisplayOffsets(offsetsWidth: Double) extends OffsetsDisplay
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

  val pLabelWidth: Double = tableTextWidth(offsetAxis(OffsetAxis.AxisP))
  val qLabelWidth: Double = tableTextWidth(offsetAxis(OffsetAxis.AxisQ))

  implicit class OffsetWidthsFnsOps(val steps: List[Step]) extends AnyVal {
    // Calculate the widest offset step
    def sequenceOffsetWidths: (Double, Double) =
      steps
        .map(s =>
          (tableTextWidth(s.offsetPText), tableTextWidth(s.offsetQText)))
        .foldLeft((0.0, 0.0)) {
          case ((p1, q1), (p2, q2)) => (p1.max(p2), q1.max(q2))
        }
  }

  implicit class ExtraStringOps(val s: String) extends AnyVal {
    def sentenceCase: String =
      (s.toList match {
        case Nil       => Nil
        case x :: rest => x.toUpper :: rest.map(_.toLower)
      }).mkString
  }

  def formatExposureTime(i: Instrument)(e: Double): String = i match {
    case Instrument.GmosN | Instrument.GmosS => f"$e%.0f"
    case _                                   => f"$e%.2f"
  }

  def formatExposure(i: Instrument)(v: Double): String =
    formatExposureTime(i)(v)

}
