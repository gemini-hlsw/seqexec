// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.model

import cats.Eq
import cats.implicits._
import gsp.math.{Angle, Axis}
import seqexec.model.enum.Instrument
import seqexec.model.{NodAndShuffleStep, OffsetFormat, OffsetType, StandardStep, Step}
import seqexec.web.client.model.StepItems._
import web.client.utils._

import scala.math.max

/**
  * Utility methods to format step items
  */
object Formatting {
  // Used to decide if the offsets are displayed
  sealed trait OffsetsDisplay

  object OffsetsDisplay {
    case object NoDisplay extends OffsetsDisplay
    final case class DisplayOffsets(
      offsetsWidth: Double,
      axisLabelWidth: Double,
      nsNodLabelWidth: Double) extends OffsetsDisplay
    implicit val eq: Eq[OffsetsDisplay] =
      Eq.by {
        case NoDisplay                  => None
        case DisplayOffsets(ow, aw, nw) => Some((ow, aw, nw))
      }
  }

  def offsetAxis[A](implicit show: OffsetFormat[A]): String =
    s"${show.format}:"

  def offsetNSNod[T](implicit show: OffsetFormat[T]): String =
    s"${show.format}"

  def offsetAngle(off: Angle): String =
    f" ${Angle.signedArcseconds.get(off).toDouble}%03.2f″"

  def axisLabelWidth[A](implicit show: OffsetFormat[A]): Double =
    tableTextWidth(offsetAxis[A])

  def nsNodLabelWidth[A](implicit show: OffsetFormat[A]): Double =
    tableTextWidth(offsetNSNod[A])

  implicit class OffsetWidthsFnsOps(val steps: List[Step]) extends AnyVal {
    private def maxWidth(angles: List[Angle]): Double =
      angles.map(angle => tableTextWidth(offsetAngle(angle))).maximumOption.orEmpty

    // Calculate the widest offset step, widest axis label and widest NS nod label
    def sequenceOffsetMaxWidth: (Double, Double, Double) =
      steps.collect {
        case s: StandardStep      =>
          (
            maxWidth(List(
              s.offset[OffsetType.Telescope, Axis.P].toAngle,
              s.offset[OffsetType.Telescope, Axis.Q].toAngle
              )),
            max(axisLabelWidth[Axis.P], axisLabelWidth[Axis.Q]),
            0.0
          )
        case s: NodAndShuffleStep =>
          (
            maxWidth(List(
              s.offset[OffsetType.NSNodB, Axis.P].toAngle,
              s.offset[OffsetType.NSNodB, Axis.Q].toAngle,
              s.offset[OffsetType.NSNodA, Axis.P].toAngle,
              s.offset[OffsetType.NSNodA, Axis.Q].toAngle
              )),
            max(axisLabelWidth[Axis.P], axisLabelWidth[Axis.Q]),
            max(nsNodLabelWidth[OffsetType.NSNodB], nsNodLabelWidth[OffsetType.NSNodA])
          )
      }.foldLeft((0.0, 0.0, 0.0)) { case ((ow1, aw1, nw1), (ow2, aw2, nw2)) =>
        ((ow1 max ow2, aw1 max aw2, nw1 max nw2))
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
