// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import cats.Eq
import cats.implicits._
import edu.gemini.seqexec.server.tcs.{BinaryEnabledDisabled, BinaryOnOff, BinaryYesNo}
import monocle.Iso
import shapeless.tag
import shapeless.tag.@@
import squants.{Angle, Length, Ratio}
import squants.space.{Arcseconds, Millimeters}

import java.util.concurrent.TimeUnit.SECONDS

import scala.concurrent.duration.FiniteDuration

package tcs {

  final class FocalPlaneScale(angle: Angle, length: Length) extends Ratio[Angle, Length] {

    override def base: Angle = angle

    override def counter: Length = length

    def times(l: Length): Angle = convertToBase(l)
    def *(l: Length): Angle = times(l)

    def divide(a: Angle): Length = convertToCounter(a)
  }

  object FocalPlaneScale {

    implicit class AngleOps(a: Angle) {
      def dividedBy(fps: FocalPlaneScale): Length = fps.divide(a)
      def /(fps: FocalPlaneScale): Length = dividedBy(fps)
    }

    implicit class LengthOps(l: Length) {
      def times(fps: FocalPlaneScale): Angle = fps * l
      def *(fps: FocalPlaneScale): Angle = times(fps)
    }

  }

}

package object tcs {
  val BottomPort: Int = 1
  val InvalidPort: Int = 0

  val tcsTimeout : FiniteDuration = FiniteDuration(60, SECONDS)
  val agTimeout : FiniteDuration = FiniteDuration(60, SECONDS)

  val NonStopExposures = -1

  // Focal plane scale, expressed with squants quantities.
  val FOCAL_PLANE_SCALE = new FocalPlaneScale(Arcseconds(1.61144), Millimeters(1))

  val pwfs1OffsetThreshold: Length = Arcseconds(0.01)/FOCAL_PLANE_SCALE
  val pwfs2OffsetThreshold: Length = Arcseconds(0.01)/FOCAL_PLANE_SCALE

  val AoOffsetThreshold: Length = Arcseconds(0.01)/FOCAL_PLANE_SCALE

  implicit val ooEq: Eq[BinaryOnOff] =
    Eq[Int].contramap(_.ordinal())
  implicit val ynEq: Eq[BinaryYesNo] =
    Eq[Int].contramap(_.ordinal())
  implicit val endisEq: Eq[BinaryEnabledDisabled] =
    Eq[Int].contramap(_.ordinal())

  def tagIso[B, T]: Iso[B@@T, B] = Iso.apply[B@@T, B](x => x)(tag[T](_))

}
