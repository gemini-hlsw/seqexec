// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import cats.Eq
import cats.implicits._
import edu.gemini.seqexec.server.tcs.{BinaryOnOff, BinaryYesNo}
import squants.{Angle, Length, Ratio}
import squants.space.{Arcseconds, Millimeters}

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

  // Is there a way to express this value with squants quantities ?
  val FOCAL_PLANE_SCALE = new FocalPlaneScale(Arcseconds(1.61144), Millimeters(1))

  implicit val ooEq: Eq[BinaryOnOff] =
    Eq[Int].contramap(_.ordinal())
  implicit val ynEq: Eq[BinaryYesNo] =
    Eq[Int].contramap(_.ordinal())
}
