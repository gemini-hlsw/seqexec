// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.duration.SECONDS

import cats.Eq
import shapeless.tag.@@

trait GmosParameters {
  trait NsPairsI
  trait NsRowsI
  trait NsCyclesI
  trait NsExposureDividerI

  type NsPairs           = Int @@ NsPairsI
  type NsRows            = Int @@ NsRowsI
  type NsCycles          = Int @@ NsCyclesI
  type NsExposureDivider = Int @@ NsExposureDividerI

  implicit val nsPairsEq: Eq[NsPairs] = Eq.by(x => x: Int)
  implicit val nsRowsEq: Eq[NsRows] = Eq.by(x => x: Int)
  implicit val nsCyclesEq: Eq[NsCycles] = Eq.by(x => x: Int)

  // Remaining time when it is not safe to stop, pause or abort
  val SafetyCutoff: FiniteDuration = new FiniteDuration(3, SECONDS)

}

object GmosParameters extends GmosParameters
