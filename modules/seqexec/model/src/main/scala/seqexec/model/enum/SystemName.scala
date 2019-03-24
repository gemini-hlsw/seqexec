// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model.enum

import cats.{ Eq, Show }
import cats.implicits._

sealed abstract class SystemName(val system: String) {

  def withParam(p: String): String =
    s"$system:$p"

}

object SystemName {

  case object Ocs            extends SystemName("ocs")
  case object Observe        extends SystemName("observe")
  case object Instrument     extends SystemName("instrument")
  case object Telescope      extends SystemName("telescope")
  case object Gcal           extends SystemName("gcal")
  case object Calibration    extends SystemName("calibration")
  case object Meta           extends SystemName("meta")
  case object AdaptiveOptics extends SystemName("adaptive optics")

  val all: List[SystemName] =
    List(Ocs, Observe, Instrument, Telescope, Gcal, Calibration, Meta, AdaptiveOptics)

  def fromString(system: String): Option[SystemName] =
    all.find(_.system === system)

  def unsafeFromString(system: String): SystemName =
    fromString(system).getOrElse(sys.error(s"Unknown system name $system"))

  implicit val show: Show[SystemName] =
    Show.show(_.system)

  implicit val equal: Eq[SystemName] =
    Eq.fromUniversalEquals

}