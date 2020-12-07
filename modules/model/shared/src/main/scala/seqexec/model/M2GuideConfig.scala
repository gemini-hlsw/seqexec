// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model

import cats._
import cats.syntax.all._
import seqexec.model.enum.ComaOption
import seqexec.model.enum.TipTiltSource

/** Data type for M2 guide config. */
sealed trait M2GuideConfig extends Product with Serializable

object M2GuideConfig {
  case object M2GuideOff extends M2GuideConfig
  final case class M2GuideOn(coma: ComaOption, source: Set[TipTiltSource])
      extends M2GuideConfig

  object M2GuideOn {
    implicit val eq: Eq[M2GuideOn] = Eq.by(x => (x.coma, x.source))
  }

  implicit val show: Show[M2GuideConfig] = Show.fromToString
  implicit val eq: Eq[M2GuideConfig] = Eq.instance {
    case (M2GuideOff, M2GuideOff)                   => true
    case (a @ M2GuideOn(_, _), b @ M2GuideOn(_, _)) => a === b
    case _                                          => false
  }
}
