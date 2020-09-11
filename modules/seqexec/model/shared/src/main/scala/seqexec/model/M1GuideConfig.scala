// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model

import cats.Eq
import cats.syntax.all._
import seqexec.model.enum.M1Source

/** Data type for M1 guide config. */
sealed trait M1GuideConfig extends Product with Serializable

object M1GuideConfig {
  case object M1GuideOff extends M1GuideConfig
  final case class M1GuideOn(source: M1Source) extends M1GuideConfig

  object M1GuideOn {
    implicit val eq: Eq[M1GuideOn] = Eq.by(_.source)
  }

  implicit val eq: Eq[M1GuideConfig] = Eq.instance {
    case (M1GuideOff, M1GuideOff)     => true
    case (M1GuideOn(a), M1GuideOn(b)) => a === b
    case _                            => false
  }

}
