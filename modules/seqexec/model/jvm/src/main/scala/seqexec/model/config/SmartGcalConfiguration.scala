// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model.config

import cats.Eq
import cats.implicits._
import java.nio.file.Path

final case class SmartGcalConfiguration(
  smartGCalHost: String,
  smartGCalDir:  Path
)

object SmartGcalConfiguration {
  implicit val eqSmartGcalConfiguration: Eq[SmartGcalConfiguration] =
    Eq.by(x => (x.smartGCalHost, x.smartGCalDir))
}
