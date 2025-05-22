// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model.config

import cats.Eq
import org.http4s.Uri

import java.nio.file.Path

/**
 * Configuration for Smart Gcal
 * @param smartGCalHost
 *   Host where smartgcal runs
 * @param smartGcalDir
 *   Local directory to store cached files
 */
final case class SmartGcalConfiguration(
  smartGCalHost: Uri,
  smartGCalDir:  Path
)

object SmartGcalConfiguration {
  implicit val eqSmartGcalConfiguration: Eq[SmartGcalConfiguration] =
    Eq.by(x => (x.smartGCalHost, x.smartGCalDir))
}
