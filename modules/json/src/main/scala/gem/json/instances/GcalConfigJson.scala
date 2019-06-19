// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.json.instances

import gem.config.GcalConfig
import io.circe.{ Decoder, Encoder }
import io.circe.generic.semiauto._

trait GcalConfigJson {
  import coadds._
  import enumerated._
  import nonemptyset._
  import time._

  implicit val GcalArcsEncoder: Encoder[GcalConfig.GcalArcs] = deriveEncoder
  implicit val GcalArcsDecoder: Decoder[GcalConfig.GcalArcs] = deriveDecoder

  implicit val GcalLampEncoder: Encoder[GcalConfig.GcalLamp] = deriveEncoder
  implicit val GcalLampDecoder: Decoder[GcalConfig.GcalLamp] = deriveDecoder

  implicit val GcalConfigEncoder: Encoder[GcalConfig] = deriveEncoder
  implicit val GcalConfigDecoder: Decoder[GcalConfig] = deriveDecoder

}
object gcalconfig extends GcalConfigJson