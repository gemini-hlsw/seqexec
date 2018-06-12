// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.json.instances

import gem.Step
import gem.config.TelescopeConfig
import io.circe.{ Decoder, Encoder }
import io.circe.generic.semiauto._

@SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
trait StepJson {
  import dynamicconfig._
  import enumerated._
  import offset._
  import gcalconfig._

  implicit val TelescopeConfigEncoder: Encoder[TelescopeConfig] = deriveEncoder
  implicit val TelescopeConfigDecoder: Decoder[TelescopeConfig] = deriveDecoder

  implicit val StepBaseEncoder: Encoder[Step.Base] = deriveEncoder
  implicit val StepBaseDecoder: Decoder[Step.Base] = deriveDecoder

  implicit val StepEncoder: Encoder[Step] = deriveEncoder
  implicit val StepDecoder: Decoder[Step] = deriveDecoder

}
object step extends StepJson