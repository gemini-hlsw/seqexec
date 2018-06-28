// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.json.instances

import gem.Observation
import io.circe.{ Decoder, Encoder }
import io.circe.generic.semiauto._

@SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
trait ObservationJson {
  import targetenvironment._
  import staticconfig._
  import step._

  implicit val ObservationEncoder: Encoder[Observation] = deriveEncoder
  implicit val ObservationDecoder: Decoder[Observation] = deriveDecoder

}
object observation extends ObservationJson