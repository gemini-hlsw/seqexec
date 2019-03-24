// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.json.instances

import gem.ProgramId
import gem.json.syntax.prism._
import io.circe.{ Decoder, Encoder }

trait ProgramIdJson {

  implicit val ProgramIdEncoder: Encoder[ProgramId] = ProgramId.fromString.toEncoder
  implicit val ProgramIdDecoder: Decoder[ProgramId] = ProgramId.fromString.toDecoder

}
object programid extends ProgramIdJson