// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.json.instances

import gem.Program
import io.circe.{ Decoder, Encoder }
import io.circe.generic.semiauto._

trait ProgramJson {
  import index._
  import observation._
  import programid._
  import sortedmap._

  implicit val ProgramEncoder: Encoder[Program] = deriveEncoder
  implicit val ProgramDecoder: Decoder[Program] = deriveDecoder

}
object program extends ProgramJson