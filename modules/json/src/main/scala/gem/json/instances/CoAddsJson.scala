// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.json.instances

import gem.CoAdds
import gem.json.syntax.prism._
import io.circe.{ Decoder, Encoder }

trait CoAddsJson {

  implicit val CoAddsEncoder: Encoder[CoAdds] = CoAdds.fromShort.toEncoder
  implicit val CoAddsDecoder: Decoder[CoAdds] = CoAdds.fromShort.toDecoder

}
object coadds extends CoAddsJson