// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.json.instances

import gem.json.syntax.prism._
import gem.math.Wavelength
import io.circe.{ Decoder, Encoder }

trait WavelengthJson {

  implicit val WavelengthEncoder: Encoder[Wavelength] = Wavelength.fromAngstroms.toEncoder
  implicit val WavelengthDecoder: Decoder[Wavelength] = Wavelength.fromAngstroms.toDecoder

}
object wavelength extends WavelengthJson