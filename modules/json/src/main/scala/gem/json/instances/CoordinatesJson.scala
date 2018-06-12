// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.json.instances

import gem.json.syntax.all._
import gem.math.{ Coordinates, Declination, RightAscension }
import io.circe._
import io.circe.generic.semiauto._

@SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
trait CoordinatesJson {

  implicit val RightAscensionEncoder: Encoder[RightAscension] = RightAscension.fromStringHMS.toEncoder
  implicit val RightAscensionDecoder: Decoder[RightAscension] = RightAscension.fromStringHMS.toDecoder

  implicit val DeclinationEncoder: Encoder[Declination] = Declination.fromStringSignedDMS.toEncoder
  implicit val DeclinationDecoder: Decoder[Declination] = Declination.fromStringSignedDMS.toDecoder

  implicit val coordinatesEncoder: Encoder[Coordinates] = deriveEncoder
  implicit val coordinatesDecoder: Decoder[Coordinates] = deriveDecoder

}
object coordinates extends CoordinatesJson