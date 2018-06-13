// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.json.instances

import gem.json.syntax.splitepi._
import gem.EphemerisKey
import gem.math.{ Ephemeris, EphemerisCoordinates }
import io.circe.{ Decoder, Encoder }
import io.circe.generic.semiauto._

@SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
trait EphemerisJson {
  import coordinates._
  import offset._
  import time._

  implicit val EphemerisCoordinatesEncoder: Encoder[EphemerisCoordinates] = deriveEncoder
  implicit val EphemerisCoordinatesDecoder: Decoder[EphemerisCoordinates] = deriveDecoder

  implicit val EphemerisElementEncoder: Encoder[Ephemeris.Element] = deriveEncoder
  implicit val EphemerisElementDecoder: Decoder[Ephemeris.Element] = deriveDecoder

  implicit val EphemerisEncoder: Encoder[Ephemeris] = Ephemeris.elements.reverse.toEncoder
  implicit val EphemerisDecoder: Decoder[Ephemeris] = Ephemeris.elements.reverse.toDecoder

  implicit val EphemerisKeyEncoder: Encoder[EphemerisKey] = deriveEncoder
  implicit val EphemerisKeyDecoder: Decoder[EphemerisKey] = deriveDecoder

}
object ephemeris extends EphemerisJson