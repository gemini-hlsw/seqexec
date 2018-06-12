// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.json.instances

import gem.{ EphemerisKey, Target }
import gem.enum.Site
import gem.math.{ Ephemeris, ProperMotion }
import io.circe._
import io.circe.{ Decoder, Encoder }
import io.circe.generic.semiauto._

@SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
trait TargetJson {
  import ephemeris._
  import propermotion._
  import enumerated._

  // private locals, wrapped to silence bogus unused warning.
  private object locals {

    implicit val ephMapEnc: Encoder[Map[Site, Ephemeris]] = Encoder[List[(Site, Ephemeris)]].contramap(_.toList)
    implicit val ephMapDec: Decoder[Map[Site, Ephemeris]] = Decoder[List[(Site, Ephemeris)]].map(_.toMap)

    implicit val keyPmDisjEnc: Encoder[Either[EphemerisKey, ProperMotion]] = deriveEncoder
    implicit val keyPmDisjDec: Decoder[Either[EphemerisKey, ProperMotion]] = deriveDecoder

  }
  import locals._

  implicit val TargetEncoder: Encoder[Target] = deriveEncoder
  implicit val TargetDecoder: Decoder[Target] = deriveDecoder

}
object target extends TargetJson

