// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.json.instances

import gem.json.syntax.all._
import gem.math.{ Angle, Epoch, ProperMotion, RadialVelocity }
import io.circe._
import io.circe.generic.semiauto._

@SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
trait ProperMotionJson {
  import coordinates._
  import offset._

  implicit val EpochEncoder: Encoder[Epoch] = Epoch.fromString.toEncoder
  implicit val EpochDecoder: Decoder[Epoch] = Epoch.fromString.toDecoder

  implicit val RadialVelocityEncoder: Encoder[RadialVelocity] = RadialVelocity.kilometersPerSecond.reverse.toEncoder
  implicit val RadialVelocityDecoder: Decoder[RadialVelocity] = RadialVelocity.kilometersPerSecond.reverse.toDecoder

  // Local angle encoder in mas, wrapped to suppress bogus unused warning
  private object locals {
    implicit val ParallaxEncoder: Encoder[Angle] = Angle.signedMilliarcseconds.reverse.toEncoder
    implicit val ParallaxDecoder: Decoder[Angle] = Angle.signedMilliarcseconds.reverse.toDecoder
  }
  import locals._

  implicit val ProperMotionEncoder: Encoder[ProperMotion] = deriveEncoder
  implicit val ProperMotionDecoder: Decoder[ProperMotion] = deriveDecoder

}
object propermotion extends ProperMotionJson