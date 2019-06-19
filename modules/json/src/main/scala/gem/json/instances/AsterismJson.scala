// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.json.instances

import gem.Asterism
import io.circe.{ Decoder, Encoder }
import io.circe.generic.semiauto._

trait AsterismJson {
  import target._

  implicit val PhoenixAsterismEncoder: Encoder[Asterism.Phoenix] = deriveEncoder
  implicit val PhoenixAsterismDecoder: Decoder[Asterism.Phoenix] = deriveDecoder

  implicit val MichelleAsterismEncoder: Encoder[Asterism.Michelle] = deriveEncoder
  implicit val MichelleAsterismDecoder: Decoder[Asterism.Michelle] = deriveDecoder

  implicit val GnirsAsterismEncoder: Encoder[Asterism.Gnirs] = deriveEncoder
  implicit val GnirsAsterismDecoder: Decoder[Asterism.Gnirs] = deriveDecoder

  implicit val NiriAsterismEncoder: Encoder[Asterism.Niri] = deriveEncoder
  implicit val NiriAsterismDecoder: Decoder[Asterism.Niri] = deriveDecoder

  implicit val TrecsAsterismEncoder: Encoder[Asterism.Trecs] = deriveEncoder
  implicit val TrecsAsterismDecoder: Decoder[Asterism.Trecs] = deriveDecoder

  implicit val NiciAsterismEncoder: Encoder[Asterism.Nici] = deriveEncoder
  implicit val NiciAsterismDecoder: Decoder[Asterism.Nici] = deriveDecoder

  implicit val NifsAsterismEncoder: Encoder[Asterism.Nifs] = deriveEncoder
  implicit val NifsAsterismDecoder: Decoder[Asterism.Nifs] = deriveDecoder

  implicit val GpiAsterismEncoder: Encoder[Asterism.Gpi] = deriveEncoder
  implicit val GpiAsterismDecoder: Decoder[Asterism.Gpi] = deriveDecoder

  implicit val GsaoiAsterismEncoder: Encoder[Asterism.Gsaoi] = deriveEncoder
  implicit val GsaoiAsterismDecoder: Decoder[Asterism.Gsaoi] = deriveDecoder

  implicit val GmosSAsterismEncoder: Encoder[Asterism.GmosS] = deriveEncoder
  implicit val GmosSAsterismDecoder: Decoder[Asterism.GmosS] = deriveDecoder

  implicit val AcqCamAsterismEncoder: Encoder[Asterism.AcqCam] = deriveEncoder
  implicit val AcqCamAsterismDecoder: Decoder[Asterism.AcqCam] = deriveDecoder

  implicit val GmosNAsterismEncoder: Encoder[Asterism.GmosN] = deriveEncoder
  implicit val GmosNAsterismDecoder: Decoder[Asterism.GmosN] = deriveDecoder

  implicit val BhrosAsterismEncoder: Encoder[Asterism.Bhros] = deriveEncoder
  implicit val BhrosAsterismDecoder: Decoder[Asterism.Bhros] = deriveDecoder

  implicit val VisitorAsterismEncoder: Encoder[Asterism.Visitor] = deriveEncoder
  implicit val VisitorAsterismDecoder: Decoder[Asterism.Visitor] = deriveDecoder

  implicit val Flamingos2AsterismEncoder: Encoder[Asterism.Flamingos2] = deriveEncoder
  implicit val Flamingos2AsterismDecoder: Decoder[Asterism.Flamingos2] = deriveDecoder

  implicit val GhostDualTargetAsterismEncoder: Encoder[Asterism.GhostDualTarget] = deriveEncoder
  implicit val GhostDualTargetAsterismDecoder: Decoder[Asterism.GhostDualTarget] = deriveDecoder

  implicit val AsterismEncoder: Encoder[Asterism] = deriveEncoder
  implicit val AsterismDecoder: Decoder[Asterism] = deriveDecoder

}
object asterism extends AsterismJson