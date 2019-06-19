// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.json.instances

import gem.TargetEnvironment
import io.circe._
import io.circe.{ Decoder, Encoder }
import io.circe.generic.semiauto._

trait TargetEnvironmentJson {
  import asterism._
  import sortedset._
  import target._

  implicit val PhoenixTargetEnvironmentEncoder: Encoder[TargetEnvironment.Phoenix] = deriveEncoder
  implicit val PhoenixTargetEnvironmentDecoder: Decoder[TargetEnvironment.Phoenix] = deriveDecoder

  implicit val MichelleTargetEnvironmentEncoder: Encoder[TargetEnvironment.Michelle] = deriveEncoder
  implicit val MichelleTargetEnvironmentDecoder: Decoder[TargetEnvironment.Michelle] = deriveDecoder

  implicit val GnirsTargetEnvironmentEncoder: Encoder[TargetEnvironment.Gnirs] = deriveEncoder
  implicit val GnirsTargetEnvironmentDecoder: Decoder[TargetEnvironment.Gnirs] = deriveDecoder

  implicit val NiriTargetEnvironmentEncoder: Encoder[TargetEnvironment.Niri] = deriveEncoder
  implicit val NiriTargetEnvironmentDecoder: Decoder[TargetEnvironment.Niri] = deriveDecoder

  implicit val TrecsTargetEnvironmentEncoder: Encoder[TargetEnvironment.Trecs] = deriveEncoder
  implicit val TrecsTargetEnvironmentDecoder: Decoder[TargetEnvironment.Trecs] = deriveDecoder

  implicit val NiciTargetEnvironmentEncoder: Encoder[TargetEnvironment.Nici] = deriveEncoder
  implicit val NiciTargetEnvironmentDecoder: Decoder[TargetEnvironment.Nici] = deriveDecoder

  implicit val NifsTargetEnvironmentEncoder: Encoder[TargetEnvironment.Nifs] = deriveEncoder
  implicit val NifsTargetEnvironmentDecoder: Decoder[TargetEnvironment.Nifs] = deriveDecoder

  implicit val GpiTargetEnvironmentEncoder: Encoder[TargetEnvironment.Gpi] = deriveEncoder
  implicit val GpiTargetEnvironmentDecoder: Decoder[TargetEnvironment.Gpi] = deriveDecoder

  implicit val GsaoiTargetEnvironmentEncoder: Encoder[TargetEnvironment.Gsaoi] = deriveEncoder
  implicit val GsaoiTargetEnvironmentDecoder: Decoder[TargetEnvironment.Gsaoi] = deriveDecoder

  implicit val GmosSTargetEnvironmentEncoder: Encoder[TargetEnvironment.GmosS] = deriveEncoder
  implicit val GmosSTargetEnvironmentDecoder: Decoder[TargetEnvironment.GmosS] = deriveDecoder

  implicit val AcqCamTargetEnvironmentEncoder: Encoder[TargetEnvironment.AcqCam] = deriveEncoder
  implicit val AcqCamTargetEnvironmentDecoder: Decoder[TargetEnvironment.AcqCam] = deriveDecoder

  implicit val GmosNTargetEnvironmentEncoder: Encoder[TargetEnvironment.GmosN] = deriveEncoder
  implicit val GmosNTargetEnvironmentDecoder: Decoder[TargetEnvironment.GmosN] = deriveDecoder

  implicit val BhrosTargetEnvironmentEncoder: Encoder[TargetEnvironment.Bhros] = deriveEncoder
  implicit val BhrosTargetEnvironmentDecoder: Decoder[TargetEnvironment.Bhros] = deriveDecoder

  implicit val VisitorTargetEnvironmentEncoder: Encoder[TargetEnvironment.Visitor] = deriveEncoder
  implicit val VisitorTargetEnvironmentDecoder: Decoder[TargetEnvironment.Visitor] = deriveDecoder

  implicit val Flamingos2TargetEnvironmentEncoder: Encoder[TargetEnvironment.Flamingos2] = deriveEncoder
  implicit val Flamingos2TargetEnvironmentDecoder: Decoder[TargetEnvironment.Flamingos2] = deriveDecoder

  implicit val GhostTargetEnvironmentEncoder: Encoder[TargetEnvironment.Ghost] = deriveEncoder
  implicit val GhostTargetEnvironmentDecoder: Decoder[TargetEnvironment.Ghost] = deriveDecoder

  implicit val TargetEnvironmentEncoder: Encoder[TargetEnvironment] = deriveEncoder
  implicit val TargetEnvironmentDecoder: Decoder[TargetEnvironment] = deriveDecoder

}
object targetenvironment extends TargetEnvironmentJson