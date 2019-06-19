// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.json.instances

import gem.Step
import gem.config.TelescopeConfig
import io.circe.{ Decoder, Encoder }
import io.circe.generic.semiauto._

trait StepJson {
  import dynamicconfig._
  import enumerated._
  import offset._
  import gcalconfig._

  implicit val TelescopeConfigEncoder: Encoder[TelescopeConfig] = deriveEncoder
  implicit val TelescopeConfigDecoder: Decoder[TelescopeConfig] = deriveDecoder

  implicit val StepBaseEncoder: Encoder[Step.Base] = deriveEncoder
  implicit val StepBaseDecoder: Decoder[Step.Base] = deriveDecoder

  // Individual codecs are necessary for Observation codec

  implicit val PhoenixStepEncoder: Encoder[Step.Phoenix] = deriveEncoder
  implicit val PhoenixStepDecoder: Decoder[Step.Phoenix] = deriveDecoder

  implicit val MichelleStepEncoder: Encoder[Step.Michelle] = deriveEncoder
  implicit val MichelleStepDecoder: Decoder[Step.Michelle] = deriveDecoder

  implicit val GnirsStepEncoder: Encoder[Step.Gnirs] = deriveEncoder
  implicit val GnirsStepDecoder: Decoder[Step.Gnirs] = deriveDecoder

  implicit val NiriStepEncoder: Encoder[Step.Niri] = deriveEncoder
  implicit val NiriStepDecoder: Decoder[Step.Niri] = deriveDecoder

  implicit val TrecsStepEncoder: Encoder[Step.Trecs] = deriveEncoder
  implicit val TrecsStepDecoder: Decoder[Step.Trecs] = deriveDecoder

  implicit val NiciStepEncoder: Encoder[Step.Nici] = deriveEncoder
  implicit val NiciStepDecoder: Decoder[Step.Nici] = deriveDecoder

  implicit val NifsStepEncoder: Encoder[Step.Nifs] = deriveEncoder
  implicit val NifsStepDecoder: Decoder[Step.Nifs] = deriveDecoder

  implicit val GpiStepEncoder: Encoder[Step.Gpi] = deriveEncoder
  implicit val GpiStepDecoder: Decoder[Step.Gpi] = deriveDecoder

  implicit val GsaoiStepEncoder: Encoder[Step.Gsaoi] = deriveEncoder
  implicit val GsaoiStepDecoder: Decoder[Step.Gsaoi] = deriveDecoder

  implicit val GmosSStepEncoder: Encoder[Step.GmosS] = deriveEncoder
  implicit val GmosSStepDecoder: Decoder[Step.GmosS] = deriveDecoder

  implicit val AcqCamStepEncoder: Encoder[Step.AcqCam] = deriveEncoder
  implicit val AcqCamStepDecoder: Decoder[Step.AcqCam] = deriveDecoder

  implicit val GmosNStepEncoder: Encoder[Step.GmosN] = deriveEncoder
  implicit val GmosNStepDecoder: Decoder[Step.GmosN] = deriveDecoder

  implicit val BhrosStepEncoder: Encoder[Step.Bhros] = deriveEncoder
  implicit val BhrosStepDecoder: Decoder[Step.Bhros] = deriveDecoder

  implicit val VisitorStepEncoder: Encoder[Step.Visitor] = deriveEncoder
  implicit val VisitorStepDecoder: Decoder[Step.Visitor] = deriveDecoder

  implicit val Flamingos2StepEncoder: Encoder[Step.Flamingos2] = deriveEncoder
  implicit val Flamingos2StepDecoder: Decoder[Step.Flamingos2] = deriveDecoder

  implicit val GhostStepEncoder: Encoder[Step.Ghost] = deriveEncoder
  implicit val GhostStepDecoder: Decoder[Step.Ghost] = deriveDecoder

  // And the ADT

  implicit val StepEncoder: Encoder[Step] = deriveEncoder
  implicit val StepDecoder: Decoder[Step] = deriveDecoder

}
object step extends StepJson