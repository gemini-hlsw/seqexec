// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.json.instances

import gem.config._
import gem.enum._
import io.circe._
import io.circe.generic.semiauto._

trait DynamicConfigJson {
  import coadds._
  import enumerated._
  import time._
  import wavelength._

  // FLAMINGOS2

  implicit val F2FpuChoiceEncoder: Encoder[F2Config.F2FpuChoice] = deriveEncoder
  implicit val F2FpuChoiceDecoder: Decoder[F2Config.F2FpuChoice] = deriveDecoder

  // GMOS Common

  implicit val GmosCcdReadoutEncoder: Encoder[GmosConfig.GmosCcdReadout] = deriveEncoder
  implicit val GmosCcdReadoutDecoder: Decoder[GmosConfig.GmosCcdReadout] = deriveDecoder

  implicit val GmosCommonDynamicConfigEncoder: Encoder[GmosConfig.GmosCommonDynamicConfig] = deriveEncoder
  implicit val GmosCommonDynamicConfigDecoder: Decoder[GmosConfig.GmosCommonDynamicConfig] = deriveDecoder

  implicit val GmosCustomMaskEncoder: Encoder[GmosConfig.GmosCustomMask] = deriveEncoder
  implicit val GmosCustomMaskDecoder: Decoder[GmosConfig.GmosCustomMask] = deriveDecoder

  implicit def gmosGratingEncoder[D: Encoder]: Encoder[GmosConfig.GmosGrating[D]] = deriveEncoder
  implicit def gmosGratingDecoder[D: Decoder]: Decoder[GmosConfig.GmosGrating[D]] = deriveDecoder

  // GMOS North

  implicit val EitherGmosCustomMaskGmosNorthFpuEncoder: Encoder[Either[GmosConfig.GmosCustomMask, GmosNorthFpu]] = deriveEncoder
  implicit val EitherGmosCustomMaskGmosNorthFpuDecoder: Decoder[Either[GmosConfig.GmosCustomMask, GmosNorthFpu]] = deriveDecoder

  // GMOS South

  implicit val EitherGmosCustomMaskGmosSouthFpuEncoder: Encoder[Either[GmosConfig.GmosCustomMask, GmosSouthFpu]] = deriveEncoder
  implicit val EitherGmosCustomMaskGmosSouthFpuDecoder: Decoder[Either[GmosConfig.GmosCustomMask, GmosSouthFpu]] = deriveDecoder

  // GNIRS

  implicit val EitherGnirsFpuOtherGnirsFpuSlitEncoder: Encoder[Either[GnirsFpuOther, GnirsFpuSlit]] = deriveEncoder
  implicit val EitherGnirsFpuOtherGnirsFpuSlitDecoder: Decoder[Either[GnirsFpuOther, GnirsFpuSlit]] = deriveDecoder

  // Individual codecs are necessary for Step codecs
  implicit val PhoenixDynamicConfigEncoder: Encoder[DynamicConfig.Phoenix] = deriveEncoder
  implicit val PhoenixDynamicConfigDecoder: Decoder[DynamicConfig.Phoenix] = deriveDecoder

  implicit val MichelleDynamicConfigEncoder: Encoder[DynamicConfig.Michelle] = deriveEncoder
  implicit val MichelleDynamicConfigDecoder: Decoder[DynamicConfig.Michelle] = deriveDecoder

  implicit val GnirsDynamicConfigEncoder: Encoder[DynamicConfig.Gnirs] = deriveEncoder
  implicit val GnirsDynamicConfigDecoder: Decoder[DynamicConfig.Gnirs] = deriveDecoder

  implicit val NiriDynamicConfigEncoder: Encoder[DynamicConfig.Niri] = deriveEncoder
  implicit val NiriDynamicConfigDecoder: Decoder[DynamicConfig.Niri] = deriveDecoder

  implicit val TrecsDynamicConfigEncoder: Encoder[DynamicConfig.Trecs] = deriveEncoder
  implicit val TrecsDynamicConfigDecoder: Decoder[DynamicConfig.Trecs] = deriveDecoder

  implicit val NiciDynamicConfigEncoder: Encoder[DynamicConfig.Nici] = deriveEncoder
  implicit val NiciDynamicConfigDecoder: Decoder[DynamicConfig.Nici] = deriveDecoder

  implicit val NifsDynamicConfigEncoder: Encoder[DynamicConfig.Nifs] = deriveEncoder
  implicit val NifsDynamicConfigDecoder: Decoder[DynamicConfig.Nifs] = deriveDecoder

  implicit val GpiDynamicConfigEncoder: Encoder[DynamicConfig.Gpi] = deriveEncoder
  implicit val GpiDynamicConfigDecoder: Decoder[DynamicConfig.Gpi] = deriveDecoder

  implicit val GsaoiDynamicConfigEncoder: Encoder[DynamicConfig.Gsaoi] = deriveEncoder
  implicit val GsaoiDynamicConfigDecoder: Decoder[DynamicConfig.Gsaoi] = deriveDecoder

  implicit val GmosSDynamicConfigEncoder: Encoder[DynamicConfig.GmosS] = deriveEncoder
  implicit val GmosSDynamicConfigDecoder: Decoder[DynamicConfig.GmosS] = deriveDecoder

  implicit val AcqCamDynamicConfigEncoder: Encoder[DynamicConfig.AcqCam] = deriveEncoder
  implicit val AcqCamDynamicConfigDecoder: Decoder[DynamicConfig.AcqCam] = deriveDecoder

  implicit val GmosNDynamicConfigEncoder: Encoder[DynamicConfig.GmosN] = deriveEncoder
  implicit val GmosNDynamicConfigDecoder: Decoder[DynamicConfig.GmosN] = deriveDecoder

  implicit val BhrosDynamicConfigEncoder: Encoder[DynamicConfig.Bhros] = deriveEncoder
  implicit val BhrosDynamicConfigDecoder: Decoder[DynamicConfig.Bhros] = deriveDecoder

  implicit val VisitorDynamicConfigEncoder: Encoder[DynamicConfig.Visitor] = deriveEncoder
  implicit val VisitorDynamicConfigDecoder: Decoder[DynamicConfig.Visitor] = deriveDecoder

  implicit val Flamingos2DynamicConfigEncoder: Encoder[DynamicConfig.Flamingos2] = deriveEncoder
  implicit val Flamingos2DynamicConfigDecoder: Decoder[DynamicConfig.Flamingos2] = deriveDecoder

  implicit val GhostDynamicConfigEncoder: Encoder[DynamicConfig.Ghost] = deriveEncoder
  implicit val GhostDynamicConfigDecoder: Decoder[DynamicConfig.Ghost] = deriveDecoder

  // And the DynamicConfig ADT itself

  implicit val DynamicConfigEncoder: Encoder[DynamicConfig] = deriveEncoder
  implicit val DynamicConfigDecoder: Decoder[DynamicConfig] = deriveDecoder

}
object dynamicconfig extends DynamicConfigJson
