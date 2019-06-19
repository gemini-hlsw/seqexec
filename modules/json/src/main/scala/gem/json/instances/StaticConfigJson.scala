// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.json.instances

import gem.config._
import io.circe.{ Decoder, Encoder }
import io.circe.generic.semiauto._

trait StaticConfigJson {
  import enumerated._
  import offset._

  // GMOS Common

  implicit val GmosShuffleOffsetEncoder: Encoder[GmosConfig.GmosShuffleOffset] = Encoder[Int].contramap(_.detectorRows)
  implicit val GmosShuffleOffsetDecoder: Decoder[GmosConfig.GmosShuffleOffset] = Decoder[Int].map(GmosConfig.GmosShuffleOffset.unsafeFromRowCount)

  implicit val GmosShuffleCyclesEncoder: Encoder[GmosConfig.GmosShuffleCycles] = Encoder[Int].contramap(_.toInt)
  implicit val GmosShuffleCyclesDecoder: Decoder[GmosConfig.GmosShuffleCycles] = Decoder[Int].map(GmosConfig.GmosShuffleCycles.unsafeFromCycleCount)

  implicit val GmosNodAndShuffleEncoder: Encoder[GmosConfig.GmosNodAndShuffle] = deriveEncoder
  implicit val GmosNodAndShuffleDecoder: Decoder[GmosConfig.GmosNodAndShuffle] = deriveDecoder

  implicit val GmosCustomRoiEntryEncoder: Encoder[GmosConfig.GmosCustomRoiEntry] = Encoder[(Short, Short, Short, Short)].contramap(e => (e.xMin, e.yMin, e.xRange, e.yRange))
  implicit val GmosCustomRoiEntryDecoder: Decoder[GmosConfig.GmosCustomRoiEntry] = Decoder[(Short, Short, Short, Short)].map((GmosConfig.GmosCustomRoiEntry.unsafeFromDescription _).tupled)

  implicit val GmosCommonStaticConfigEncoder: Encoder[GmosConfig.GmosCommonStaticConfig] = deriveEncoder
  implicit val GmosCommonStaticConfigDecoder: Decoder[GmosConfig.GmosCommonStaticConfig] = deriveDecoder

  // Individual codecs are necessary for Observation codecs

  implicit val PhoenixStaticConfigEncoder: Encoder[StaticConfig.Phoenix] = deriveEncoder
  implicit val PhoenixStaticConfigDecoder: Decoder[StaticConfig.Phoenix] = deriveDecoder

  implicit val MichelleStaticConfigEncoder: Encoder[StaticConfig.Michelle] = deriveEncoder
  implicit val MichelleStaticConfigDecoder: Decoder[StaticConfig.Michelle] = deriveDecoder

  implicit val GnirsStaticConfigEncoder: Encoder[StaticConfig.Gnirs] = deriveEncoder
  implicit val GnirsStaticConfigDecoder: Decoder[StaticConfig.Gnirs] = deriveDecoder

  implicit val NiriStaticConfigEncoder: Encoder[StaticConfig.Niri] = deriveEncoder
  implicit val NiriStaticConfigDecoder: Decoder[StaticConfig.Niri] = deriveDecoder

  implicit val TrecsStaticConfigEncoder: Encoder[StaticConfig.Trecs] = deriveEncoder
  implicit val TrecsStaticConfigDecoder: Decoder[StaticConfig.Trecs] = deriveDecoder

  implicit val NiciStaticConfigEncoder: Encoder[StaticConfig.Nici] = deriveEncoder
  implicit val NiciStaticConfigDecoder: Decoder[StaticConfig.Nici] = deriveDecoder

  implicit val NifsStaticConfigEncoder: Encoder[StaticConfig.Nifs] = deriveEncoder
  implicit val NifsStaticConfigDecoder: Decoder[StaticConfig.Nifs] = deriveDecoder

  implicit val GpiStaticConfigEncoder: Encoder[StaticConfig.Gpi] = deriveEncoder
  implicit val GpiStaticConfigDecoder: Decoder[StaticConfig.Gpi] = deriveDecoder

  implicit val GsaoiStaticConfigEncoder: Encoder[StaticConfig.Gsaoi] = deriveEncoder
  implicit val GsaoiStaticConfigDecoder: Decoder[StaticConfig.Gsaoi] = deriveDecoder

  implicit val GmosSStaticConfigEncoder: Encoder[StaticConfig.GmosS] = deriveEncoder
  implicit val GmosSStaticConfigDecoder: Decoder[StaticConfig.GmosS] = deriveDecoder

  implicit val AcqCamStaticConfigEncoder: Encoder[StaticConfig.AcqCam] = deriveEncoder
  implicit val AcqCamStaticConfigDecoder: Decoder[StaticConfig.AcqCam] = deriveDecoder

  implicit val GmosNStaticConfigEncoder: Encoder[StaticConfig.GmosN] = deriveEncoder
  implicit val GmosNStaticConfigDecoder: Decoder[StaticConfig.GmosN] = deriveDecoder

  implicit val BhrosStaticConfigEncoder: Encoder[StaticConfig.Bhros] = deriveEncoder
  implicit val BhrosStaticConfigDecoder: Decoder[StaticConfig.Bhros] = deriveDecoder

  implicit val VisitorStaticConfigEncoder: Encoder[StaticConfig.Visitor] = deriveEncoder
  implicit val VisitorStaticConfigDecoder: Decoder[StaticConfig.Visitor] = deriveDecoder

  implicit val Flamingos2StaticConfigEncoder: Encoder[StaticConfig.Flamingos2] = deriveEncoder
  implicit val Flamingos2StaticConfigDecoder: Decoder[StaticConfig.Flamingos2] = deriveDecoder

  implicit val GhostStaticConfigEncoder: Encoder[StaticConfig.Ghost] = deriveEncoder
  implicit val GhostStaticConfigDecoder: Decoder[StaticConfig.Ghost] = deriveDecoder

  // And the ADT itself
  implicit val StaticConfigEncoder: Encoder[StaticConfig] = deriveEncoder
  implicit val StaticConfigDecoder: Decoder[StaticConfig] = deriveDecoder

}
object staticconfig extends StaticConfigJson