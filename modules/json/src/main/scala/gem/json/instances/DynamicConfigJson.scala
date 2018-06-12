// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.json.instances

import gem.config._
import gem.enum._
import io.circe._
import io.circe.generic.semiauto._

@SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
trait DynamicConfigJson {
  import coadds._
  import enumerated._
  import offset._
  import time._
  import wavelength._

  // FLAMINGOS2

  implicit val F2FpuChoiceEncoder: Encoder[F2Config.F2FpuChoice] = deriveEncoder
  implicit val F2FpuChoiceDecoder: Decoder[F2Config.F2FpuChoice] = deriveDecoder

  implicit val Flamingos2Encoder: Encoder[DynamicConfig.Flamingos2] = deriveEncoder
  implicit val Flamingos2Decoder: Decoder[DynamicConfig.Flamingos2] = deriveDecoder

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

  implicit val zzz: Encoder[Either[GnirsFpuOther, GnirsFpuSlit]] = deriveEncoder
  implicit val xxx: Decoder[Either[GnirsFpuOther, GnirsFpuSlit]] = deriveDecoder

  // This is a sanity check. Uncomment these lines if the derivation for DynamicConfig fails and
  // this will provide some clues as to what's missing.
  // List[Any](
  //   deriveEncoder[DynamicConfig.Phoenix],    deriveDecoder[DynamicConfig.Phoenix],
  //   deriveEncoder[DynamicConfig.Michelle],   deriveDecoder[DynamicConfig.Michelle],
  //   deriveEncoder[DynamicConfig.Gnirs],      deriveDecoder[DynamicConfig.Gnirs],
  //   deriveEncoder[DynamicConfig.Niri],       deriveDecoder[DynamicConfig.Niri],
  //   deriveEncoder[DynamicConfig.Trecs],      deriveDecoder[DynamicConfig.Trecs],
  //   deriveEncoder[DynamicConfig.Nici],       deriveDecoder[DynamicConfig.Nici],
  //   deriveEncoder[DynamicConfig.Nifs],       deriveDecoder[DynamicConfig.Nifs],
  //   deriveEncoder[DynamicConfig.Gpi],        deriveDecoder[DynamicConfig.Gpi],
  //   deriveEncoder[DynamicConfig.Gsaoi],      deriveDecoder[DynamicConfig.Gsaoi],
  //   deriveEncoder[DynamicConfig.GmosS],      deriveDecoder[DynamicConfig.GmosS],
  //   deriveEncoder[DynamicConfig.AcqCam],     deriveDecoder[DynamicConfig.AcqCam],
  //   deriveEncoder[DynamicConfig.GmosN],      deriveDecoder[DynamicConfig.GmosN],
  //   deriveEncoder[DynamicConfig.Bhros],      deriveDecoder[DynamicConfig.Bhros],
  //   deriveEncoder[DynamicConfig.Visitor],    deriveDecoder[DynamicConfig.Visitor],
  //   deriveEncoder[DynamicConfig.Flamingos2], deriveDecoder[DynamicConfig.Flamingos2],
  //   deriveEncoder[DynamicConfig.Ghost],      deriveDecoder[DynamicConfig.Ghost],
  // ).foreach(_ => ()) // ensure it's a unit statement

  implicit val DynamicConfigEncoder: Encoder[DynamicConfig] = deriveEncoder
  implicit val DynamicConfigDecoder: Decoder[DynamicConfig] = deriveDecoder

}
object dynamicconfig extends DynamicConfigJson
