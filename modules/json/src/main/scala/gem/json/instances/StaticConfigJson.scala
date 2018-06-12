// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.json.instances

import gem.config._
import io.circe.{ Decoder, Encoder }
import io.circe.generic.semiauto._

@SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
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

  // This is a sanity check. Uncomment these lines if the derivation for DynamicConfig fails and
  // this will provide some clues as to what's missing.
  // List[Any](
  //   deriveEncoder[StaticConfig.Phoenix],    deriveDecoder[StaticConfig.Phoenix],
  //   deriveEncoder[StaticConfig.Michelle],   deriveDecoder[StaticConfig.Michelle],
  //   deriveEncoder[StaticConfig.Gnirs],      deriveDecoder[StaticConfig.Gnirs],
  //   deriveEncoder[StaticConfig.Niri],       deriveDecoder[StaticConfig.Niri],
  //   deriveEncoder[StaticConfig.Trecs],      deriveDecoder[StaticConfig.Trecs],
  //   deriveEncoder[StaticConfig.Nici],       deriveDecoder[StaticConfig.Nici],
  //   deriveEncoder[StaticConfig.Nifs],       deriveDecoder[StaticConfig.Nifs],
  //   deriveEncoder[StaticConfig.Gpi],        deriveDecoder[StaticConfig.Gpi],
  //   deriveEncoder[StaticConfig.Gsaoi],      deriveDecoder[StaticConfig.Gsaoi],
  //   deriveEncoder[StaticConfig.GmosS],      deriveDecoder[StaticConfig.GmosS],
  //   deriveEncoder[StaticConfig.AcqCam],     deriveDecoder[StaticConfig.AcqCam],
  //   deriveEncoder[StaticConfig.GmosN],      deriveDecoder[StaticConfig.GmosN],
  //   deriveEncoder[StaticConfig.Bhros],      deriveDecoder[StaticConfig.Bhros],
  //   deriveEncoder[StaticConfig.Visitor],    deriveDecoder[StaticConfig.Visitor],
  //   deriveEncoder[StaticConfig.Flamingos2], deriveDecoder[StaticConfig.Flamingos2],
  //   deriveEncoder[StaticConfig.Ghost],      deriveDecoder[StaticConfig.Ghost],
  // ).foreach(_ => ()) // ensure it's a unit statement

  implicit val StaticConfigEncoder: Encoder[StaticConfig] = deriveEncoder
  implicit val StaticConfigDecoder: Decoder[StaticConfig] = deriveDecoder

}
object staticconfig extends StaticConfigJson