// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.tcs

import cats.effect.Sync
import cats.effect.concurrent.Ref
import cats.implicits._
import mouse.boolean._
import seqexec.server.altair.AltairController._
import seqexec.server.gems.GemsController.{GemsConfig, GemsOff}
import TcsController.{ComaOption, M1GuideConfig, M1GuideOff, M1GuideOn, M1Source, M2GuideConfig, M2GuideOff, M2GuideOn, MountGuideOption, TipTiltSource, GuideConfig => TcsGuideConfig}
import io.circe.{Decoder, DecodingFailure}
import seqexec.server.tcs.TcsController.ComaOption.ComaOn
import seqexec.server.tcs.TcsController.MountGuideOption.{MountGuideOff, MountGuideOn}

trait GuideConfigDb[F[_]] {
  import GuideConfigDb._

  def value: F[GuideConfig]

  def set(v: GuideConfig): F[Unit]

}

object GuideConfigDb {

  final case class GuideConfig(tcsGuide: TcsGuideConfig,
                              gaosGuide: Option[Either[AltairConfig, GemsConfig]])

  val defaultGuideConfig = GuideConfig(TcsGuideConfig(MountGuideOff, M1GuideOff, M2GuideOff), None)

  def newDb[F[_]: Sync]: F[GuideConfigDb[F]] = Ref.of[F, GuideConfig](defaultGuideConfig).map{ref =>
    new GuideConfigDb[F] {
      override def value: F[GuideConfig] = ref.get

      override def set(v: GuideConfig): F[Unit] = ref.set(v)
    }
  }

  implicit val altairDecoder: Decoder[AltairConfig] = Decoder.instance[AltairConfig]{
    c =>
      c.downField("aoOn").as[Boolean].flatMap{
        if(_) {
          c.downField("mode").as[String].flatMap{
            case "NGS" => Right(Ngs)
            case "LGS" => c.downField("useP1").as[Boolean].flatMap{
              if(_) Right(LgsWithP1)
              else {
                c.downField("useOI").as[Boolean].flatMap{
                  if(_) c.downField("oiBlend").as[Boolean].map(LgsWithOi)
                  else for {
                    strapLoop <- c.downField("strapOn").as[Boolean]
                    sfoLoop   <- c.downField("sfoOn").as[Boolean]
                  } yield Lgs(strapLoop, sfoLoop)
                }
              }
            }
            case _     => Left(DecodingFailure("AltairConfig", c.history))
          }
        }
        else Right(AltairOff)
      }
  }

  // TODO Implement GeMS decoder
  implicit val gemsDecoder: Decoder[GemsConfig] = Decoder.const(GemsOff)

  implicit val gaosEitherDecoder: Decoder[Either[AltairConfig, GemsConfig]] =
    Decoder.decodeEither("altair", "gems")

  implicit val mountGuideDecoder: Decoder[MountGuideOption] =
    Decoder.decodeBoolean.map(_.fold(MountGuideOn, MountGuideOff))

  implicit val m1GuideSourceDecoder: Decoder[M1Source] = Decoder.decodeString.flatMap{
    case "PWFS1" => Decoder.const(M1Source.PWFS1)
    case "PWFS2" => Decoder.const(M1Source.PWFS2)
    case "OIWFS" => Decoder.const(M1Source.OIWFS)
    case "HRWFS" => Decoder.const(M1Source.HRWFS)
    case "GAOS"  => Decoder.const(M1Source.GAOS)
    case _       => Decoder.failedWithMessage("M1Source")
  }

  implicit val m1GuideDecoder: Decoder[M1GuideConfig] = Decoder.instance[M1GuideConfig]{ c =>
    c.downField("on").as[Boolean].flatMap {
      if (_) {
        c.downField("source").as[M1Source].map(M1GuideOn)
      }
      else Right(M1GuideOff)
    }
  }

  implicit val m2GuideSourceDecoder: Decoder[TipTiltSource] = Decoder.decodeString.flatMap{
    case "PWFS1" => Decoder.const(TipTiltSource.PWFS1)
    case "PWFS2" => Decoder.const(TipTiltSource.PWFS2)
    case "OIWFS" => Decoder.const(TipTiltSource.OIWFS)
    case "GAOS"  => Decoder.const(TipTiltSource.GAOS)
    case _       => Decoder.failedWithMessage("TipTiltSource")
  }

  implicit val comaDecoder: Decoder[ComaOption] = Decoder.decodeBoolean.map(_.fold(ComaOn, ComaOn))

  implicit val m2GuideDecoder: Decoder[M2GuideConfig] = Decoder.instance[M2GuideConfig]{ c =>
    c.downField("on").as[Boolean].flatMap{
      if(_) for {
        srcs <- c.downField("sources").as[Set[TipTiltSource]]
        coma <- c.downField("comaOn").as[ComaOption]
      } yield M2GuideOn(coma, srcs)
      else Right(M2GuideOff)
    }
  }

  implicit val tcsGuideConfigDecoder: Decoder[TcsGuideConfig] =
    Decoder.forProduct3("mountGuideOn", "m1Guide", "m2Guide")(TcsGuideConfig)

  implicit val guideConfigDecoder: Decoder[GuideConfig] =
    Decoder.forProduct2("tcsGuide", "gaosGuide")(GuideConfig)

}