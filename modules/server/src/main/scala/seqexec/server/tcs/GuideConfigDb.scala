// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.tcs

import cats.Applicative
import cats.effect.Concurrent
import cats.syntax.all._
import fs2.Stream
import fs2.concurrent.SignallingRef
import io.circe.Decoder
import io.circe.DecodingFailure
import monocle.macros.Lenses
import mouse.boolean._
import seqexec.model.M1GuideConfig
import seqexec.model.M1GuideConfig._
import seqexec.model.M2GuideConfig
import seqexec.model.M2GuideConfig._
import seqexec.model.TelescopeGuideConfig
import seqexec.model.enum.ComaOption
import seqexec.model.enum.ComaOption._
import seqexec.model.enum.M1Source
import seqexec.model.enum.MountGuideOption
import seqexec.model.enum.MountGuideOption._
import seqexec.model.enum.TipTiltSource
import seqexec.server.altair.AltairController._
import seqexec.server.gems.GemsController.Cwfs1Usage
import seqexec.server.gems.GemsController.Cwfs2Usage
import seqexec.server.gems.GemsController.Cwfs3Usage
import seqexec.server.gems.GemsController.GemsConfig
import seqexec.server.gems.GemsController.GemsOff
import seqexec.server.gems.GemsController.GemsOn
import seqexec.server.gems.GemsController.OIUsage
import seqexec.server.gems.GemsController.Odgw1Usage
import seqexec.server.gems.GemsController.Odgw2Usage
import seqexec.server.gems.GemsController.Odgw3Usage
import seqexec.server.gems.GemsController.Odgw4Usage
import seqexec.server.gems.GemsController.P1Usage
import squants.space.Millimeters

@Lenses
final case class GuideConfig(
  tcsGuide:      TelescopeGuideConfig,
  gaosGuide:     Option[Either[AltairConfig, GemsConfig]],
  gemsSkyPaused: Boolean
)

sealed trait GuideConfigDb[F[_]] {
  def value: F[GuideConfig]

  def set(v: GuideConfig): F[Unit]

  def update(f: GuideConfig => GuideConfig): F[Unit]

  def discrete: Stream[F, GuideConfig]
}

object GuideConfigDb {

  val defaultGuideConfig: GuideConfig =
    GuideConfig(TelescopeGuideConfig(MountGuideOff, M1GuideOff, M2GuideOff), None, false)

  def newDb[F[_]: Concurrent]: F[GuideConfigDb[F]] =
    SignallingRef[F, GuideConfig](defaultGuideConfig).map { ref =>
      new GuideConfigDb[F] {
        override def value: F[GuideConfig] = ref.get

        override def set(v: GuideConfig): F[Unit] = ref.set(v)

        override def update(f: GuideConfig => GuideConfig): F[Unit] = ref.update(f)

        override def discrete: Stream[F, GuideConfig] = ref.discrete
      }
    }

  def constant[F[_]: Applicative]: GuideConfigDb[F] =
    new GuideConfigDb[F] {
      override def value: F[GuideConfig] = GuideConfigDb.defaultGuideConfig.pure[F]

      override def set(v: GuideConfig): F[Unit] = Applicative[F].unit

      override def update(f: GuideConfig => GuideConfig): F[Unit] = Applicative[F].unit

      override def discrete: Stream[F, GuideConfig] = Stream.emit(GuideConfigDb.defaultGuideConfig)
    }

  implicit val altairDecoder: Decoder[AltairConfig] = Decoder.instance[AltairConfig] { c =>
    c.downField("aoOn").as[Boolean].flatMap {
      if (_)
        c.downField("mode").as[String].flatMap {
          case "NGS" =>
            for {
              blnd <- c.downField("oiBlend").as[Boolean]
              gsx  <- c.downField("aogsx").as[Double]
              gsy  <- c.downField("aogsy").as[Double]
            } yield Ngs(blnd, (Millimeters(gsx), Millimeters(gsy)))
          case "LGS" =>
            c.downField("useP1").as[Boolean].flatMap {
              if (_) Right(LgsWithP1)
              else
                c.downField("useOI").as[Boolean].flatMap {
                  if (_) Right(LgsWithOi)
                  else
                    for {
                      strapLoop <- c.downField("strapOn").as[Boolean]
                      sfoLoop   <- c.downField("sfoOn").as[Boolean]
                      gsx       <- c.downField("aogsx").as[Double]
                      gsy       <- c.downField("aogsy").as[Double]
                    } yield Lgs(strapLoop, sfoLoop, (Millimeters(gsx), Millimeters(gsy)))
                }
            }
          case _     => Left(DecodingFailure("AltairConfig", c.history))
        }
      else Right(AltairOff)
    }
  }

  implicit val gemsDecoder: Decoder[GemsConfig] = Decoder.instance[GemsConfig] { c =>
    c.downField("aoOn").as[Boolean].flatMap { x =>
      if (x)
        for {
          cwfs1 <- c.downField("ttgs1On").as[Boolean]
          cwfs2 <- c.downField("ttgs2On").as[Boolean]
          cwfs3 <- c.downField("ttgs3On").as[Boolean]
          odgw1 <- c.downField("odgw1On").as[Boolean]
          odgw2 <- c.downField("odgw2On").as[Boolean]
          odgw3 <- c.downField("odgw3On").as[Boolean]
          odgw4 <- c.downField("odgw4On").as[Boolean]
          useP1 <- c.downField("useP1").as[Boolean].recover { case _ => false }
          useOI <- c.downField("useOI").as[Boolean].recover { case _ => false }
        } yield GemsOn(
          Cwfs1Usage.fromBoolean(cwfs1),
          Cwfs2Usage.fromBoolean(cwfs2),
          Cwfs3Usage.fromBoolean(cwfs3),
          Odgw1Usage.fromBoolean(odgw1),
          Odgw2Usage.fromBoolean(odgw2),
          Odgw3Usage.fromBoolean(odgw3),
          Odgw4Usage.fromBoolean(odgw4),
          P1Usage.fromBoolean(useP1),
          OIUsage.fromBoolean(useOI)
        )
      else Right(GemsOff)
    }
  }

  implicit val gaosEitherDecoder: Decoder[Either[AltairConfig, GemsConfig]] =
    Decoder.decodeEither("altair", "gems")

  implicit val mountGuideDecoder: Decoder[MountGuideOption] =
    Decoder.decodeBoolean.map(_.fold(MountGuideOn, MountGuideOff))

  implicit val m1GuideSourceDecoder: Decoder[M1Source] = Decoder.decodeString.flatMap {
    case "PWFS1" => Decoder.const(M1Source.PWFS1)
    case "PWFS2" => Decoder.const(M1Source.PWFS2)
    case "OIWFS" => Decoder.const(M1Source.OIWFS)
    case "HRWFS" => Decoder.const(M1Source.HRWFS)
    case "GAOS"  => Decoder.const(M1Source.GAOS)
    case _       => Decoder.failedWithMessage("M1Source")
  }

  implicit val m1GuideDecoder: Decoder[M1GuideConfig] = Decoder.instance[M1GuideConfig] { c =>
    c.downField("on").as[Boolean].flatMap {
      if (_)
        c.downField("source").as[M1Source].map(M1GuideOn(_))
      else Right(M1GuideOff)
    }
  }

  implicit val m2GuideSourceDecoder: Decoder[TipTiltSource] = Decoder.decodeString.flatMap {
    case "PWFS1" => Decoder.const(TipTiltSource.PWFS1)
    case "PWFS2" => Decoder.const(TipTiltSource.PWFS2)
    case "OIWFS" => Decoder.const(TipTiltSource.OIWFS)
    case "GAOS"  => Decoder.const(TipTiltSource.GAOS)
    case _       => Decoder.failedWithMessage("TipTiltSource")
  }

  implicit val comaDecoder: Decoder[ComaOption] = Decoder.decodeBoolean.map(_.fold(ComaOn, ComaOff))

  implicit val m2GuideDecoder: Decoder[M2GuideConfig] = Decoder.instance[M2GuideConfig] { c =>
    c.downField("on").as[Boolean].flatMap {
      if (_) for {
        srcs <- c.downField("sources").as[Set[TipTiltSource]]
        coma <- c.downField("comaOn").as[ComaOption](comaDecoder)
      } yield M2GuideOn(coma, srcs)
      else Right(M2GuideOff)
    }
  }

  implicit val tcsGuideConfigDecoder: Decoder[TelescopeGuideConfig] =
    Decoder.forProduct3[TelescopeGuideConfig, MountGuideOption, M1GuideConfig, M2GuideConfig](
      "mountGuideOn",
      "m1Guide",
      "m2Guide"
    )(TelescopeGuideConfig(_, _, _))(mountGuideDecoder, m1GuideDecoder, m2GuideDecoder)

  implicit val guideConfigDecoder: Decoder[GuideConfig] =
    Decoder.forProduct2("tcsGuide", "gaosGuide")(GuideConfig(_, _, false))

}
