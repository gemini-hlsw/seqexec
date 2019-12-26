// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.server

import cats.implicits._
import cats.effect.Sync
import gem.enum.Site
import org.http4s.Uri
import pureconfig._
import pureconfig.generic.auto._
import pureconfig.error._
import pureconfig.module.catseffect.syntax._
import pureconfig.module.http4s._
import pureconfig.generic.ProductHint
import shapeless.tag
import shapeless.tag.@@
import seqexec.model.config._

package config {
  final case class SiteValueUnknown(site: String) extends FailureReason {
    def description: String = s"site '$site' invalid"
  }
  final case class ModeValueUnknown(mode: String) extends FailureReason {
    def description: String = s"mode '$mode' invalid"
  }
  final case class StrategyValueUnknown(strategy: String) extends FailureReason {
    def description: String = s"strategy '$strategy' invalid"
  }
}

/**
 * Settings and decoders to parse the configuration files
 */
package object config {
  implicit val siteReader = ConfigReader.fromCursor[Site]{ cf =>
    cf.asString.flatMap {
      case "GS" => Site.GS.asRight
      case "GN" => Site.GN.asRight
      case s    => cf.failed(SiteValueUnknown(s))
    }
  }

  implicit val modeReader = ConfigReader.fromCursor[Mode]{ cf =>
    cf.asString.flatMap {
      case "production" => Mode.Production.asRight
      case "dev"        => Mode.Development.asRight
      case s            => cf.failed(ModeValueUnknown(s))
    }
  }

  implicit val controlStrategyReader = ConfigReader.fromCursor[ControlStrategy]{ cf =>
    cf.asString.flatMap { c =>
      ControlStrategy.fromString(c) match {
        case Some(x) => x.asRight
        case _       => cf.failed(StrategyValueUnknown(c))
      }
    }
  }

  implicit def uriSettings[A]: ConfigReader[Uri @@ A] = ConfigReader[Uri].map(tag[A][Uri])

  implicit val tlsInfoHint = ProductHint[TLSConfig](ConfigFieldMapping(KebabCase, KebabCase))
  implicit val webServerConfigurationHint = ProductHint[WebServerConfiguration](ConfigFieldMapping(KebabCase, KebabCase))
  implicit val smartGcalConfigurationHint = ProductHint[SmartGcalConfiguration](ConfigFieldMapping(KebabCase, KebabCase))
  implicit val authenticationConfigHint = ProductHint[AuthenticationConfig](ConfigFieldMapping(KebabCase, KebabCase))
  implicit val seqexecServerHint = ProductHint[SeqexecEngineConfiguration](ConfigFieldMapping(KebabCase, KebabCase))
  implicit val systemsControlHint = ProductHint[SystemsControlConfiguration](ConfigFieldMapping(KebabCase, KebabCase))

  def loadConfiguration[F[_]: Sync](config: ConfigObjectSource): F[SeqexecConfiguration] =
      config.loadF[F, SeqexecConfiguration]

}
