// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model.config

import cats.Eq
import cats.implicits._
import java.nio.file.Path

final case class TLSConfig(keyStore: Path, keyStorePwd: String, certPwd: String)

object TLSConfig {

  implicit val sslConfigEq: Eq[TLSConfig] =
    Eq.by(x => (x.keyStore, x.keyStorePwd, x.certPwd))
}

/** Configuration for the web server */
final case class WebServerConfiguration(
  host:            String,
  port:            Int,
  insecurePort:    Int,
  externalBaseUrl: String,
  tls:             Option[TLSConfig]
)

object WebServerConfiguration {
  implicit val webServerConfigurationEq: Eq[WebServerConfiguration] =
    Eq.by(x => (x.host, x.port, x.insecurePort, x.externalBaseUrl, x.tls))
}
