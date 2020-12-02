// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model.config

import java.nio.file.Path

import cats.Eq

/**
  * Configuration for the TLS server
  * @param keyStore Location where to find the keystore
  * @param keyStorePwd Password for the keystore
  * @param certPwd Password for the certificate used for TLS
  */
final case class TLSConfig(keyStore: Path, keyStorePwd: String, certPwd: String)

object TLSConfig {

  implicit val sslConfigEq: Eq[TLSConfig] =
    Eq.by(x => (x.keyStore, x.keyStorePwd, x.certPwd))
}

/**
  * Configuration for the web server side of the seqexec
  * @param host Host name to listen, typicall 0.0.0.0
  * @param port Port to listen for web requestes
  * @param insecurePort Port where we setup a redirect server to send to https
  * @param externalBaseUrl Redirects need an external facing name
  * @param tls Configuration of TLS, optional
  */
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
