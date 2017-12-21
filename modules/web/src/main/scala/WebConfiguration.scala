// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package web

import pdi.jwt.JwtAlgorithm
import pdi.jwt.algorithms.JwtHmacAlgorithm

/**
 * Static configuration for the server. This is used to construct an Environment. It's not yet
 * clear what should be passed in and what should be hard-coded.
 */
final case class WebConfiguration(
  jwt:       WebConfiguration.Jwt,
  webServer: WebConfiguration.WebServer,
  log:       WebConfiguration.Log
)

object WebConfiguration {

  /** A reasonable configuration for testing with a local database. */
  val forTesting: WebConfiguration =
    WebConfiguration(
      jwt = Jwt(
        algorithm  = JwtAlgorithm.HS256,
        cookieName = "gem.jwt",
        ttlSeconds = 60 * 5
      ),
      webServer = WebServer(
        port = 9090,
        host = "0.0.0.0"
      ),
      log = Log(
        name            = "web",
        shutdownTimeout = 1000L
      )
    )

  /** Logger configuration. */
  final case class Log(name: String, shutdownTimeout: Long)

  /** JWT configuration. */
  final case class Jwt(algorithm: JwtHmacAlgorithm, cookieName: String, ttlSeconds: Long)

  /** Web server configuration. */
  final case class WebServer(port: Int, host: String)

}
