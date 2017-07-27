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
final case class Configuration(
  database:  Configuration.Database,
  jwt:       Configuration.Jwt,
  webServer: Configuration.WebServer,
  log:       Configuration.Log
)

object Configuration {

  /** A reasonable configuration for testing with a local database. */
  val forTesting: Configuration =
    Configuration(
      Database("org.postgresql.Driver", "jdbc:postgresql:gem", "postgres", ""),
      Jwt(JwtAlgorithm.HS256, "gem.jwt", 60L),
      WebServer(8080, "localhost"),
      Log("web", 1000L)
    )

  /** Logger configuration. */
  final case class Log(name: String, shutdownTimeout: Long)

  /** Database configuration. */
  final case class Database(driver: String, connectUrl: String, userName: String, password: String)

  /** JWT configuration. */
  final case class Jwt(algorithm: JwtHmacAlgorithm, cookieName: String, ttlSeconds: Long)

  /** Web server configuration. */
  final case class WebServer(port: Int, host: String)

}
