// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model.config

import scala.concurrent.duration.FiniteDuration

import cats.Eq
import org.http4s.Uri

/**
  * Configuration for the general authentication service
  * @param devMode Indicates if we are in development mode, In this mode there is an internal list of users
  * @param sessionLifeHrs How long will the session live in hours
  * @param cookieName Name of the cookie to store the token
  * @param secretKey Secret key to encrypt jwt tokens
  * @param useSSL Whether we use SSL setting the cookie to be https only
  * @param ldap URL of the ldap servers
  */
final case class AuthenticationConfig(
  sessionLifeHrs: FiniteDuration,
  cookieName:     String,
  secretKey:      String,
  useSSL:         Boolean = false,
  ldapURLs:       List[Uri]
)

object AuthenticationConfig {
  implicit val eqAuthenticationConfig: Eq[AuthenticationConfig] =
    Eq.by(
      x =>
        (x.sessionLifeHrs.toNanos,
         x.cookieName,
         x.secretKey,
         x.useSSL,
         x.ldapURLs)
    )

}
