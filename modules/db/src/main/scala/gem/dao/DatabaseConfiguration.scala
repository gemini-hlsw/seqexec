// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.dao

import cats.effect.{ Async, ContextShift }
import doobie._

/** Configuration for a database connection. */
final case class DatabaseConfiguration(
  driver:     String,
  connectUrl: String,
  userName:   String,
  password:   String
) {

  /** A transactor in F using the connection values provided by this configuration. */
  def transactor[F[_]: Async: ContextShift]: Transactor[F] =
    Transactor.fromDriverManager[F](driver, connectUrl, userName, password)

}

object DatabaseConfiguration {

  /**
   * A configuration for testing. This will use the connect URL from JVM property `ocs3.databaseUrl`
   * when available, otherwise will connect to a local database called `gem`, in both cases under
   * user `postgres`, with no password.
   */
  val forTesting: DatabaseConfiguration = {
    val url = sys.props.getOrElse("ocs3.databaseUrl", "jdbc:postgresql:gem")
    apply("org.postgresql.Driver", url, "postgres", "")
  }

}