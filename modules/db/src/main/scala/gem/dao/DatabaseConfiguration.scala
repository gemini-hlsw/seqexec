// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.dao

import cats.effect.Async
import doobie._

/** Configuration for a database connection. */
final case class DatabaseConfiguration(
  driver:     String,
  connectUrl: String,
  userName:   String,
  password:   String
) {

  /** A transactor in F using the connection values provided by this configuration. */
  def transactor[F[_]: Async]: Transactor[F] =
    Transactor.fromDriverManager[F](driver, connectUrl, userName, password)

}

object DatabaseConfiguration {

  /** A configuration for local testing. */
  val forTesting: DatabaseConfiguration =
    apply("org.postgresql.Driver", "jdbc:postgresql:gem", "postgres", "")

}